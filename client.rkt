#lang racket/base

(require
  "grpc-op-batch.rkt"
  "malloc-util.rkt"
  "ffi/lib.rkt"
  "ffi/slice.rkt"
  "ffi/timespec.rkt"
  "ffi/call.rkt"
  "ffi/byte-buffer.rkt"
  "time/time.rkt"
  racket/async-channel
  racket/port
  ffi/unsafe
  racket/list)

(provide send-request)


(define-cstruct _recv-status
  ([trailers _grpc-metadata-array]
   [code _int]
   [details _string]
   [details-capacity _size_t]))


(define (send-request cq chan method)
  (define deadline (gpr-timespec-add (gpr-now) (seconds->duration 1)))

  (define call (grpc-channel-create-call chan cq method "localhost" deadline))

  (define recv-metadata (malloc-struct _grpc-metadata-array))
  (grpc-metadata-array-init recv-metadata)

  (define send-message-buffer (make-grpc-byte-buffer #"\x08\x00\x10\x12"))

  (define recv-status (malloc-struct _recv-status))
  (grpc-metadata-array-init (recv-status-trailers recv-status))
  (set-recv-status-code! recv-status 0)
  (set-recv-status-details! recv-status #f)
  (set-recv-status-details-capacity! recv-status 0)

  ;; TODO This needs pointers
  (define grpc-recv-status
    (error 'pointers "Not yet implemented")
    #;
    (make-grpc-recv_status_on_client
      (recv-status-trailers-pointer recv-status)
      (recv-status-code-pointer recv-status)
      (recv-status-details-pointer recv-status)
      (recv-status-details-capacity-pointer recv-status)))

  (sync
    (grpc-call-start-batch call
      (grpc-op-batch
         #:send-initial-metadata 0 #f
         #:send-message send-message-buffer
         #:send-close-from-client
         #:recv-initial-metadata recv-metadata)))

  (define recv-message-channel (make-async-channel))

  (define read-thread
    (thread
      (lambda ()
        (call-with-malloc _pointer
          (λ (payload-pointer)
            (let loop ()
              (sync
                (grpc-call-start-batch call
                  (grpc-op-batch #:recv-message payload-pointer)))
              (define payload (ptr-ref payload-pointer _pointer))
              (when payload
                (async-channel-put
                  recv-message-channel
                  (port->bytes (grpc-byte-buffer->input-port payload)))
                (loop)))))
        (sync
          (grpc-call-start-batch call
            (grpc-op-batch #:recv-status-on-client grpc-recv-status))))))


  (handle-evt
    (thread-dead-evt read-thread)
    (lambda (_)
      (define status (recv-status-code recv-status))
      (case status
        [(0)
         (grpc-call-unref call)
         (void)]
        [else
          (printf "Error ~a: ~a~n" (recv-status-details recv-status))]))))



