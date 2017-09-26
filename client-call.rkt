#lang racket/base

(require
  "grpc-op-batch.rkt"
  "malloc-util.rkt"
  "ffi/lib.rkt"
  "ffi/timespec.rkt"
  "ffi/call.rkt"
  "ffi/byte-buffer.rkt"
  "ffi/slice.rkt"
  racket/async-channel
  racket/port
  racket/match
  ffi/unsafe)


(provide
  make-client-call
  client-call-send-message
  client-call-recv-message
  client-call-run)

(define (make-client-call chan method cq)
  (define deadline (gpr-now))
  (set-gpr-timespec-seconds! deadline (+ (gpr-timespec-seconds deadline) 1))
  (define host "localhost")

  (define call (grpc-channel-create-call chan cq method host deadline))


  (define cancelled-sema (make-semaphore))
  (define recv-message-channel (make-async-channel))
  (define send-message-channel (make-async-channel))



  (client-call call recv-message-channel))

(struct client-call (call recv-message-channel))


(define-cstruct _recv-status
  ([trailers _grpc-metadata-array]
   [code _int]
   [details _string]
   [details-capacity _size_t]))

(define (client-call-send-message client-call bytes)
  (define call (client-call-call client-call))
  (call-with-malloc-grpc-metadata-array
    (λ (recv-metadata)
      (call-with-malloc-grpc-byte-buffer bytes
        (λ (send-message-buffer)
          (sync
            (grpc-call-start-batch call 
              (grpc-op-batch
                 #:send-initial-metadata 0 #f
                 #:send-message send-message-buffer
                 #:send-close-from-client
                 #:recv-initial-metadata recv-metadata))))))))

(define (client-call-recv-message client-call)
  (sync (client-call-recv-message-channel client-call)))


(define (client-call-run ccall)
  (match-define (client-call call recv-message-channel) ccall)

  (define status-channel (make-async-channel))





    
  (define (handle-recv-status)
    (define recv-status (malloc-struct _recv-status))
    (grpc-metadata-array-init (recv-status-trailers recv-status))
    (set-recv-status-code! recv-status 0)
    (set-recv-status-details! recv-status #f)
    (set-recv-status-details-capacity! recv-status 0)


    (define grpc-recv-status
      (error 'pointer "Not implemented")
      #;
      (make-grpc-recv_status_on_client
        (recv-status-trailers-pointer recv-status)
        (recv-status-code-pointer recv-status)
        (recv-status-details-pointer recv-status)
        (recv-status-details-capacity-pointer recv-status)))


    (sync
      (grpc-call-start-batch call
        (grpc-op-batch #:recv-status-on-client grpc-recv-status)))
    (async-channel-put status-channel
        (list
          (recv-status-code recv-status)
          (recv-status-details recv-status)))
    (free recv-status))




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
        (handle-recv-status))))


  (handle-evt
    status-channel
    (lambda (msg)
      (match-define (list status details) msg)
      (case status
        [(0)
         (grpc-call-unref call)
         (void)]
        [else
          (printf "Error ~a: ~a~n" status details)]))))

