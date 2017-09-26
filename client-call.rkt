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
  (define deadline (gpr-now 'monotonic))
  (set-gpr-timespec-seconds! deadline (+ (gpr-timespec-seconds deadline) 1))

  (define call (grpc-channel-create-call chan #f cq method deadline))


  (define cancelled-sema (make-semaphore))
  (define recv-message-channel (make-async-channel))
  (define send-message-channel (make-async-channel))

  (client-call call recv-message-channel))

(struct client-call (call recv-message-channel))

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
    (define trailers-pointer (ptr-ref (malloc _grpc-metadata-array 'raw) _grpc-metadata-array))
    (define status-code-pointer (malloc _int 'raw))
    (define status-details-pointer (ptr-ref (malloc _grpc-slice 'raw) _grpc-slice))

    (grpc-metadata-array-init trailers-pointer)

    (define grpc-recv-status
      (make-grpc-recv-status-on-client
        trailers-pointer
        status-code-pointer
        status-details-pointer))

    (sync
      (grpc-call-start-batch call
        (grpc-op-batch #:recv-status-on-client grpc-recv-status)))
    (async-channel-put status-channel
        (list
          (ptr-ref status-code-pointer _int)
          (grpc-slice->bytes status-details-pointer)))
    (free trailers-pointer)
    (free status-code-pointer)
    (free status-details-pointer))

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

