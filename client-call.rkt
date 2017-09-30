#lang racket/base

(require
  "grpc-op-batch.rkt"
  "malloc-util.rkt"
  "ffi/lib.rkt"
  "ffi/timespec.rkt"
  "ffi/call.rkt"
  "ffi/channel.rkt"
  "ffi/completion-queue.rkt"
  "ffi/byte-buffer.rkt"
  (submod "ffi/byte-buffer.rkt" unsafe)
  "ffi/slice.rkt"
  (submod "ffi/slice.rkt" unsafe)
  racket/port
  racket/promise
  racket/match
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))


(provide
  (contract-out
    [make-client-stub
      (c:-> grpc-channel? (and/c immutable? bytes?) grpc-completion-queue? client-stub?)]
    [client-stub-call (c:-> client-stub? bytes? (promise/c bytes?))]))

(struct client-stub (chan method cq))

(define (make-client-stub chan method cq)
  (client-stub chan method cq))

(define (client-stub-call stub request #:deadline [deadline #f])
  (define gpr-deadline (or deadline (gpr-infinite-future 'realtime)))
  (match-define (client-stub chan method cq) stub)
  (define call (grpc-channel-create-call chan #f cq method gpr-deadline))

  (define (send-message)
    (call-with-malloc-grpc-metadata-array
      (λ (recv-metadata)
        (define b (box (make-grpc-byte-buffer request)))
        (sync
          (grpc-call-start-batch call
            (grpc-op-batch
               #:send-initial-metadata 0 #f
               #:send-message (unbox b)
               #:send-close-from-client
               #:recv-initial-metadata recv-metadata)))
        (set-box! b #f))))

  (define (recv-message)
    (define payload-pointer (malloc _pointer 'raw))
    (define trailers-pointer (ptr-ref (malloc _grpc-metadata-array 'raw) _grpc-metadata-array))
    (define status-code-pointer (malloc _int 'raw))
    (define status-details-pointer (ptr-ref (malloc _grpc-slice/ffi 'raw) _grpc-slice/ffi))

    (sync
      (let ([grpc-recv-status
             (make-grpc-recv-status-on-client
                         trailers-pointer
                         status-code-pointer
                         status-details-pointer)])
         (grpc-call-start-batch call
           (grpc-op-batch
             #:recv-message payload-pointer
             #:recv-status-on-client grpc-recv-status))))


    (define status-code (ptr-ref status-code-pointer _int))
    (begin0
      (if (zero? status-code)
          (let ([payload (ptr-ref payload-pointer _pointer)])
            (if payload
                (port->bytes (grpc-byte-buffer->input-port (pointer->grpc-byte-buffer payload)))
                (error 'rpc "No message received")))
          (error 'rpc "Error: ~a ~s" status-code (grpc-slice->bytes status-details-pointer)))
      (free payload-pointer)
      (free trailers-pointer)
      (free status-code-pointer)
      (free status-details-pointer)))

  (delay/thread
    (send-message)
    (recv-message)))
