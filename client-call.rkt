#lang racket/base

(require
  "grpc-op-batch.rkt"
  "malloc-util.rkt"
  "ffi/lib.rkt"
  "ffi/timespec.rkt"
  "ffi/call.rkt"
  "ffi/immobile-pointers.rkt"
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
        (define buffer (make-grpc-byte-buffer request))
        (sync
          (grpc-call-start-batch call
            (grpc-op-batch
               #:send-initial-metadata 0 #f
               #:send-message buffer
               #:send-close-from-client
               #:recv-initial-metadata recv-metadata)
            (list buffer recv-metadata))))))

  (define (recv-message)
    (define payload-pointer (malloc _pointer 'atomic-interior))
    (define trailers-pointer (ptr-ref (malloc _grpc-metadata-array 'atomic-interior) _grpc-metadata-array))
    (define status-code-pointer (make-immobile-int))
    (define status-details-pointer (make-immobile-grpc-slice))

    (sync
      (let ([grpc-recv-status
             (make-grpc-recv-status-on-client
               trailers-pointer
               status-code-pointer
               status-details-pointer)])
         (grpc-call-start-batch call
           (grpc-op-batch
             #:recv-message payload-pointer
             #:recv-status-on-client grpc-recv-status)
           (list payload-pointer trailers-pointer status-code-pointer status-details-pointer))))

    (define status-code (immobile-int-ref status-code-pointer))
    (if (zero? status-code)
        (let ([payload (ptr-ref payload-pointer _pointer)])
          (if payload
              (port->bytes (grpc-byte-buffer->input-port (pointer->grpc-byte-buffer payload)))
              (error 'rpc "No message received")))
        (error 'rpc "Error: ~a ~s" status-code (grpc-slice->bytes status-details-pointer))))

  (delay/thread
    (send-message)
    (recv-message)))
