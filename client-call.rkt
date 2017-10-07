#lang racket/base

(require
  "ffi/timespec.rkt"
  "ffi/call.rkt"
  "ffi/immobile-pointers.rkt"
  "ffi/channel.rkt"
  "ffi/completion-queue.rkt"
  "ffi/byte-buffer.rkt"
  "ffi/metadata-array.rkt"
  (submod "ffi/metadata-array.rkt" unsafe)
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
    [client-stub-call (c:-> client-stub? bytes? bytes?)]))

(struct client-stub (chan method cq))
(struct client-call (raw promise)
  #:property prop:evt 1)

(define (client-call-force call)
  (force (client-call-promise call)))

(define (make-client-stub chan method cq)
  (client-stub chan method cq))

(define (client-stub-call stub request #:deadline [deadline #f])
  (client-call-force (client-async-stub-call stub request #:deadline deadline)))


(define (client-async-stub-call stub request #:deadline [deadline #f])
  (define gpr-deadline (or deadline (gpr-infinite-future 'realtime)))
  (match-define (client-stub chan method cq) stub)
  (define call (grpc-channel-create-call chan #f cq method gpr-deadline))

  (define (send-message)
    (define recv-metadata (malloc-immobile-grpc-metadata-array))
    (define buffer (make-grpc-byte-buffer request))
    (sync
      (grpc-call-start-batch call cq
        (grpc-op-batch
           #:send-initial-metadata 0 #f
           #:send-message buffer
           #:send-close-from-client
           #:recv-initial-metadata recv-metadata)
        (lambda (success)
          (free-immobile-grpc-metadata-array recv-metadata)))))

  (client-call
    call
    (delay/thread
      (send-message)
      (force (grpc-call-client-receive-unary call cq)))))
