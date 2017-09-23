#lang racket

(require
  ffi/unsafe
  "ffi/lib.rkt")

(provide start-completion-queue)


(define (start-completion-queue)
  (define cq (grpc-completion-queue-create))
  (define blocking-place
    (place pch
      (define cq (sync pch))
      (let loop ()
        (define result (grpc-completion-queue-next cq gpr-inf-future))
        (case (grpc-event-type result)
          [(0)
           (place-channel-put pch 'shutdown)
           (grpc-completion-queue-shutdown cq)]
          [(1) (loop)]
          [(2)
           (place-channel-put
             pch
             (list (grpc-event-success result)
                   (grpc-event-value result)))
           (loop)]))))
  (place-channel-put blocking-place cq)
  (thread
    (lambda ()
      (let loop ()
        (match (sync blocking-place)
          ['shutdown (void)]
          [(list success pointer)
           (define val (ptr-ref pointer _racket))
           (free-immobile-cell pointer)
           (semaphore-post val)
           (loop)]))))
  cq)
