#lang racket/base

(require
  "completion-queue.rkt"
  "timespec.rkt"
  ffi/unsafe
  racket/match
  racket/place)

(define (make-grpc-completion-queue)
  (define cq (grpc-completion-queue-create-for-next))
  (define blocking-place
    (place pch
      (define cq (sync pch))
      (let loop ()
        (define result (grpc-completion-queue-next cq (gpr-infinite-future 'monotonic)))
        (case (grpc-event-type result)
          [(shutdown)
           (place-channel-put pch 'shutdown)
           (grpc-completion-queue-shutdown cq)]
          [(timeout) (loop)]
          [(op-complete)
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
           (match (ptr-ref pointer _racket)
             [(vector sema success-box)
              (free-immobile-cell pointer)
              (set-box! success-box success)
              (semaphore-post sema)])
           (loop)]))))
  cq)


(module* main #f
  (define now (gpr-now 'monotonic))

  (define cq (make-grpc-completion-queue))
  (define alarm (grpc-alarm-create))
  (sync (grpc-alarm-set alarm cq now))
  (grpc-alarm-destroy alarm)


  (define one-second-from-now
    (let ([timespec (gpr-now 'monotonic)])
      (set-gpr-timespec-seconds! timespec (+ (gpr-timespec-seconds timespec) 1))
      timespec))
  (define alarm2 (grpc-alarm-create))
  (define evt (grpc-alarm-set alarm2 cq one-second-from-now))
  (grpc-alarm-cancel alarm2)
  (sync evt)
  (grpc-alarm-destroy alarm2))
  
