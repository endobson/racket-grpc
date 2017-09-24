#lang racket/base

(require
  "completion-queue.rkt"
  "timespec.rkt"
  )

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
  
