#lang racket/base

(require
  "completion-queue.rkt"
  "alarm.rkt"
  "timespec.rkt")

(module* main #f
  (define now (gpr-now 'monotonic))

  (define cq (make-grpc-completion-queue))
  (define alarm (grpc-alarm-create cq now))


  (define one-second-from-now
    (let ([timespec (gpr-now 'monotonic)])
      (set-gpr-timespec-seconds! timespec (+ (gpr-timespec-seconds timespec) 1))
      timespec))
  (define alarm2 (grpc-alarm-create cq one-second-from-now))
  (grpc-alarm-cancel alarm2)
  (sync alarm2))
  
