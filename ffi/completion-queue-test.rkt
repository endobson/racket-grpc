#lang racket/base

(require
  "completion-queue.rkt"
  "alarm.rkt"
  "timespec.rkt"
  "../time/time.rkt")

(module* main #f
  (define now (gpr-now 'monotonic))

  (define cq (make-grpc-completion-queue))
  (define alarm (grpc-alarm-create cq now))
  (sync alarm)


  (define one-second-from-now
    (gpr-timespec-add (gpr-now 'monotonic)
                      (seconds->duration 1)))
  (define alarm2 (grpc-alarm-create cq one-second-from-now))
  (grpc-alarm-cancel alarm2)
  (sync alarm2))
  
