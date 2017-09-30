#lang racket/base

(require
  "base-lib.rkt"
  "../time/time.rkt"
  ffi/unsafe
  racket/math
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [gpr-timespec? predicate/c]
    [gpr-timespec->time (c:-> gpr-timespec/realtime/c time?)]
    [time->gpr-timespec (c:-> time? gpr-timespec/realtime/c)]
    [gpr-timespec-add (c:-> gpr-timespec? duration? gpr-timespec?)]
    [gpr-infinite-past (c:-> gpr-clock-type/c gpr-timespec?)]
    [gpr-now (c:-> gpr-clock-type/c gpr-timespec?)]
    [gpr-infinite-future (c:-> gpr-clock-type/c gpr-timespec?)]))

(module* unsafe #f
  (provide
    (contract-out
      [_gpr-timespec ctype?])))

(define _gpr-clock-type
  (_enum '(monotonic realtime precise timespan)))
(define gpr-clock-type/c
  (or/c 'monotonic 'realtime 'precise 'timespan))

(define-cstruct _gpr-timespec
  ([seconds _int64]
   [nanoseconds _int]
   [clock-type _gpr-clock-type]))

(define gpr-timespec/realtime/c
  (and/c gpr-timespec? (lambda (t) (eq? (gpr-timespec-clock-type t) 'realtime))))

(define gpr-infinite-past
  (get-ffi-obj "gpr_inf_past" lib-grpc
    (_fun _gpr-clock-type -> _gpr-timespec)))

(define gpr-now
  (get-ffi-obj "gpr_now" lib-grpc
    (_fun _gpr-clock-type -> _gpr-timespec)))

(define gpr-infinite-future
  (get-ffi-obj "gpr_inf_future" lib-grpc
    (_fun _gpr-clock-type -> _gpr-timespec)))

(define gpr-time-add
  (get-ffi-obj "gpr_time_add" lib-grpc
    (_fun _gpr-timespec _gpr-timespec -> _gpr-timespec)))

(define gpr-time-from-nanos
  (get-ffi-obj "gpr_time_from_nanos" lib-grpc
    (_fun _int64 _gpr-clock-type -> _gpr-timespec)))


(define (gpr-timespec->time t)
  (unix-nanos->time (+ (gpr-timespec-seconds t)
                       (* (gpr-timespec-nanoseconds t) 1000000000))))

(define (time->gpr-timespec t)
  (gpr-time-from-nanos (time->unix-nanos t) 'realtime))

(define (duration->gpr-timespec d)
  (gpr-time-from-nanos (duration->nanos d) 'timespan))

(define (gpr-timespec-add t amt)
  (gpr-time-add t (duration->gpr-timespec amt)))
