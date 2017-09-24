#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [_gpr-timespec ctype?]
    [_gpr-timespec-pointer ctype?]
    [gpr-timespec? (c:-> any/c boolean?)]
    [gpr-timespec-seconds (c:-> gpr-timespec? exact-integer?)]
    [set-gpr-timespec-seconds! (c:-> gpr-timespec? exact-integer? void?)]
    [gpr-infinite-past (c:-> gpr-clock-type/c gpr-timespec?)]
    [gpr-now (c:-> gpr-clock-type/c gpr-timespec?)]
    [gpr-infinite-future (c:-> gpr-clock-type/c gpr-timespec?)]))
    

(define _gpr-clock-type
  (_enum '(monotonic realtime precise timespan)))
(define gpr-clock-type/c
  (or/c 'monotonic 'realtime 'precise 'timespan))

(define-cstruct _gpr-timespec
  ([seconds _int64]
   [nanoseconds _int]
   [clock-type _gpr-clock-type]))

(define gpr-infinite-past
  (get-ffi-obj "gpr_inf_past" lib-grpc 
    (_fun _gpr-clock-type -> _gpr-timespec)))

(define gpr-now
  (get-ffi-obj "gpr_now" lib-grpc
    (_fun _gpr-clock-type -> _gpr-timespec)))

(define gpr-infinite-future
  (get-ffi-obj "gpr_inf_future" lib-grpc 
    (_fun _gpr-clock-type -> _gpr-timespec)))



