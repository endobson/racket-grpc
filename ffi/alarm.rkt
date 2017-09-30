#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  (submod "timespec.rkt" unsafe)
  "completion-queue.rkt"
  (submod "completion-queue.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/alloc
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    ;; Grpc alarms
    [grpc-alarm-create (c:-> grpc-completion-queue? gpr-timespec? grpc-alarm-evt?)]
    [grpc-alarm-cancel (c:-> grpc-alarm-evt? void?)]))

;; Completion alarm
(define _grpc-alarm _pointer)

;; The event, and a reference to the original alarm to ensure that it isn't
;; destroyed until it fires.
(struct grpc-alarm-evt (evt pointer)
  #:property prop:evt 0)

(define grpc-alarm-destroy/ffi
  (get-ffi-obj "grpc_alarm_destroy" lib-grpc
    (_fun _grpc-alarm _pointer -> _void)))
(define (grpc-alarm-destroy alarm)
  (grpc-alarm-destroy/ffi alarm #f))

(define grpc-alarm-create/ffi
  (get-ffi-obj "grpc_alarm_create" lib-grpc
    (_fun _pointer -> _pointer)))
(define grpc-alarm-alloc
  ((allocator grpc-alarm-destroy)
   (lambda () (grpc-alarm-create/ffi #f))))

(define grpc-alarm-set/ffi
  (get-ffi-obj "grpc_alarm_set" lib-grpc
    (_fun _grpc-alarm _grpc-completion-queue _gpr-timespec _pointer _pointer -> _void)))

(define (grpc-alarm-create cq timespec)
  (define alarm (grpc-alarm-alloc))
  (define-values (tag evt) (make-grpc-completion-queue-tag))
  (grpc-alarm-set/ffi alarm cq timespec tag #f)
  (grpc-alarm-evt evt alarm))

(define grpc-alarm-cancel/ffi
  (get-ffi-obj "grpc_alarm_cancel" lib-grpc
    (_fun _grpc-alarm _pointer -> _void)))
(define (grpc-alarm-cancel alarm)
  (grpc-alarm-cancel/ffi (grpc-alarm-evt-pointer alarm) #f))





