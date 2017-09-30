#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  "completion-queue.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    ;; Grpc alarms
    [_grpc-alarm ctype?]
    [grpc-alarm-create (c:-> grpc-alarm?)]
    [grpc-alarm-set (c:-> grpc-alarm? grpc-completion-queue? gpr-timespec? evt?)]
    [grpc-alarm-cancel (c:-> grpc-alarm? void?)]
    [grpc-alarm-destroy (c:-> grpc-alarm? void?)]))

;; Completion alarm
(define _grpc-alarm _pointer)
(define grpc-alarm? cpointer?)

(define grpc-alarm-create/ffi
  (get-ffi-obj "grpc_alarm_create" lib-grpc
    (_fun _pointer -> _grpc-alarm)))
(define (grpc-alarm-create)
  (grpc-alarm-create/ffi #f))

(define grpc-alarm-set/ffi
  (get-ffi-obj "grpc_alarm_set" lib-grpc
    (_fun _grpc-alarm _grpc-completion-queue _gpr-timespec _pointer _pointer -> _void)))
(define (grpc-alarm-set alarm cq timespec)
  (define-values (tag evt) (make-grpc-completion-queue-tag))
  (grpc-alarm-set/ffi alarm cq timespec tag #f)
  evt)

(define grpc-alarm-cancel/ffi
  (get-ffi-obj "grpc_alarm_cancel" lib-grpc
    (_fun _grpc-alarm _pointer -> _void)))
(define (grpc-alarm-cancel alarm)
  (grpc-alarm-cancel/ffi alarm #f))

(define grpc-alarm-destroy/ffi
  (get-ffi-obj "grpc_alarm_destroy" lib-grpc
    (_fun _grpc-alarm _pointer -> _void)))
(define (grpc-alarm-destroy alarm)
  (grpc-alarm-destroy/ffi alarm #f))




