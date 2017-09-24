#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    ;; Grpc events
    [grpc-event-type (c:-> grpc-event? grpc-completion-type?)]
    [grpc-event-success (c:-> grpc-event? exact-integer?)]
    [grpc-event-value (c:-> grpc-event? cpointer?)]

    ;; Compeltion queues 
    [_grpc-completion-queue ctype?]
    [grpc-completion-queue-create-for-next (c:-> grpc-completion-queue?)]
    [grpc-completion-queue-next (c:-> grpc-completion-queue? gpr-timespec? grpc-event?)]
    [grpc-completion-queue-shutdown (c:-> grpc-completion-queue? void?)]
    [grpc-completion-queue-destroy (c:-> grpc-completion-queue? void?)]

    ;; Grpc alarms
    [_grpc-alarm ctype?]
    [grpc-alarm-create (c:-> grpc-alarm?)]
    [grpc-alarm-set (c:-> grpc-alarm? grpc-completion-queue? gpr-timespec? evt?)]
    [grpc-alarm-cancel (c:-> grpc-alarm? void?)]
    [grpc-alarm-destroy (c:-> grpc-alarm? void?)]))
  
;; Completion events
(define _grpc-completion-type
  (_enum '(shutdown timeout op-complete)))
(define grpc-completion-type?
  (or/c 'shutdown 'timeout 'op-complete))

(define-cstruct _grpc-event
  ([type _grpc-completion-type]
   [success _int]
   [value _pointer]))

;; Completion queues
(define _grpc-completion-queue _pointer)
(define grpc-completion-queue? cpointer?)

(define grpc-completion-queue-create-for-next/ffi
  (get-ffi-obj "grpc_completion_queue_create_for_next" lib-grpc
               (_fun _pointer -> _grpc-completion-queue)))
(define (grpc-completion-queue-create-for-next)
  (grpc-completion-queue-create-for-next/ffi #f))

(define grpc-completion-queue-next/ffi
  (get-ffi-obj "grpc_completion_queue_next" lib-grpc
    (_fun _grpc-completion-queue _gpr-timespec _pointer -> _grpc-event)))
(define (grpc-completion-queue-next cq deadline)
  (grpc-completion-queue-next/ffi cq deadline #f))

(define grpc-completion-queue-shutdown
  (get-ffi-obj "grpc_completion_queue_shutdown" lib-grpc
    (_fun _grpc-completion-queue -> _void)))

(define grpc-completion-queue-destroy
  (get-ffi-obj "grpc_completion_queue_destroy" lib-grpc
    (_fun _grpc-completion-queue -> _void)))


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
  (define sema (make-semaphore))
  (define b (box #f))
  (define cell (malloc-immobile-cell (vector sema b)))
  (grpc-alarm-set/ffi alarm cq timespec cell #f)
  (wrap-evt
    (semaphore-peek-evt sema)
    (lambda (evt)
      (not (= (unbox b) 0)))))


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




