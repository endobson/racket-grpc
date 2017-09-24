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
    [_grpc-completion-queue ctype?]
    [grpc-completion-queue-create-for-next (c:-> grpc-completion-queue?)]
    [grpc-completion-queue-next (c:-> grpc-completion-queue? gpr-timespec? grpc-event?)]
    [grpc-completion-queue-shutdown (c:-> grpc-completion-queue? void?)]
    [grpc-completion-queue-destroy (c:-> grpc-completion-queue? void?)]
    [grpc-event-type (c:-> grpc-event? grpc-completion-type?)]
    [grpc-event-success (c:-> grpc-event? exact-integer?)]
    [grpc-event-value (c:-> grpc-event? cpointer?)]))
  
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
    (_fun _grpc-completion-queue _gpr-timespec -> _grpc-event)))
(define (grpc-completion-queue-next cq deadline)
  (grpc-completion-queue-next/ffi cq deadline #f))

(define grpc-completion-queue-shutdown
  (get-ffi-obj "grpc_completion_queue_shutdown" lib-grpc
    (_fun _grpc-completion-queue -> _void)))

(define grpc-completion-queue-destroy
  (get-ffi-obj "grpc_completion_queue_destroy" lib-grpc
    (_fun _grpc-completion-queue -> _void)))
