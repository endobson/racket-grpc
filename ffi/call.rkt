#lang racket/base

(require
  "base-lib.rkt"
  "channel.rkt"
  "completion-queue.rkt"
  "slice.rkt"
  "timespec.rkt"
  ffi/unsafe
  ffi/cvector
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-channel-create-call
      (c:-> grpc-channel? grpc-call? grpc-completion-queue? string?  gpr-timespec?)]
    [grpc-call-start-batch
      (c:-> grpc-call? cvector? evt?)]))



(define _grpc-call _pointer)
(define grpc-call? cpointer?)

(define grpc-channel-create-call/ffi
  (get-ffi-obj "grpc_channel_create_call" lib-grpc
    (_fun _grpc-channel _grpc-call _uint32 _grpc-slice _grpc-slice-pointer _gpr-timespec _pointer -> _grpc-call)))

(define (grpc-channel-create-call channel parent cq method deadline)
  (define method-slice (grpc-slice-from-copied-buffer method))
  (begin0
    (grpc-channel-create-call channel parent 0 cq method-slice #f deadline #f)
    (grpc-slice-unref method-slice)))

;; TODO make this an enum
(define _grpc-call-error _int)

(define grpc-call-start-batch/ffi
  (get-ffi-obj "grpc_call_start_batch" lib-grpc
    (_fun _grpc-call (ops : _cvector) (_size = (cvector-length ops)) _pointer _pointer -> _grpc-call-error)))
(define (grpc-call-start-batch call ops)
  (define-values (tag evt) (make-grpc-completion-queue-tag))
  (define status (grpc-call-start-batch call ops tag #f))
  (unless (zero? status)
    (free-immobile-cell tag)
    (error 'grpc-call-start-batch "Error: ~a" status))
  evt)


