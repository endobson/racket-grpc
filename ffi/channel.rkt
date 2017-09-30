#lang racket/base

(require
  "base-lib.rkt"
  "completion-queue.rkt"
  (submod "completion-queue.rkt" unsafe)
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [_grpc-channel ctype?]
    [grpc-channel? (c:-> any/c boolean?)]
    [grpc-insecure-channel-create (c:-> string? grpc-channel?)]
    [grpc-channel-destroy (c:-> grpc-channel? void?)]
    [grpc-channel-check-connectivity-state (c:-> grpc-channel? boolean? exact-nonnegative-integer?)]
    [grpc-channel-ping (c:-> grpc-channel? grpc-completion-queue? evt?)]))


;; Channels
(define _grpc-channel _pointer)
(define grpc-channel? cpointer?)

(define grpc-insecure-channel-create/ffi
  (get-ffi-obj "grpc_insecure_channel_create" lib-grpc
               (_fun _string _pointer _pointer -> _grpc-channel)))
(define (grpc-insecure-channel-create target)
  (grpc-insecure-channel-create/ffi target #f #f))

(define grpc-channel-destroy
  (get-ffi-obj "grpc_channel_destroy" lib-grpc
               (_fun _grpc-channel -> _void)))

(define grpc-channel-check-connectivity-state
  (get-ffi-obj "grpc_channel_check_connectivity_state" lib-grpc
               (_fun _grpc-channel _bool -> _int)))

(define grpc-channel-ping/ffi
  (get-ffi-obj "grpc_channel_ping" lib-grpc
               (_fun _grpc-channel _grpc-completion-queue _pointer _pointer -> _grpc-channel)))
(define (grpc-channel-ping channel cq)
  (define-values (tag evt) (make-grpc-completion-queue-tag))
  (grpc-channel-ping/ffi channel cq tag #f)
  evt)

