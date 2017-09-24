#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-insecure-channel-create (c:-> string? grpc-channel?)]
    [grpc-channel-destroy (c:-> grpc-channel? void)]))


;; Channels
(define _grpc-channel _pointer)
(define grpc-channel? cpointer?)

(define (grpc-insecure-channel-create target)
  (get-ffi-obj "grpc_insecure_channel_create" lib-grpc
               (_fun _string _pointer _pointer -> _grpc-channel)))

(define (grpc-channel-destroy target)
  (get-ffi-obj "grpc_channel_destroy" lib-grpc
               (_fun _grpc-channel -> _grpc-channel)))

