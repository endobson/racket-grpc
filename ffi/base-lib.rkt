#lang racket/base

(require
  racket/runtime-path
  ffi/unsafe)

(provide lib-grpc)

(define-runtime-path lib-grpc-path "./libgrpc_unsecure.so")
(define lib-grpc (ffi-lib lib-grpc-path))

((get-ffi-obj "grpc_init" lib-grpc (_fun -> _void)))
