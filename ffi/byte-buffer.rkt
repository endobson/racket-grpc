#lang racket/base

(require
  "base-lib.rkt"
  "slice.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-byte-buffer-destroy (c:-> grpc-byte-buffer? void?)]))

(define _grpc-byte-buffer _pointer)
(define grpc-byte-buffer? cpointer?)

(define grpc-byte-buffer-destroy
  (get-ffi-obj "grpc_byte_buffer_destroy" lib-grpc
    (_fun _grpc-byte-buffer -> _void)))

(define-cstruct _grpc-byte-buffer-reader
  ([buffer-in _pointer]
   [buffer-out _pointer]
   [current _uint]))

(define grpc-byte-buffer-reader-init
  (get-ffi-obj "grpc_byte_buffer_reader_init" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer _grpc-byte-buffer -> _void)))

(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer -> _void)))

(define grpc-byte-buffer-reader-next
  (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer _grpc-slice-pointer -> _int)))
