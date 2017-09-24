#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [_grpc-slice ctype?]
    [_grpc-slice-pointer ctype?]
    [grpc-slice? (c:-> any/c boolean?)]
    [grpc-slice-from-copied-buffer (c:-> bytes? grpc-slice?)]
    [grpc-slice-unref (c:-> grpc-slice? void?)]))


(define-cstruct _grpc-slice
  ([refcount _pointer]
   [data (_union _grpc-slice-refcounted _grpc-slice-inlined)]))

(define-cstruct _grpc-slice-refcounted
  ([bytes _pointer]
   [length _size]))

(define-cstruct _grpc-slice-inlined
  ([length _uint8]
   [bytes (_array _uint8 (+ (ctype-sizeof _size) (ctype-sizeof _pointer) -1))]))


(define grpc-slice-from-copied-buffer
  (get-ffi-obj "grpc_slice_from_copied_buffer" lib-grpc
    (_fun (b : _bytes) (_size = (bytes-length b)) -> _grpc-slice)))

(define grpc-slice-unref
  (get-ffi-obj "grpc_slice_unref" lib-grpc
    (_fun _grpc-slice -> _void)))

