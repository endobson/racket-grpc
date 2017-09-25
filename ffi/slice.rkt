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
    [_grpc-slice-pointer/null ctype?]
    [grpc-slice? (c:-> any/c boolean?)]
    [grpc-slice-from-copied-buffer (c:-> bytes? grpc-slice?)]
    [grpc-slice-unref (c:-> grpc-slice? void?)]
    [grpc-slice->bytes (c:-> grpc-slice? bytes?)]))

(define-cstruct _grpc-slice-refcounted
  ([bytes _pointer]
   [length _size]))

(define-cstruct _grpc-slice-inlined
  ([length _uint8]
   [bytes (_array _uint8 (+ (ctype-sizeof _size) (ctype-sizeof _pointer) -1))]))

(define-cstruct _grpc-slice
  ([refcount _pointer]
   [data (_union _grpc-slice-refcounted _grpc-slice-inlined)]))

(define grpc-slice-from-copied-buffer
  (get-ffi-obj "grpc_slice_from_copied_buffer" lib-grpc
    (_fun (b : _bytes) (_size = (bytes-length b)) -> _grpc-slice)))

(define grpc-slice-unref
  (get-ffi-obj "grpc_slice_unref" lib-grpc
    (_fun _grpc-slice -> _void)))

(define (grpc-slice->bytes slice)
  (define-values (length start)
    (if (grpc-slice-refcount slice)
        (let ([data (union-ref (grpc-slice-data slice) 0)])
          (values
            (grpc-slice-refcounted-length data)
            (grpc-slice-refcounted-bytes data)))
        (let ([data (union-ref (grpc-slice-data slice) 1)])
          (values 
            (grpc-slice-inlined-length data)
            (array-ptr (grpc-slice-inlined-bytes data))))))
  (define bytes (make-bytes length))
  (memmove bytes start length)
  bytes)
