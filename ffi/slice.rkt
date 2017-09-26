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
    [grpc-slice->bytes (c:-> grpc-slice? bytes?)]
    [grpc-slice->bytes!
      (c:-> bytes? exact-nonnegative-integer? grpc-slice? exact-nonnegative-integer?
            exact-nonnegative-integer?)]
    [grpc-slice-length (c:-> grpc-slice? exact-nonnegative-integer?)]
    [make-empty-grpc-slice (c:-> grpc-slice?)]))

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

(define (grpc-slice->bytes! bytes-dest dest-start slice-src slice-start)
  (define-values (length slice-pointer)
    (if (grpc-slice-refcount slice-src)
        (let ([data (union-ref (grpc-slice-data slice-src) 0)])
          (values
            (grpc-slice-refcounted-length data)
            (grpc-slice-refcounted-bytes data)))
        (let ([data (union-ref (grpc-slice-data slice-src) 1)])
          (values
            (grpc-slice-inlined-length data)
            (array-ptr (grpc-slice-inlined-bytes data))))))
  (define copy-amount
    (min
      (- (bytes-length bytes-dest) dest-start)
      (- length slice-start)))

  (memmove
    (ptr-add bytes-dest dest-start)
    (ptr-add slice-pointer slice-start)
    copy-amount)
  copy-amount)

(define (grpc-slice-length slice)
  (if (grpc-slice-refcount slice)
      (grpc-slice-refcounted-length (union-ref (grpc-slice-data slice) 0))
      (grpc-slice-inlined-length (union-ref (grpc-slice-data slice) 1))))

(define (make-empty-grpc-slice)
  (ptr-ref (malloc _grpc-slice) _grpc-slice))
