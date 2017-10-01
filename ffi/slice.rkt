#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  ffi/unsafe/alloc
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-slice? predicate/c]
    [immobile-grpc-slice? predicate/c]
    [grpc-slice-from-copied-buffer (c:-> bytes? grpc-slice?)]
    [grpc-slice-length (c:-> grpc-slice? exact-nonnegative-integer?)]
    [grpc-slice->bytes (c:-> grpc-slice? bytes?)]
    [grpc-slice->bytes!
      (c:-> bytes? exact-nonnegative-integer? grpc-slice? exact-nonnegative-integer?
            exact-nonnegative-integer?)]
    ;; For use in functions that need slices for long term responses.
    [make-immobile-grpc-slice (c:-> grpc-slice?)]))

(module* unsafe #f
  (provide
    _grpc-slice ;; fun-syntax
    _grpc-slice-pointer ;; fun-syntax
    (contract-out
      [_immobile-grpc-slice-pointer ctype?]
      ;; For use in functions that return reffed slices.
      [_grpc-slice/ffi ctype?]
      [grpc-slice-unref (c:-> grpc-slice/ffi? void?)]
      [grpc-slice (c:-> grpc-slice/ffi? grpc-slice?)])))

(define-cstruct _grpc-slice-refcounted
  ([bytes _pointer]
   [length _size]))

(define-cstruct _grpc-slice-inlined
  ([length _uint8]
   [bytes (_array _uint8 (+ (ctype-sizeof _size) (ctype-sizeof _pointer) -1))]))

(define-cstruct _grpc-slice/ffi
  ([refcount _pointer]
   [data (_union _grpc-slice-refcounted _grpc-slice-inlined)]))

(struct grpc-slice (pointer))
(struct immobile-grpc-slice grpc-slice ())
(define-fun-syntax _grpc-slice
  (syntax-id-rules (_grpc-slice)
    [_grpc-slice (type: _grpc-slice/ffi pre: (x => (grpc-slice-pointer x)))]))
(define-fun-syntax _grpc-slice-pointer
  (syntax-id-rules (_grpc-slice-pointer)
    [_grpc-slice-pointer (type: _grpc-slice/ffi-pointer pre: (x => (grpc-slice-pointer x)))]))
(define _immobile-grpc-slice-pointer
  (make-ctype _grpc-slice/ffi-pointer
    grpc-slice-pointer
    (lambda (x) (error '_immobile-grpc-slice-pointer "Cannot make values"))))

(define grpc-slice-unref
  (get-ffi-obj "grpc_slice_unref" lib-grpc
    (_fun _grpc-slice/ffi -> _void)))

(define grpc-slice-from-copied-buffer
  (let ([raw ((allocator grpc-slice-unref)
              (get-ffi-obj "grpc_slice_from_copied_buffer" lib-grpc
                (_fun (b : _bytes) (_size = (bytes-length b)) -> _grpc-slice/ffi)))])
    (lambda (bytes) (grpc-slice (raw bytes)))))

(define (grpc-slice->bytes slice)
  (define ffi-slice (grpc-slice-pointer slice))
  (define-values (length start)
    (if (grpc-slice/ffi-refcount ffi-slice)
        (let ([data (union-ref (grpc-slice/ffi-data ffi-slice) 0)])
          (values
            (grpc-slice-refcounted-length data)
            (grpc-slice-refcounted-bytes data)))
        (let ([data (union-ref (grpc-slice/ffi-data ffi-slice) 1)])
          (values
            (grpc-slice-inlined-length data)
            (array-ptr (grpc-slice-inlined-bytes data))))))
  (define bytes (make-bytes length))
  (memmove bytes start length)
  bytes)

(define (grpc-slice->bytes! bytes-dest dest-start slice-src slice-start)
  (define ffi-slice (grpc-slice-pointer slice-src))
  (define-values (length slice-pointer)
    (if (grpc-slice/ffi-refcount ffi-slice)
        (let ([data (union-ref (grpc-slice/ffi-data ffi-slice) 0)])
          (values
            (grpc-slice-refcounted-length data)
            (grpc-slice-refcounted-bytes data)))
        (let ([data (union-ref (grpc-slice/ffi-data ffi-slice) 1)])
          (values
            (grpc-slice-inlined-length data)
            (array-ptr (grpc-slice-inlined-bytes data))))))
  (define copy-amount
    (max
      (min
        (- (bytes-length bytes-dest) dest-start)
        (- length slice-start))
      0))

  (memmove
    (ptr-add bytes-dest dest-start)
    (ptr-add slice-pointer slice-start)
    copy-amount)
  copy-amount)

(define (grpc-slice-length slice)
  (define ffi-slice (grpc-slice-pointer slice))
  (if (grpc-slice/ffi-refcount ffi-slice)
      (grpc-slice-refcounted-length (union-ref (grpc-slice/ffi-data ffi-slice) 0))
      (grpc-slice-inlined-length (union-ref (grpc-slice/ffi-data ffi-slice) 1))))

(define (make-immobile-grpc-slice)
  (define slice
    (ptr-ref (malloc _grpc-slice/ffi 'atomic-interior) _grpc-slice/ffi))
  (set-grpc-slice/ffi-refcount! slice #f)
  (set-grpc-slice-inlined-length!
    (union-ref (grpc-slice/ffi-data slice) 1)
    0)
  (register-finalizer slice grpc-slice-unref)
  (immobile-grpc-slice slice))
