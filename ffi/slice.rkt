#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  ffi/unsafe/alloc
  ffi/unsafe/atomic
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [immobile-grpc-slice? predicate/c]
    [make-immobile-grpc-slice (c:-> immobile-grpc-slice?)]))

(module* unsafe #f
  (provide
    _grpc-slice/arg ;; fun-syntax
    _grpc-slice-pointer/arg ;; fun-syntax
    _grpc-slice-pointer/output-arg ;; fun-syntax
    (contract-out
      [_immobile-grpc-slice-pointer ctype?]
      [_grpc-slice-pointer/return ctype?])))

(define-cstruct _grpc-slice-refcounted
  ([bytes _pointer]
   [length _size]))

(define-cstruct _grpc-slice-inlined
  ([length _uint8]
   [bytes (_array _uint8 (+ (ctype-sizeof _size) (ctype-sizeof _pointer) -1))]))

(define-cstruct _grpc-slice/ffi
  ([refcount _pointer]
   [data (_union _grpc-slice-refcounted _grpc-slice-inlined)]))

(define-fun-syntax _grpc-slice/arg
  (syntax-id-rules (_grpc-slice/arg)
    [_grpc-slice/arg
      (type: _grpc-slice/ffi
       pre: (x => (begin
                    (unless (in-atomic-mode?)
                      (error '_grpc-slice/arg "Must be in atomic mode"))
                    (grpc-slice-from-copied-buffer x)))
       post: (x => (grpc-slice-unref x)))]))

(define-fun-syntax _grpc-slice-pointer/arg
  (syntax-id-rules (_grpc-slice-pointer/arg)
    [_grpc-slice-painter/arg
      (type: _grpc-slice/ffi-pointer
       pre: (x => (begin
                    (unless (in-atomic-mode?)
                      (error '_grpc-slice-pointer/arg "Must be in atomic mode"))
                    (grpc-slice-from-copied-buffer x)))
       post: (x => (grpc-slice-unref x)))]))

(define-fun-syntax _grpc-slice-pointer/output-arg
  (syntax-id-rules (_grpc-slice-pointer/output-arg)
    [_grpc-slice-painter/arg
      (type: _grpc-slice/ffi-pointer
       pre: (let ()
              (unless (in-atomic-mode?)
                (error '_grpc-slice-pointer/arg "Must be in atomic mode"))
              (define p (malloc _grpc-slice/ffi))
              (memset p 0 (ctype-sizeof _grpc-slice/ffi))
              (ptr-ref p _grpc-slice/ffi))
       post: (x =>
                (begin0
                  (grpc-slice->bytes x)
                  (grpc-slice-unref x))))]))

(define _grpc-slice-pointer/return
  (make-ctype _grpc-slice/ffi
    (lambda (x) (error '_grpc-slice-pointer/return "Cannot make values"))
    (lambda (x)
      (unless (in-atomic-mode?)
        (error '_grpc-slice/return "Must be in atomic mode"))
      (and x
        (begin0
          (grpc-slice->bytes x)
          (grpc-slice-unref x))))))


(struct immobile-grpc-slice (pointer))
(define _immobile-grpc-slice-pointer
  (make-ctype _grpc-slice/ffi-pointer
    immobile-grpc-slice-pointer
    (lambda (x) (error '_immobile-grpc-slice-pointer "Cannot make values"))))

(define grpc-slice-unref
  (get-ffi-obj "grpc_slice_unref" lib-grpc
    (_fun _grpc-slice/ffi -> _void)))

(define grpc-slice-from-copied-buffer
  (let ([raw ((allocator grpc-slice-unref)
              (get-ffi-obj "grpc_slice_from_copied_buffer" lib-grpc
                (_fun (b : _bytes) (_size = (bytes-length b)) -> _grpc-slice/ffi)))])
    (lambda (bytes) (raw bytes))))

(define (grpc-slice->bytes slice)
  (define-values (length start)
    (if (grpc-slice/ffi-refcount slice)
        (let ([data (union-ref (grpc-slice/ffi-data slice) 0)])
          (values
            (grpc-slice-refcounted-length data)
            (grpc-slice-refcounted-bytes data)))
        (let ([data (union-ref (grpc-slice/ffi-data slice) 1)])
          (values
            (grpc-slice-inlined-length data)
            (array-ptr (grpc-slice-inlined-bytes data))))))
  (define bytes (make-bytes length))
  (memmove bytes start length)
  bytes)

(define (make-immobile-grpc-slice)
  (define slice
    (ptr-ref (malloc _grpc-slice/ffi 'atomic-interior) _grpc-slice/ffi))
  (set-grpc-slice/ffi-refcount! slice #f)
  (set-grpc-slice-inlined-length!
    (union-ref (grpc-slice/ffi-data slice) 1)
    0)
  (register-finalizer slice grpc-slice-unref)
  (immobile-grpc-slice slice))
