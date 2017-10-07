#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))


(provide
  (contract-out
    [immobile-grpc-metadata-array? predicate/c]))

(module* unsafe #f
  (provide
    (contract-out
      [malloc-immobile-grpc-metadata-array (c:-> immobile-grpc-metadata-array?)]
      [free-immobile-grpc-metadata-array (c:-> immobile-grpc-metadata-array? void?)]
      [_immobile-grpc-metadata-array ctype?])))

(define-cstruct _grpc-metadata-array
  ([count _size]
   [capacity _size]
   [metadata _pointer]))

(struct immobile-grpc-metadata-array (pointer))
(define _immobile-grpc-metadata-array
  (make-ctype _grpc-metadata-array-pointer
    immobile-grpc-metadata-array-pointer
    (lambda (x) (error '_immobile-grpc-metadata-array "Cannot make values"))))


(define grpc-metadata-array-init
  (get-ffi-obj "grpc_metadata_array_init" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))

(define grpc-metadata-array-destroy
  (get-ffi-obj "grpc_metadata_array_destroy" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))

(define (malloc-immobile-grpc-metadata-array)
  (define p (ptr-ref (malloc _grpc-metadata-array 'raw) _grpc-metadata-array))
  (grpc-metadata-array-init p)
  (immobile-grpc-metadata-array p))

(define (free-immobile-grpc-metadata-array m)
  (define p (immobile-grpc-metadata-array-pointer m))
  (grpc-metadata-array-destroy p)
  (free p))
