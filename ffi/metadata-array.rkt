#lang racket/base

(require
  "base-lib.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))


(provide
  (contract-out
    [immobile-grpc-metadata-array? predicate/c]
    [make-immobile-grpc-metadata-array (c:-> immobile-grpc-metadata-array?)]))

(module* unsafe #f
  (provide
    (contract-out
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

(define (make-immobile-grpc-metadata-array)
  (define m (ptr-ref (malloc _grpc-metadata-array 'atomic-interior) _grpc-metadata-array))
  (grpc-metadata-array-init m)
  (register-finalizer m grpc-metadata-array-destroy)
  (immobile-grpc-metadata-array m))
