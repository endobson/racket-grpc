#lang racket/base

(require
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [immobile-int? predicate/c]
    [make-immobile-int (c:-> immobile-int?)]
    [immobile-int-ref (c:-> immobile-int? exact-integer?)]))

(module* unsafe #f
  (provide
    (contract-out
      [_immobile-int ctype?])))


(struct immobile-int (pointer))
(define _immobile-int
  (make-ctype _pointer
    immobile-int-pointer
    (lambda (x) (error '_immobile-int "Cannot make values"))))

(define (make-immobile-int)
  (define p (malloc _int 'atomic-interior))
  (ptr-set! p _int 0)
  (immobile-int p))

(define (immobile-int-ref i)
  (ptr-ref (immobile-int-pointer i) _int))

