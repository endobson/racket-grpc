#lang racket/base

(require
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [immobile-int? predicate/c]))

(module* unsafe #f
  (provide
    (contract-out
      [_immobile-int ctype?]
      [malloc-immobile-int (c:-> immobile-int?)]
      [immobile-int-ref (c:-> immobile-int? exact-integer?)]
      [free-immobile-int (c:-> immobile-int? void?)])))

(struct immobile-int (pointer))
(define _immobile-int
  (make-ctype _pointer
    immobile-int-pointer
    (lambda (x) (error '_immobile-int "Cannot make values"))))

(define (malloc-immobile-int)
  (define p (malloc _int 'raw))
  (ptr-set! p _int 0)
  (immobile-int p))

(define (free-immobile-int i)
  (free (immobile-int-pointer i)))

(define (immobile-int-ref i)
  (ptr-ref (immobile-int-pointer i) _int))

