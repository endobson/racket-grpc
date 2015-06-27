#lang racket/base

(require
  racket/promise)

(provide
  make-return-box
  return-box?
  set-return-box!)

(struct return-box (sema (v #:mutable))
  #:property prop:evt
    (lambda (b)
      (wrap-evt
        (semaphore-peek-evt (return-box-sema b))
        (lambda (_)
          (force (return-box-v b))))))
(define (make-return-box)
  (return-box (make-semaphore) #f))
(define (set-return-box! b v)
  (set-return-box-v! b v)
  (semaphore-post (return-box-sema b)))
