#lang racket/base

(require
  "ffi/byte-buffer.rkt"
  "ffi/call.rkt"
  "ffi/lib.rkt"
  "ffi/slice.rkt"
  ffi/unsafe
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse))

(provide
  call-with-malloc
  malloc-struct)

(define (call-with-malloc _ctype fun #:cast [_cast-type #f])
  (define pointer (malloc _ctype 'raw))
  (define casted-pointer (if _cast-type (cast pointer _pointer _cast-type) pointer))
  (fun casted-pointer)
  (free pointer))


(define-syntax malloc-struct
  (syntax-parser
    ([_ _ctype:id]
     (define/with-syntax _pointer-ctype (format-id #'_ctype "~a-pointer" #'_ctype))
     #'(cast (malloc _ctype 'raw) _pointer _pointer-ctype))))

(define-syntax call-with-malloc-struct
  (syntax-parser
    ([_ _ctype:id fun:expr]
     #'(let ()
         (define pointer (malloc-struct _ctype))
         (fun pointer)
         (free pointer)))))
