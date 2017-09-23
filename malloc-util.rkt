#lang racket/base

(require
  "lib.rkt"
  ffi/unsafe
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse))

(provide
  call-with-malloc
  call-with-malloc-grpc-metadata-array
  call-with-malloc-grpc-byte-buffer
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

(define (call-with-malloc-grpc-metadata-array fun)
  (call-with-malloc-struct _grpc-metadata-array
    (λ (metadata)
      (grpc-metadata-array-init metadata)
      (fun metadata)
      (grpc-metadata-array-destroy metadata))))

(define (call-with-malloc-grpc-byte-buffer bytes fun)
  (define slice (grpc-slice-from-copied-buffer bytes))
  (define buffer (grpc-raw-byte-buffer-create slice 1))
  (grpc-slice-unref slice)
  (fun buffer)
  (grpc-byte-buffer-destroy buffer))
