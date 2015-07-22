#lang racket/base

(require
  "../server.rkt"
  racket/port
  (for-syntax
    racket/base
    racket/list
    syntax/parse))



(define test-service
  (make-service "grpc.testing.TestService"
    (define (EmptyCall input)
      #"EmptyCall")
    (define (Echo input)
      input)))

(begin-for-syntax
  (define-literal-set make-service-literals (define)))

(define-syntax (make-service stx)
  (define-syntax-class method-definition
     #:literal-sets (make-service-literals)
     #:attributes (method)
     (pattern (define (method-name:id arg:id) body:expr)
       #:attr method
         (list (symbol->string (syntax-e #'method-name))
               #'(lambda (arg) body))))
     

  (syntax-parse stx
    [(_ service-name:str defs:method-definition ...)
     #`(make-immutable-hash
         (list
           #,@(for/list ([method (in-list (attribute defs.method))])
                #`(cons #,(string-append "/" (syntax-e #'service-name) "/" (first method))
                        #,(second method)))))]))

(define config
  (server-config
    test-service
    (list
      "localhost:8000")))


(module+ main
  (start-server config))
