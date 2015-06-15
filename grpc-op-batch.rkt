#lang racket/base

(require
  (for-syntax
    racket/base
    racket/syntax
    unstable/syntax
    syntax/parse)

  "lib.rkt"
  ffi/unsafe
  ffi/unsafe/cvector)

(provide grpc-op-batch)

(begin-for-syntax
  (define-splicing-syntax-class op^
    #:attributes (initialize)
    (pattern
      (~seq #:send-initial-metadata count metadata)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-initial-metadata)
            (set-grpc-send-initial-metadata-count!
              (union-ref (grpc-op-data op) 0)
              count)
            (set-grpc-send-initial-metadata-metadata!
              (union-ref (grpc-op-data op) 0)
              metadata)))
    (pattern
      (~seq #:recv-initial-metadata metadata)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-initial-metadata)
            (union-set! (grpc-op-data op) 3 metadata)))
    (pattern
      (~seq #:send-message message)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-message)
            (union-set! (grpc-op-data op) 1 message)))
    (pattern
      (~seq #:recv-message message-ptr)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-message)
            (union-set! (grpc-op-data op) 4 message-ptr)))
    (pattern
      (~seq #:send-close-from-client)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-close-from-client)))
    (pattern
      (~seq #:recv-status-on-client recv-status)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-status-on-client)
            (union-set! (grpc-op-data op) 5 recv-status)))))

(define-syntax grpc-op-batch
  (syntax-parser
    [(_ ops:op^ ...)
     (define num-ops (syntax-length #'(ops ...)))
     (define/with-syntax (indices ...) (build-list num-ops values))
     #`(let ()
         (define ops-vector (make-cvector _grpc-op #,num-ops))
         (ops.initialize (cvector-ref ops-vector indices)) ...
         ops-vector)]))
