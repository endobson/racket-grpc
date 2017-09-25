#lang racket/base

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse)

  "ffi/lib.rkt"
  ffi/unsafe
  ffi/unsafe/cvector
  racket/list)

(provide
  grpc-op-batch)

(begin-for-syntax
  (define-splicing-syntax-class base-op^
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
      (~seq #:send-message message)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-message)
            (union-set! (grpc-op-data op) 1 message)))
    (pattern
      (~seq #:send-status-from-server metadata-count metadata status status-details)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-status-from-server)
            (define data (union-ref (grpc-op-data op) 2))
            (set-grpc-send-status-from-server-trailing_metadata_count! data metadata-count)
            (set-grpc-send-status-from-server-trailing_metadata! data metadata)
            (set-grpc-send-status-from-server-status! data status)
            (set-grpc-send-status-from-server-status_details! data status-details)))
    (pattern
      (~seq #:send-close-from-client)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-close-from-client)))
    (pattern
      (~seq #:recv-initial-metadata metadata)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-initial-metadata)
            (union-set! (grpc-op-data op) 3 metadata)))
    (pattern
      (~seq #:recv-message message-ptr)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-message)
            (union-set! (grpc-op-data op) 4 message-ptr)))
    (pattern
      (~seq #:recv-status-on-client recv-status)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-status-on-client)
            (union-set! (grpc-op-data op) 5 recv-status)))
    (pattern
      (~seq #:recv-close-on-server close-ptr)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-close-on-server)
            (set-grpc-recv-close-on-server-cancelled! (union-ref (grpc-op-data op) 6) close-ptr))))

  (define-splicing-syntax-class op^
    #:attributes (test initialize)
    (pattern :base-op^
      #:with test #'#t)
    (pattern
      (~seq #:cond test :base-op^))))


(define-syntax grpc-op-batch
  (syntax-parser
    [(_ ops:op^ ...)
     (define num-ops (length (syntax->list #'(ops ...))))
     (define/with-syntax (tests ...) (generate-temporaries #'(ops.test ...)))
     #`(let ()
         (define tests ops.test) ...
         (define ops-length (count values (list tests ...)))

         (define index 0)
         (define ops-vector (make-cvector _grpc-op ops-length))
         (when tests
            (define op (cvector-ref ops-vector index))
            (set-grpc-op-flags! op 0)
            (set-grpc-op-reserved! op #f)
            (ops.initialize op)
            (set! index (add1 index))) ...
         ops-vector)]))

