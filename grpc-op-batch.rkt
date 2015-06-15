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
            (union-set! (grpc-op-data op) 6 close-ptr)))))

(define-syntax grpc-op-batch
  (syntax-parser
    [(_ ops:op^ ...)
     (define num-ops (syntax-length #'(ops ...)))
     (define/with-syntax (indices ...) (build-list num-ops values))
     #`(let ()
         (define ops-vector (make-cvector _grpc-op #,num-ops))
         (ops.initialize (cvector-ref ops-vector indices)) ...
         ops-vector)]))
