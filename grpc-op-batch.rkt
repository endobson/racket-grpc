#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse)

  "lib.rkt"
  ffi/unsafe
  ffi/unsafe/cvector)

(provide grpc-op-batch)

(define-syntax grpc-op-batch
  (syntax-parser
    [(_
       #:send-initial-metadata initial-metadata-count initial-metadata-buffer
       #:recv-initial-metadata recv-metadata
       #:send-message send-message-buffer
       #:recv-message recv-message-buffer-ptr
       #:send-close-from-client
       #:recv-status-on-client recv-status)
     #'(let ()
         (define ops (make-cvector _grpc-op 6))
         (define op1 (cvector-ref ops 0))
         (define op2 (cvector-ref ops 1))
         (define op3 (cvector-ref ops 2))
         (define op4 (cvector-ref ops 3))
         (define op5 (cvector-ref ops 4))
         (define op6 (cvector-ref ops 5))
         (set-grpc-op-op! op1 'send-initial-metadata)
         (set-grpc-send-initial-metadata-count!
           (union-ref (grpc-op-data op1) 0)
           initial-metadata-count)
         (set-grpc-send-initial-metadata-metadata!
           (union-ref (grpc-op-data op1) 0)
           initial-metadata-buffer)
         (set-grpc-op-op! op2 'recv-initial-metadata)
         (union-set! (grpc-op-data op2) 3 recv-metadata)
         (set-grpc-op-op! op3 'send-message)
         (union-set! (grpc-op-data op3) 1 send-message-buffer)
         (set-grpc-op-op! op4 'recv-message)
         (union-set! (grpc-op-data op4) 4 recv-message-buffer-ptr)
         (set-grpc-op-op! op5 'send-close-from-client)
         (set-grpc-op-op! op6 'recv-status-on-client)
         (union-set! (grpc-op-data op6) 5 recv-status)
         ops)]))
