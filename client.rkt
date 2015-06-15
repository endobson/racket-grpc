#lang racket/base

(require
  "lib.rkt"
  "place.rkt"
  "buffer-reader.rkt"
  racket/format
  racket/port
  racket/place
  ffi/unsafe
  ffi/cvector
  ffi/unsafe/cvector
  racket/list)

(define chan (grpc-channel-create "localhost:8000" #f))

(define (run cq)
  (define deadline (gpr-now))
  (set-gpr-timespec-tv_sec! deadline (+ (gpr-timespec-tv_sec deadline) 1))
  
  (define call (grpc-channel-create-call chan cq "/grpc.testing.TestService/EmptyCall" "localhost" deadline))
  
  
  (define recv-metadata (make-grpc-metadata-array 0 0 #f))
  (grpc-metadata-array-init recv-metadata)

  (define send-message-slice (gpr-slice-from-copied-buffer #"\x08\x00\x10\x12"))
  (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))

  
  (define recv-message-buffer-ptr (malloc _pointer))
  (ptr-set! recv-message-buffer-ptr _pointer #f)
  
  (define recv-trailing-metadata (make-grpc-metadata-array 0 0 #f))
  (grpc-metadata-array-init recv-trailing-metadata)
  (define recv-status_code (malloc _int))
  (ptr-set! recv-status_code _int 0)
  (define recv-status_details (malloc _pointer))
  (ptr-set! recv-status_details _pointer #f)
  
  (define recv-status_details_capacity (malloc _size_t))
  (ptr-set! recv-status_details_capacity _size_t 0)
  (define recv-status (make-grpc-recv_status_on_client recv-trailing-metadata recv-status_code recv-status_details recv-status_details_capacity))
  
  (define ops (make-cvector _grpc-op 6))
  (define op1 (cvector-ref ops 0))
  (define op2 (cvector-ref ops 1))
  (define op3 (cvector-ref ops 2))
  (define op4 (cvector-ref ops 3))
  (define op5 (cvector-ref ops 4))
  (define op6 (cvector-ref ops 5))
  (set-grpc-op-op! op1 'send-initial-metadata)
  (set-grpc-send-initial-metadata-count! (union-ref (grpc-op-data op1) 0) 0)
  (set-grpc-op-op! op2 'recv-initial-metadata)
  (union-set! (grpc-op-data op2) 3 recv-metadata)
  (set-grpc-op-op! op3 'send-message)
  (union-set! (grpc-op-data op3) 1 send-message-buffer)
  (set-grpc-op-op! op4 'recv-message)
  (union-set! (grpc-op-data op4) 4 recv-message-buffer-ptr)
  (set-grpc-op-op! op5 'send-close-from-client)
  (set-grpc-op-op! op6 'recv-status-on-client)
  (union-set! (grpc-op-data op6) 5 recv-status)
  

  (define sema (make-semaphore))
  
  (grpc-call-start-batch call ops (malloc-immobile-cell sema))
  (sync sema)

  (define status (ptr-ref recv-status_code _int))
  (case status
    [(0)
     (define recv-message-buffer (ptr-ref recv-message-buffer-ptr _pointer))
     (void)
     (write (port->bytes (grpc-buffer->input-port recv-message-buffer)))
     (newline)]
    [else
      (printf "Error ~a: ~a~n" status (ptr-ref recv-status_details _string))]))
  
  
(module+ main
  (define cq (start-completion-queue))

  (for ([i (in-range 100)])
    (time
      (for ([j (in-range 4000)])
        (run cq)))
    (printf "RequestBatch ~a~n" i)))
