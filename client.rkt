#lang racket/base

(require
  "lib.rkt"
  "place.rkt"
  "grpc-op-batch.rkt"
  "buffer-reader.rkt"
  racket/format
  racket/port
  racket/async-channel
  racket/place
  ffi/unsafe
  ffi/cvector
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


  (define recv-message-buffer-ptr (malloc _pointer 'raw))
  (ptr-set! recv-message-buffer-ptr _pointer #f)

  (define recv-trailing-metadata (make-grpc-metadata-array 0 0 #f))
  (grpc-metadata-array-init recv-trailing-metadata)
  (define recv-status_code (malloc _int 'raw))
  (ptr-set! recv-status_code _int 0)
  (define recv-status_details (malloc _pointer 'raw))
  (ptr-set! recv-status_details _pointer #f)

  (define recv-status_details_capacity (malloc _size_t 'raw))
  (ptr-set! recv-status_details_capacity _size_t 0)
  (define recv-status (make-grpc-recv_status_on_client recv-trailing-metadata recv-status_code recv-status_details recv-status_details_capacity))



  (define ops
    (grpc-op-batch
      #:send-initial-metadata 0 #f
      #:recv-initial-metadata recv-metadata
      #:send-message send-message-buffer
      #:recv-message recv-message-buffer-ptr
      #:send-close-from-client
      #:recv-status-on-client recv-status))

  (define sema (make-semaphore))

  (grpc-call-start-batch call ops (malloc-immobile-cell sema))


  (handle-evt
    sema
    (lambda (_)
      (define status (ptr-ref recv-status_code _int))
      (case status
        [(0)
         (define recv-message-buffer (ptr-ref recv-message-buffer-ptr _pointer))
         (grpc-byte-buffer-destroy recv-message-buffer)
         (grpc-call-destroy call)
         (void)]
        [else
          (printf "Error ~a: ~a~n" status (ptr-ref recv-status_details _string))]))))


(module+ main
  (define cq (start-completion-queue))

  (define responses (make-async-channel))

  (define (request-thread)
    (thread
      (lambda ()
        (for ([i (in-range 100)])
          (async-channel-put
            responses
            (sync (run cq)))))))

  (for ([i (in-range 1000)])
    (request-thread))

  (for ([i (in-range 100)])
    (time
      (for ([j (in-range 1000)])
        (sync responses)))
    (printf "RequestBatch ~a~n" i)))
