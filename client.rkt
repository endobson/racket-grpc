#lang racket/base

(require
  "lib.rkt"
  "grpc-op-batch.rkt"
  "buffer-reader.rkt"
  racket/async-channel
  racket/port
  ffi/unsafe
  racket/list)

(provide send-request)

(define (send-request cq chan method)
  (define deadline (gpr-now))
  (set-gpr-timespec-tv_sec! deadline (+ (gpr-timespec-tv_sec deadline) 1))

  (define call (grpc-channel-create-call chan cq method "localhost" deadline))


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
      #:send-message send-message-buffer
      #:send-close-from-client
      #:recv-initial-metadata recv-metadata))
  (define sema (make-semaphore))

  (define call-error (grpc-call-start-batch call ops (malloc-immobile-cell sema)))
  (unless (zero? call-error)
    (error 'bad-call1 "Error ~a" call-error))
  (sync (semaphore-peek-evt sema))


  (define recv-message-channel (make-async-channel))

  (define read-thread
    (thread
      (lambda ()
        (define payload-pointer (malloc _pointer 'raw))
        (define sema (make-semaphore))
        (let loop ()
          (define call-error
            (grpc-call-start-batch
              call
              (grpc-op-batch #:recv-message payload-pointer)
              (malloc-immobile-cell sema)))
          (unless (zero? call-error)
            (error 'broken-call "Receiving message ~a" call-error))
          (sync sema)
          (define payload (ptr-ref payload-pointer _pointer))
          (when payload
            (async-channel-put
              recv-message-channel
              (port->bytes (grpc-buffer->input-port payload)))
            (loop)))
        (define call-error
          (grpc-call-start-batch
            call
            (grpc-op-batch #:recv-status-on-client recv-status)
            (malloc-immobile-cell sema)))
        (unless (zero? call-error)
          (error 'broken-call "Sending close ~a" call-error))
        (sync sema)
        (free payload-pointer))))


  (handle-evt
    (thread-dead-evt read-thread)
    (lambda (_)
      (define status (ptr-ref recv-status_code _int))
      (case status
        [(0)
         (grpc-call-destroy call)
         (void)]
        [else
          (printf "Error ~a: ~a~n" status (ptr-ref recv-status_details _string))]))))



