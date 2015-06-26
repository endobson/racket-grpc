#lang racket/base

(require
  "grpc-op-batch.rkt"
  "lib.rkt"
  "buffer-reader.rkt"
  "timestamp.rkt"
  "return-box.rkt"
  ffi/unsafe
  racket/async-channel
  racket/port
  racket/match)

(provide
  request-server-call
  server-call-wait
  server-call-method
  server-call-recv-message-evt
  server-call-send-initial-metadata
  server-call-send-message
  server-call-send-batch
  server-call-send-status)

(struct send-initial-metadata (metadata sema rb))
(struct send-message (message sema rb))
(struct send-status (status metadata sema rb))
(struct send-batch (ops sema rb))

(define-cstruct _server-context
  ([call _pointer]
   [payload _pointer]
   [details _grpc-call-details]
   [metadata _grpc-metadata-array]
   [cancelled _int]))


(define (request-server-call server cq)
  (define ctx (cast (malloc _server-context 'raw) _pointer _server-context-pointer))

  (define call-pointer (server-context-call-pointer ctx))
  (set-server-context-call! ctx #f)
  (define payload (server-context-payload-pointer ctx))
  (set-server-context-payload! ctx #f)
  (define details (server-context-details ctx))
  (set-grpc-call-details-method! details #f)
  (set-grpc-call-details-method-capacity! details 0)
  (set-grpc-call-details-host! details #f)
  (set-grpc-call-details-host-capacity! details 0)
  (define deadline (grpc-call-details-deadline details))
  (define metadata (server-context-metadata ctx))
  (grpc-metadata-array-init metadata)

  (define sema (make-semaphore))

  (grpc-server-request-call
    server
    call-pointer
    details
    metadata
    cq
    cq
    (malloc-immobile-cell sema))
  (sync sema)

  (define method (cast (grpc-call-details-method details) _pointer _string))
  (define call (ptr-ref call-pointer _pointer))
  (free ctx)
  (create-server-call (timestamp 0 0) call method cq))



(define (create-server-call deadline grpc-call method cq)
  (define cancelled-sema (make-semaphore))
  (define recv-message-channel (make-async-channel))
  (define send-message-channel (make-async-channel))

  (define read-thread
    (thread
      (lambda ()
        (define payload-pointer (malloc _pointer 'raw))
        (define sema (make-semaphore))
        (let loop ()
          (define call-error
            (grpc-call-start-batch
              grpc-call
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
        (free payload-pointer))))

  (define write-thread
    (thread
      (lambda ()
        (let loop ([state 'before-metadata])
          (match (sync send-message-channel)
            [(send-initial-metadata metadata sema rb)
             (set-return-box! rb 
               (grpc-call-start-batch
                 grpc-call
                 (grpc-op-batch #:send-initial-metadata 0 #f)
                 (malloc-immobile-cell sema)))
             (loop 'after-metadata)]
            [(send-message message sema rb)
             (define send-message-slice (gpr-slice-from-copied-buffer message))
             (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))
             (gpr-slice-unref send-message-slice)

             (set-return-box! rb 
               (grpc-call-start-batch
                 grpc-call
                 (grpc-op-batch #:send-message send-message-buffer)
                 (malloc-immobile-cell sema)))
             (grpc-byte-buffer-destroy send-message-buffer)
             (loop 'after-metadata)]
            [(send-batch ops sema rb)
             (set-return-box! rb 
               (grpc-call-start-batch
                 grpc-call
                 ops
                 (malloc-immobile-cell sema)))
             (loop 'after-metadata)]
            [(send-status status metadata sema rb)
             (set-return-box! rb 
               (grpc-call-start-batch
                 grpc-call
                 (grpc-op-batch #:send-status-from-server 0 #f 0 #f)
                 (malloc-immobile-cell sema)))])))))

  (server-call (hash)
               deadline
               method
               grpc-call
               (semaphore-peek-evt cancelled-sema)
               (guard-evt (lambda () recv-message-channel))
               send-message-channel
               (thread-dead-evt read-thread)
               (thread-dead-evt write-thread)))


(struct server-call (client-metadata
                     deadline
                     method
                     grpc-call
                     cancelled-evt
                     recv-message-evt
                     send-channel
                     read-thread-finished-evt
                     write-thread-finished-evt))


(define (server-call-is-cancelled? call)
  (sync/timeout 0 (server-call-cancelled-evt call)))

(define (server-call-send-message call message)
  (define sema (make-semaphore))
  (define rb (make-return-box))
  (async-channel-put
    (server-call-send-channel call)
    (send-message message sema rb))
  (sync rb)
  (semaphore-peek-evt sema))

(define (server-call-send-initial-metadata call metadata)
  (define sema (make-semaphore))
  (define rb (make-return-box))
  (async-channel-put
    (server-call-send-channel call)
    (send-initial-metadata metadata sema rb))
  (sync rb)
  (semaphore-peek-evt sema))

(define (server-call-send-status call status metadata)
  (define sema (make-semaphore))
  (define rb (make-return-box))
  (async-channel-put
    (server-call-send-channel call)
    (send-status status metadata sema rb))
  (sync rb)
  (semaphore-peek-evt sema))

(define (server-call-send-batch call)
  (define sema (make-semaphore))
  (define rb (make-return-box))

  (define send-message-slice (gpr-slice-from-copied-buffer #""))
  (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))
  (gpr-slice-unref send-message-slice)
  (async-channel-put
    (server-call-send-channel call)
    (send-batch
      (grpc-op-batch #:send-initial-metadata 0 #f
                     #:send-message send-message-buffer
                     #:send-status-from-server 0 #f 0 #f)
      sema
      rb))
  (sync rb)
  (grpc-byte-buffer-destroy send-message-buffer)

  (semaphore-peek-evt sema))




(define (server-call-wait call)
  (sync (server-call-read-thread-finished-evt call))
  (sync (server-call-write-thread-finished-evt call))
  (grpc-call-destroy (server-call-grpc-call call)))
