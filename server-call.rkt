#lang racket/base

(require
  "grpc-op-batch.rkt"
  "lib.rkt"
  "buffer-reader.rkt"
  "timestamp.rkt"
  "return-box.rkt"
  "status.rkt"
  ffi/unsafe
  racket/async-channel
  racket/port
  racket/match)

(provide
  request-server-call
  server-call-wait
  server-call-method
  server-call-recv-message-evt
  server-call-send)

(struct send-data (initial-metadata message status trailing-metadata sema rb))

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
            [(send-data initial-metadata message status trailing-metadata sema rb)
             (unless (or (not initial-metadata) (hash-empty? initial-metadata))
               (error 'nyi))
             (unless (or (not status) (equal? status ok-status))
               (error 'nyi))
             (unless (or (not trailing-metadata) (hash-empty? trailing-metadata))
               (error 'nyi))

             (define send-initial-metadata
               (or initial-metadata
                   (and (eq? state 'before-metadata) (hash))))

             (define send-message-buffer
               (and message
                 (let ([send-message-slice (gpr-slice-from-copied-buffer message)])
                   (begin0
                     (grpc-raw-byte-buffer-create send-message-slice 1)
                     (gpr-slice-unref send-message-slice)))))

             (set-return-box! rb 
               (grpc-call-start-batch
                 grpc-call
                 (grpc-op-batch
                   #:cond send-initial-metadata #:send-initial-metadata 0 #f
                   #:cond send-message-buffer #:send-message send-message-buffer
                   #:cond status #:send-status-from-server 0 #f 0 #f)
                 (malloc-immobile-cell sema)))
             (when send-message-buffer
               (grpc-byte-buffer-destroy send-message-buffer))
             (unless status
               (loop 'after-metadata))])))))

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

(define (server-call-send call
          #:initial-metadata [initial-metadata #f]
          #:message [message #f]
          #:status [status #f]
          #:trailing-metadata [trailing-metadata #f])
  (define sema (make-semaphore))
  (define rb (make-return-box))

  (async-channel-put
    (server-call-send-channel call)
    (send-data initial-metadata message status trailing-metadata sema rb))
  (unless (zero? (sync rb))
    (error 'server-call-send-ops "Bad status code"))
  (semaphore-peek-evt sema))





(define (server-call-wait call)
  (sync (server-call-read-thread-finished-evt call))
  (sync (server-call-write-thread-finished-evt call))
  (grpc-call-destroy (server-call-grpc-call call)))
