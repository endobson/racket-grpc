#lang racket/base

(require
  "grpc-op-batch.rkt"
  "lib.rkt"
  "buffer-reader.rkt"
  ffi/unsafe
  racket/async-channel
  racket/match)

(struct send-initial-metadata (metadata sema))
(struct send-message (message sema))
(struct send-status (status metadata sema))

(define (create-server-call deadline call-pointer cq) 
  (define cancelled-sema (make-semaphore))
  (define recv-message-channel (make-async-channel))
  (define send-channel (make-async-channel))

  (thread
    (lambda ()
      (define payload-pointer (malloc _pointer 'raw))
      (define cancelled-pointer (malloc _int 'raw))
      (define sema (make-semaphore))
      (let loop ()
        (define call-error
          (grpc-call-start-batch
            call-pointer
            (grpc-op-batch #:recv-message payload-pointer)
            (malloc-immobile-cell sema)))
        (unless (zero? call-error)
          (error 'broken-call))
        (sync sema)
        (define payload (ptr-ref payload-pointer _pointer))
        (when payload
          (async-channel-put
            recv-message-channel
            (grpc-buffer->input-port payload))
          (loop)))
      (define call-error
        (grpc-call-start-batch
          call-pointer
          (grpc-op-batch #:recv-close-on-server cancelled-pointer)
          (malloc-immobile-cell sema)))
      (unless (zero? call-error)
        (error 'broken-call))
      (unless (zero? (ptr-ref cancelled-pointer 'int))
        (semaphore-post cancelled-sema))
      (free payload-pointer)
      (free cancelled-pointer)))

  (thread
    (lambda ()
      (let loop ([state 'before-metadata])
        (match (sync recv-message-channel)
          [(send-initial-metadata metadata sema)
           (define call-error
             (grpc-call-start-batch
               call-pointer
               (grpc-op-batch #:send-initial-metadata 0 #f)
               (malloc-immobile-cell sema)))
           (unless (zero? call-error)
             (error 'broken-call))
           (loop)]
          [(send-message message sema)
           (define call-error
             (grpc-call-start-batch
               call-pointer
               (grpc-op-batch #:send-message message)
               (malloc-immobile-cell sema)))
           (unless (zero? call-error)
             (error 'broken-call))
           (loop)]
          [(send-status status metadata sema)
           (define call-error
             (grpc-call-start-batch
               call-pointer
               (grpc-op-batch #:send-status-from-server 0 #f 0 #f)
               (malloc-immobile-cell sema)))
           (unless (zero? call-error)
             (error 'broken-call))]))))

  (server-call (hash)
               deadline
               (semaphore-peek-evt cancelled-sema)
               (guard-evt (lambda () recv-message-channel))
               send-channel))




(struct server-call (client-metadata
                     deadline
                     cancelled-evt
                     recv-message-evt
                     send-channel))
(struct status (code message))
(define status-ok (status 'ok ""))


(define (server-call-is-cancelled? call)
  (sync/timeout 0 (server-call-cancelled-evt call)))

(define (server-call-send-message call message)
  (define sema (make-semaphore))
  (async-channel-put
    (server-call-send-channel call)
    (send-message message sema)))

(define (server-call-send-initial-metadata call metadata)
  (define sema (make-semaphore))
  (async-channel-put
    (server-call-send-channel call)
    (send-initial-metadata metadata sema)))

(define (server-call-send-status call status metadata)
  (define sema (make-semaphore))
  (async-channel-put
    (server-call-send-channel call)
    (send-status status metadata sema)))

