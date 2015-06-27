#lang racket/base

(require
  "lib.rkt"
  "place.rkt"
  "server-call.rkt"
  "timestamp.rkt"
  "grpc-op-batch.rkt"
  "buffer-reader.rkt"
  "status.rkt"
  racket/port
  racket/async-channel
  racket/match
  ffi/unsafe
  racket/list)

(struct server-config
        (methods addresses))

(provide start-server server-config)

(define (start-server config)
  (define server (grpc-server-create #f))
  (define cq (start-completion-queue))

  (grpc-server-register-completion-queue server cq)
  (for ([address (server-config-addresses config)])
    (grpc-server-add-http2-port server address))

  (grpc-server-start server)

  (define methods
    (for/hash ([(k v) (in-hash (server-config-methods config))])
      (values
        (string->immutable-string k)
        v)))



  (define (server-fun input)
    (port->bytes input))

  (let loop ()
    (define server-call (request-server-call server cq))

    (thread
      (lambda ()
        (define message (sync (server-call-recv-message-evt server-call)))

        (define output
          (match (hash-ref methods (server-call-method server-call) 'unimplemented)
            ['unimplemented
             ;; TODO(endobson) send back unimplemented status code]
             #""]
            [fun
              (fun message)]))

        (sync (server-call-send server-call
                #:initial-metadata (hash)
                #:message #"response"
                #:status ok-status
                #:trailing-metadata (hash)))

        (server-call-wait server-call)))

    (loop)))
