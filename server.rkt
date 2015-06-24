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

        (define send-message-slice (gpr-slice-from-copied-buffer output))
        (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))
        (gpr-slice-unref send-message-slice)

        (server-call-send-initial-metadata server-call (hash))
        (server-call-send-message server-call send-message-buffer)
        (server-call-send-status server-call ok-status (hash))



        (grpc-byte-buffer-destroy send-message-buffer)
        (server-call-wait server-call)))

    (loop)))
