#lang racket/base

(require
  "server-call.rkt"
  "timestamp.rkt"
  "grpc-op-batch.rkt"
  "status.rkt"
  "ffi/lib.rkt"
  "ffi/completion-queue.rkt"
  "ffi/byte-buffer.rkt"
  racket/port
  racket/async-channel
  racket/match
  ffi/unsafe
  racket/list
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [server-config
      (c:-> (hash/c (and/c string? immutable?) (c:-> any/c any) #:immutable #t)
            (listof (and/c string? immutable?))
            server-config?)]
    [start-server (c:-> server-config? none/c)]))

(struct server-config
        (methods addresses))



(define (start-server config)
  (define server (grpc-server-create #f))
  (define cq (make-grpc-completion-queue))

  (grpc-server-register-completion-queue server cq)
  (for ([address (server-config-addresses config)])
    (grpc-server-add-insecure-http2-port server address))

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
                #:message #"response"
                #:status ok-status))

        (server-call-wait server-call)))

    (loop)))
