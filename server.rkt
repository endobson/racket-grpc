#lang racket/base

(require
  "server-call.rkt"
  "timestamp.rkt"
  "grpc-op-batch.rkt"
  "status.rkt"
  "ffi/server.rkt"
  (submod "ffi/server.rkt" unsafe)
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
  (define cq (make-grpc-completion-queue))

  (define server
    (make-grpc-server
      cq
      (map grpc-server-insecure-http2-port (server-config-addresses config))))

  (define methods (server-config-methods config))

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
