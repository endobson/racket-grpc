#lang racket/base

(require
  "client.rkt"
  "ffi/lib.rkt"
  "ffi/channel.rkt"
  "ffi/completion-queue.rkt"
  racket/async-channel)

(module+ main

  (define chan (grpc-insecure-channel-create "localhost:8000"))

  (define cq (make-grpc-completion-queue))

  (define responses (make-async-channel))

  (define (request-thread)
    (thread
      (lambda ()
        (for ([i (in-range 100)])
          (async-channel-put
            responses
            (sync (send-request cq chan "/grpc.testing.TestService/EmptyCall")))))))

  (for ([i (in-range 1000)])
    (request-thread))

  (for ([i (in-range 100)])
    (time
      (for ([j (in-range 1000)])
        (sync responses)))
    (printf "RequestBatch ~a~n" i)))
