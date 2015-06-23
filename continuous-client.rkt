#lang racket/base

(require
  "client.rkt"
  "lib.rkt"
  "place.rkt"
  racket/async-channel)

(module+ main

  (define chan (grpc-channel-create "localhost:8000" #f))

  (define cq (start-completion-queue))

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
