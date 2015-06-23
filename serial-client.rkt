#lang racket/base

(require
  "client.rkt"
  "lib.rkt"
  "place.rkt"
  racket/async-channel)

(module+ main
  (define chan (grpc-channel-create "localhost:8000" #f))

  (define cq (start-completion-queue))

  (for ([i (in-range 100)])
    (collect-garbage)
    (sync (send-request cq chan "/grpc.testing.TestService/GarbageCollect"))
    (time
      (for ([j (in-range 1000)])
        (sync (send-request cq chan "/grpc.testing.TestService/EmptyCall"))))
    (printf "RequestBatch ~a~n" i)))
