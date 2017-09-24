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

  (for ([i (in-range 100)])
    (collect-garbage)
    (sync (send-request cq chan "/grpc.testing.TestService/GarbageCollect"))
    (time
      (for ([j (in-range 1000)])
        (sync (send-request cq chan "/grpc.testing.TestService/EmptyCall"))))
    (printf "RequestBatch ~a~n" i)))
