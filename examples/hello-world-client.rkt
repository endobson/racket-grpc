#lang racket/base

(require
  "../client-call.rkt"
  "../ffi/lib.rkt"
  "../ffi/channel.rkt"
  "../ffi/completion-queue.rkt")

(module+ main
  (define cq (make-grpc-completion-queue))
  (define chan (grpc-insecure-channel-create "localhost:50051"))
  (define client-call (make-client-call chan #"/helloworld.Greeter/SayHello" cq))

  (client-call-send-message client-call #"\x0a\x06abcdef")
  (sync (client-call-run client-call))
  (client-call-recv-message client-call))
