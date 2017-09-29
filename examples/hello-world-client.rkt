#lang racket/base

(require
  "../client-call.rkt"
  "../ffi/lib.rkt"
  "../ffi/channel.rkt"
  "../ffi/completion-queue.rkt"
  racket/promise)

(module+ main
  (define cq (make-grpc-completion-queue))
  (define chan (grpc-insecure-channel-create "localhost:50051"))
  (define stub (make-client-stub chan #"/helloworld.Greeter/SayHello" cq))

  (force (client-stub-call stub #"\x0a\x06abcdef"))
  (force (client-stub-call stub #"\x0a\x04adef"))
  (force (client-stub-call stub #"\x0a\x05abdef"))
  (force (client-stub-call stub #"\x0a\x05adcef")))

