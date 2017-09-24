#lang racket/base

(require
  "../client-call.rkt"
  "../ffi/lib.rkt"
  "../ffi/channel.rkt"
  "../ffi/completion-queue.rkt")

(module+ main
  (define chan (grpc-insecure-channel-create "localhost:8000" #f))
  (define cq (make-grpc-completion-queue))
  ;(define client-call (make-client-call chan "/grpc.testing.TestService/Echo" cq))
  ;(client-call-send-message client-call #"\x08\x00\x10\x12")
  ;(sync (client-call-run client-call))
  ;(client-call-recv-message client-call)


  )
