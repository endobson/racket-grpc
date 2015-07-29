#lang racket/base

(require
  "../client-call.rkt"
  "../lib.rkt"
  "../place.rkt")

(module+ main
  (define chan (grpc-channel-create "localhost:8000" #f))

  (define cq (start-completion-queue))
  (define client-call (make-client-call chan "/grpc.testing.TestService/Echo" cq))
  (client-call-send-message client-call #"\x08\x00\x10\x12")
  (sync (client-call-run client-call))
  (client-call-recv-message client-call)


  )
