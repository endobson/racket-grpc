#lang racket/base

(require
  "../client-call.rkt"
  "../lib.rkt"
  "../place.rkt")

(module+ main
  (define chan (grpc-channel-create "localhost:8000" #f))

  (define cq (start-completion-queue))

  (sync (client-call-run (make-client-call chan "/grpc.testing.TestService/Echo" cq))))
