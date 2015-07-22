#lang racket/base

(require
  "../client.rkt"
  "../lib.rkt"
  "../place.rkt")

(module+ main
  (define chan (grpc-channel-create "localhost:8000" #f))

  (define cq (start-completion-queue))

  (sync (send-request cq chan "/grpc.testing.TestService/Echo")))
