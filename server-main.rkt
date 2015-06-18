#lang racket/base

(require
  "server.rkt")

(define config
  (server-config
    (hash
      "/grpc.testing.TestService/EmptyCall"
      (lambda (input)
        #"EmptyCall"))
    (list
      "localhost:8000")))

(module+ main
  (start-server config))
