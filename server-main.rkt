#lang racket/base

(require
  "server.rkt")

(define config
  (server-config
    (hash)
    (list
      "localhost:8000")))

(module+ main
  (start-server config))
