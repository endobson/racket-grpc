#lang racket/base

(require
  "channel.rkt"
  "completion-queue.rkt"
  "timespec.rkt"
  ffi/unsafe
  racket/match
  racket/place)

(module* main #f
  (define cq (make-grpc-completion-queue))
  (define channel (grpc-insecure-channel-create "localhost:50051"))
  (grpc-channel-check-connectivity-state channel #f))
  
