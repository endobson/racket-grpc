#lang racket/base

(provide
  ok-status
  status-code
  status-message
  status?)

(struct status (code message) #:transparent)

(define ok-status (status 'ok ""))
