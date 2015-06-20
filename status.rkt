#lang racket/base

(provide
  ok-status
  status-code
  status-message
  status?)

(struct status (code message))

(define ok-status (status 'ok ""))
