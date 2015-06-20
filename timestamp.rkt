#lang racket/base

(provide
  (struct-out timestamp))

(struct timestamp (seconds nanoseconds))
