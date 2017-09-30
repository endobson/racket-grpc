#lang racket/base

(require
  racket/contract)

(provide
  (contract-out
    [time->unix-seconds (-> time? exact-integer?)]
    [time->unix-millis (-> time? exact-integer?)]
    [time->unix-micros (-> time? exact-integer?)]
    [time->unix-nanos (-> time? exact-integer?)]
    [unix-seconds->time (-> exact-integer? time?)]
    [unix-millis->time (-> exact-integer? time?)]
    [unix-micros->time (-> exact-integer? time?)]
    [unix-nanos->time (-> exact-integer? time?)]))

(struct time (nanos))

(define (time->unix-seconds t)
  (quotient (time-nanos t) 1000000000))
(define (time->unix-millis t)
  (quotient (time-nanos t) 1000000))
(define (time->unix-micros t)
  (quotient (time-nanos t) 1000))
(define (time->unix-nanos t)
  (time-nanos t))

(define (unix-seconds->time t)
  (time (* t 1000000000)))
(define (unix-millis->time t)
  (time (* t 1000000)))
(define (unix-micros->time t)
  (time (* t 1000)))
(define (unix-nanos->time t)
  (time t))
