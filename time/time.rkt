#lang racket/base

(require
  racket/contract
  racket/math)

(provide
  (contract-out
    [time? predicate/c]
    [duration? predicate/c]
    [current-time (-> time?)]
    [time->unix-seconds (-> time? exact-integer?)]
    [time->unix-millis (-> time? exact-integer?)]
    [time->unix-micros (-> time? exact-integer?)]
    [time->unix-nanos (-> time? exact-integer?)]
    [unix-seconds->time (-> exact-integer? time?)]
    [unix-millis->time (-> exact-integer? time?)]
    [unix-micros->time (-> exact-integer? time?)]
    [unix-nanos->time (-> exact-integer? time?)]
    [hours->duration (-> exact-integer? duration?)]
    [minutes->duration (-> exact-integer? duration?)]
    [seconds->duration (-> exact-integer? duration?)]
    [millis->duration (-> exact-integer? duration?)]
    [micros->duration (-> exact-integer? duration?)]
    [nanos->duration (-> exact-integer? duration?)]
    [duration->seconds (-> duration? exact-integer?)]
    [duration->millis (-> duration? exact-integer?)]
    [duration->micros (-> duration? exact-integer?)]
    [duration->nanos (-> duration? exact-integer?)]))

(struct time (nanos))
(struct duration (nanos))

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

(define (current-time)
  (time (exact-truncate (* (current-inexact-milliseconds) 1000000))))

(define (hours->duration t)
  (duration (* t 3600000000000)))
(define (minutes->duration t)
  (duration (* t 60000000000)))
(define (seconds->duration t)
  (duration (* t 1000000000)))
(define (millis->duration t)
  (duration (* t 1000000)))
(define (micros->duration t)
  (duration (* t 1000)))
(define (nanos->duration t)
  (duration t))

(define (duration->seconds t)
  (quotient (duration-nanos t) 1000000000))
(define (duration->millis t)
  (quotient (duration-nanos t) 1000000))
(define (duration->micros t)
  (quotient (duration-nanos t) 1000))
(define (duration->nanos t)
  (duration-nanos t))

