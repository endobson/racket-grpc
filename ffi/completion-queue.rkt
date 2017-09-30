#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  ffi/unsafe
  ffi/unsafe/alloc
  racket/match
  racket/place
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    ;; Completion queues
    [grpc-completion-queue? predicate/c]
    [make-grpc-completion-queue (c:-> grpc-completion-queue?)]))

(module* unsafe #f
  (provide
    (contract-out
      [_grpc-completion-queue ctype?]
      [make-grpc-completion-queue-tag (c:-> (values cpointer? evt?))])))

;; Completion events
(define _grpc-completion-type
  (_enum '(shutdown timeout op-complete)))
(define grpc-completion-type?
  (or/c 'shutdown 'timeout 'op-complete))

(define-cstruct _grpc-event
  ([type _grpc-completion-type]
   [success _bool]
   [value _pointer]))

;; Completion queues
(struct grpc-completion-queue (pointer))
(define _grpc-completion-queue
  (make-ctype _pointer
    grpc-completion-queue-pointer
    (lambda () (error 'grpc-completion-queue "Cannot make values"))))

(define grpc-completion-queue-shutdown
  (get-ffi-obj "grpc_completion_queue_shutdown" lib-grpc
    (_fun _grpc-completion-queue -> _void)))

(define grpc-completion-queue-create-for-next/ffi
  (get-ffi-obj "grpc_completion_queue_create_for_next" lib-grpc
               (_fun _pointer -> _pointer)))
(define grpc-completion-queue-create-for-next
  (let ([raw ((allocator grpc-completion-queue-shutdown)
              (lambda ()
                (grpc-completion-queue-create-for-next/ffi #f)))])
    (lambda () (grpc-completion-queue (raw)))))

(define grpc-completion-queue-next/ffi
  (get-ffi-obj "grpc_completion_queue_next" lib-grpc
    (_fun _grpc-completion-queue _gpr-timespec _pointer -> _grpc-event)))
(define (grpc-completion-queue-next cq deadline)
  (grpc-completion-queue-next/ffi cq deadline #f))

(define grpc-completion-queue-destroy
  (get-ffi-obj "grpc_completion_queue_destroy" lib-grpc
    (_fun _grpc-completion-queue -> _void)))

;; Creates a completion queue and starts the place that polls from it.
(define (make-grpc-completion-queue)
  (define cq (grpc-completion-queue-create-for-next))
  (define blocking-place
    (place pch
      (define cq (grpc-completion-queue (sync pch)))
      (let loop ()
        (define result (grpc-completion-queue-next cq (gpr-infinite-future 'monotonic)))
        (case (grpc-event-type result)
          [(shutdown)
           (place-channel-put pch 'shutdown)
           (grpc-completion-queue-destroy cq)]
          [(timeout) (loop)]
          [(op-complete)
           (place-channel-put
             pch
             (list (grpc-event-success result)
                   (grpc-event-value result)))
           (loop)]))))
  (place-channel-put blocking-place (grpc-completion-queue-pointer cq))
  (thread
    (lambda ()
      (let loop ()
        (match (sync blocking-place)
          ['shutdown (void)]
          [(list success pointer)
           (match (ptr-ref pointer _racket)
             [(vector sema success-box)
              (free-immobile-cell pointer)
              (set-box! success-box success)
              (semaphore-post sema)])
           (loop)]))))
  cq)

;; All tags passed to completion queue apis must come from this.
;; The first argument should be passed to the foregin function, and the second is an 'evt?'
;; that will be ready once the underlying event has happened. The return value of the event is
;; true if the op was successful.
(define (make-grpc-completion-queue-tag)
  (define sema (make-semaphore))
  (define b (box 'unset))
  (values
    (malloc-immobile-cell (vector sema b))
    (wrap-evt
      (semaphore-peek-evt sema)
      (lambda (evt) (unbox b)))))
