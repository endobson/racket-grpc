#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  (submod "timespec.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/alloc
  racket/match
  racket/place
  (rename-in
    racket/contract
    [-> c:->]
    [->* c:->*]))

(provide
  (contract-out
    ;; Completion queues
    [grpc-completion-queue? predicate/c]
    [make-grpc-completion-queue (c:-> grpc-completion-queue?)]))

(module* unsafe #f
  (provide
    _grpc-completion-queue ;; fun-syntax
    (contract-out
      [make-grpc-completion-queue-tag (c:->* () #:rest (listof any/c) (values cpointer? evt?))])))

;; Completion events
(define _grpc-completion-type
  (_enum '(shutdown timeout op-complete)))
(define grpc-completion-type?
  (or/c 'shutdown 'timeout 'op-complete))

(define-cstruct _grpc-event
  ([type _grpc-completion-type]
   [success _bool]
   [value (_fun _bool -> _void)]))

;; Completion queues
(struct grpc-completion-queue (pointer))
(define-fun-syntax _grpc-completion-queue
  (syntax-id-rules (_grpc-completion-queue)
    [_grpc-completion-queue (type: _pointer pre: (x => (grpc-completion-queue-pointer x)))]))

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
           (grpc-completion-queue-destroy cq)]
          [(timeout) (loop)]
          [(op-complete)
           ;; The value is a racket callback that goes back to the original place
           ((grpc-event-value result) (grpc-event-success result))
           (loop)]))))
  (place-channel-put blocking-place (grpc-completion-queue-pointer cq))
  cq)

(define _completion-queue-callback
  (_fun #:async-apply (lambda (t) (t)) _bool -> _void))

;; All tags passed to completion queue apis must come from this.
;;
;; The arguments will be referenced until the event has returned.
;; This allows for holding onto memory that is being read/written to by the call.
;;
;; The first return value should be passed to the foreign function, and the second is an 'evt?'
;; that will be ready once the underlying event has happened. The return value of the event is
;; true if the op was successful.
(define (make-grpc-completion-queue-tag . refs)
  (define sema (make-semaphore))
  ;; This makes sure that the refs stay reachable until the callback is called.
  (define b (box refs))
  (define t
    (thread
      (lambda ()
        (set-box! b (thread-receive))
        (semaphore-post sema))))

  ;; The immobile cell ensures that the function pointer is reachable until the callback is called
  (define immobile-cell (malloc-immobile-cell #f))
  (define (callback success)
    (free-immobile-cell immobile-cell)
    (thread-send t success)
    (void))

  (define fp (function-ptr callback _completion-queue-callback))
  (ptr-set! immobile-cell _racket fp)

  (values
    fp
    (wrap-evt
      (semaphore-peek-evt sema)
      (lambda (evt) (unbox b)))))
