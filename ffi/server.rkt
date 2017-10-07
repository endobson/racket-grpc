#lang racket/base

(require
  "base-lib.rkt"
  "timespec.rkt"
  (submod "timespec.rkt" unsafe)
  "completion-queue.rkt"
  (submod "completion-queue.rkt" unsafe)
  "metadata-array.rkt"
  (submod "metadata-array.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/alloc
  racket/match
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [make-grpc-server (c:-> grpc-completion-queue? (listof grpc-server-port?) grpc-server?)]
    [grpc-server-insecure-http2-port (c:-> (and/c immutable? string?) grpc-server-port?)]))

(module* unsafe #f
  (provide
    (contract-out
      [_grpc-call-details ctype?]
      [grpc-call-details-method any/c]
      [set-grpc-call-details-method! any/c]
      [set-grpc-call-details-method-capacity! any/c]
      [grpc-call-details-host any/c]
      [set-grpc-call-details-host! any/c]
      [set-grpc-call-details-host-capacity! any/c]
      [grpc-call-details-deadline any/c]
      [grpc-server-request-call any/c]
      [grpc-call-unref any/c]
      [grpc-server-create any/c]
      [grpc-server-register-completion-queue any/c]
      [grpc-server-add-insecure-http2-port any/c]
      [grpc-server-start any/c])))

(struct grpc-server-port ())
(struct grpc-server-insecure-http2-port grpc-server-port (address))
(struct grpc-server (pointer cq))

(define (make-grpc-server cq ports)
  (define server
    (((allocator (lambda (s) (grpc-server-shutdown-process s cq)))
      (lambda (cq)
        (define server (grpc-server-create #f #f))
        (grpc-server-register-completion-queue server cq #f)
        server)) cq))

  (for ([port (in-list ports)])
    (match port
      [(grpc-server-insecure-http2-port address)
       (grpc-server-add-insecure-http2-port server address)]))
  (grpc-server-start server)
  (grpc-server server cq))

;; This cleans up the server by sending it a shutdown and then destroying it.
(define (grpc-server-shutdown-process server cq)
  (define-values (tag evt) (make-grpc-completion-queue-tag cq void))
  (grpc-server-shutdown-and-notify server cq tag)
  ;; Once the tag is posted server will stop being held and the next finalizer will run
  (thread
    (sync evt)
    (grpc-server-destroy server)))


(define grpc-server-create
  (get-ffi-obj "grpc_server_create" lib-grpc
    (_fun _pointer _pointer -> _pointer)))

(define grpc-server-add-insecure-http2-port
  (get-ffi-obj "grpc_server_add_insecure_http2_port" lib-grpc
    (_fun _pointer _string -> _int)))

(define grpc-server-start
  (get-ffi-obj "grpc_server_start" lib-grpc
    (_fun _pointer -> _void)))

(define grpc-server-register-method
  (get-ffi-obj "grpc_server_register_method" lib-grpc
    (_fun _pointer _string _string -> _pointer)))

(define-cstruct _grpc-call-details
  ([method _pointer]
   [method-capacity _size]
   [host _pointer]
   [host-capacity _size]
   [deadline _gpr-timespec]))


(define grpc-server-request-call
  (get-ffi-obj "grpc_server_request_call" lib-grpc
    (_fun
      _pointer ;; server
      _pointer ;; call
      _grpc-call-details-pointer ;; call_details
      _immobile-grpc-metadata-array ;; request_metadata
      _pointer ;; call completion queue
      _pointer ;; notification completion queue
      _pointer ;; tag
      -> _int)))

(define grpc-call-unref
  (get-ffi-obj "grpc_call_unref" lib-grpc
    (_fun _pointer -> _void)))

(define grpc-server-register-completion-queue
  (get-ffi-obj "grpc_server_register_completion_queue" lib-grpc
    (_fun _pointer _grpc-completion-queue _pointer -> _void)))

(define grpc-server-shutdown-and-notify
  (get-ffi-obj "grpc_server_shutdown_and_notify" lib-grpc
    (_fun _pointer _grpc-completion-queue _pointer -> _void)))

(define grpc-server-destroy
  (get-ffi-obj "grpc_server_destroy" lib-grpc
    (_fun _pointer -> _void)))
