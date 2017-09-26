#lang racket/base

(require
  "base-lib.rkt"
  "call.rkt"
  "completion-queue.rkt"
  "timespec.rkt"
  racket/format
  racket/place
  ffi/unsafe
  ffi/cvector
  ffi/unsafe/cvector
  racket/list)
(provide (all-defined-out))


(define _grpc-metadata _pointer)
(define _grpc-call _pointer)
(define _size_t _int64)


(define grpc-metadata-array-init
  (get-ffi-obj "grpc_metadata_array_init" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))

(define grpc-metadata-array-destroy
  (get-ffi-obj "grpc_metadata_array_destroy" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))

(define grpc-server-create
  (get-ffi-obj "grpc_server_create" lib-grpc
    (_fun _pointer -> _pointer)))

(define grpc-server-add-insecure-http2-port
  (get-ffi-obj "grpc_server_add_insecure_http2_port" lib-grpc
    (_fun _pointer _string -> _int)))

(define grpc-server-start
  (get-ffi-obj "grpc_server_start" lib-grpc
    (_fun _pointer -> _void)))

(define grpc-server-register-method
  (get-ffi-obj "grpc_server_register_method" lib-grpc
    (_fun _pointer _string _string -> _pointer)))

(define grpc-server-request-registered-call
  (get-ffi-obj "grpc_server_request_registered_call" lib-grpc
    (_fun
      _pointer ;; server
      _pointer ;; registered method
      _pointer ;; call
      _gpr-timespec-pointer ;; deadline
      _grpc-metadata-array-pointer ;; request_metadata
      _pointer ;; request payload
      _pointer ;; call completion queue
      _pointer ;; notification completion queue
      _pointer ;; tag
      -> _int)))

(define-cstruct _grpc-call-details
  ([method _pointer]
   [method-capacity _size_t]
   [host _pointer]
   [host-capacity _size_t]
   [deadline _gpr-timespec]))


(define grpc-server-request-call
  (get-ffi-obj "grpc_server_request_call" lib-grpc
    (_fun
      _pointer ;; server
      _pointer ;; call
      _grpc-call-details-pointer ;; call_details
      _grpc-metadata-array-pointer ;; request_metadata
      _pointer ;; call completion queue
      _pointer ;; notification completion queue
      _pointer ;; tag
      -> _int)))

(define grpc-call-unref
  (get-ffi-obj "grpc_call_unref" lib-grpc
    (_fun _pointer -> _void)))

(define grpc-server-register-completion-queue
  (get-ffi-obj "grpc_server_register_completion_queue" lib-grpc
    (_fun _pointer _pointer -> _void)))

