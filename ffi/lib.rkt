#lang racket/base

(require
  racket/format
  racket/runtime-path
  racket/place
  ffi/unsafe
  ffi/cvector
  ffi/unsafe/cvector
  racket/list)
(provide (all-defined-out))

(define-runtime-path lib-grpc-path "./libgrpc_unsecure.so")
(define lib-grpc (ffi-lib lib-grpc-path))



((get-ffi-obj "grpc_init" lib-grpc (_fun -> _void)))


(define _grpc-channel _pointer)
(define _grpc-metadata _pointer)
(define _grpc-completion-queue _pointer)
(define _grpc-call _pointer)
;; TODO make this an enum
(define _grpc-call-error _int)
;; TODO make this an enum
(define _grpc-status-code _int)
(define _size_t _int64)


(define-cstruct _grpc-metadata-array
      ([count _size_t]
       [capacity _size_t]
       [metadata _pointer]))

(define-cstruct _gpr-timespec ([tv_sec _int64] [tv_nsec _int]))

(define-cstruct _grpc-send-initial-metadata
      ([count _size_t]
       [metadata _pointer]))

;; grpc_byte_buffer *send_message;
(define _grpc-send-message _pointer)

(define-cstruct _grpc-send-status-from-server
      ([trailing_metadata_count _size_t]
       [trailing_metadata _pointer]
       [status _grpc-status-code]
       [status_details _pointer]))

;; grpc_metadata_array *recv_initial_metadata;
(define _grpc-recv_initial_metadata _grpc-metadata-array-pointer)
;; grpc_byte_buffer **recv_message;
(define _grpc-recv_message _pointer)
(define-cstruct _grpc-recv_status_on_client
      ([trailing_metadata _pointer]
       [status _pointer]
       [status_details _pointer]
       [status_details_capacity _pointer]))

(define-cstruct _grpc-recv-close-on-server
    ([cancelled _pointer]))

(define _grpc-op-type
  (_enum
    '(send-initial-metadata
      send-message
      send-close-from-client
      send-status-from-server
      recv-initial-metadata
      recv-message
      recv-status-on-client
      recv-close-on-server)))

(define-cstruct _grpc-op ([op _grpc-op-type]
                          [data (_union
                                  _grpc-send-initial-metadata
                                  _grpc-send-message
                                  _grpc-send-status-from-server
                                  _grpc-recv_initial_metadata
                                  _grpc-recv_message
                                  _grpc-recv_status_on_client
                                  _grpc-recv-close-on-server)]))


(define-cstruct _grpc-event
  ([type _int]
   [success _int]
   [value _pointer]))


(define grpc-channel-create
  (get-ffi-obj "grpc_channel_create" lib-grpc (_fun _string _pointer -> _grpc-channel)))
(define grpc-completion-queue-create
  (get-ffi-obj "grpc_completion_queue_create" lib-grpc (_fun -> _grpc-completion-queue)))
(define grpc-channel-create-call
  (get-ffi-obj "grpc_channel_create_call" lib-grpc
    (_fun _grpc-channel _grpc-completion-queue _string _string _gpr-timespec -> _grpc-call)))
(define grpc-call-start-batch
  (get-ffi-obj "grpc_call_start_batch" lib-grpc
    (_fun _grpc-call (ops : _cvector) (_size_t = (cvector-length ops)) _pointer -> _grpc-call-error)))

(define grpc-completion-queue-next
  (get-ffi-obj "grpc_completion_queue_next" lib-grpc
    (_fun _grpc-completion-queue _gpr-timespec -> _grpc-event)))

(define grpc-completion-queue-shutdown

  (get-ffi-obj "grpc_completion_queue_shutdown" lib-grpc
    (_fun _grpc-completion-queue -> _void)))


(define gpr-now
  (get-ffi-obj "gpr_now" lib-grpc
    (_fun -> _gpr-timespec)))

(define gpr-inf-future
  (get-ffi-obj "gpr_inf_future" lib-grpc _gpr-timespec))




(define grpc-metadata-array-init
  (get-ffi-obj "grpc_metadata_array_init" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))

(define grpc-metadata-array-destroy
  (get-ffi-obj "grpc_metadata_array_destroy" lib-grpc
    (_fun _grpc-metadata-array-pointer -> _void)))


(define grpc-raw-byte-buffer-create
  (get-ffi-obj "grpc_raw_byte_buffer_create" lib-grpc
    (_fun _pointer _int -> _pointer)))

(define grpc-byte-buffer-destroy
  (get-ffi-obj "grpc_byte_buffer_destroy" lib-grpc
    (_fun _pointer -> _void)))

(define-cstruct _grpc-byte-buffer-reader
  ([buffer-in _pointer]
   [buffer-out _pointer]
   [current _uint]))


(define grpc-byte-buffer-reader-init
  (get-ffi-obj "grpc_byte_buffer_reader_init" lib-grpc
    (_fun _pointer _pointer -> _void)))

(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _pointer -> _void)))

(define grpc-byte-buffer-reader-next
  (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
    (_fun _pointer _pointer -> _int)))

(define-cstruct _gpr-slice
  ([refcount _pointer]
   [bytes _pointer]
   [length _size_t]))

(define grpc-slice-from-copied-buffer
  (get-ffi-obj "grpc_slice_from_copied_buffer" lib-grpc
    (_fun (b : _bytes) (_size_t = (bytes-length b)) -> _gpr-slice)))

(define grpc-slice-unref
  (get-ffi-obj "grpc_slice_unref" lib-grpc
    (_fun _gpr-slice -> _void)))

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

