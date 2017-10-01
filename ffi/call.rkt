#lang racket/base

(require
  "base-lib.rkt"
  (submod "byte-buffer.rkt" unsafe)
  "channel.rkt"
  (submod "channel.rkt" unsafe)
  "completion-queue.rkt"
  (submod "completion-queue.rkt" unsafe)
  "immobile-pointers.rkt"
  (submod "immobile-pointers.rkt" unsafe)
  "slice.rkt"
  (submod "slice.rkt" unsafe)
  "timespec.rkt"
  (submod "timespec.rkt" unsafe)
  ffi/unsafe
  ffi/cvector
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-channel-create-call
      (c:-> grpc-channel? grpc-call? grpc-completion-queue? bytes? gpr-timespec? grpc-call?)]
    [_grpc-metadata-array ctype?]
    [_grpc-metadata-array-pointer ctype?]
    [_grpc-op ctype?]
    [set-grpc-op-op! (c:-> grpc-op? grpc-op-type? void?)]
    [set-grpc-op-flags! (c:-> grpc-op? exact-nonnegative-integer? void?)]
    [set-grpc-op-reserved! (c:-> grpc-op? cpointer? void?)]
    [grpc-op-data (c:-> grpc-op? union?)]
    [set-grpc-send-initial-metadata-count!
      (c:-> grpc-send-initial-metadata?  exact-nonnegative-integer? void?)]
    [set-grpc-send-initial-metadata-metadata! (c:-> grpc-send-initial-metadata? cpointer? void?)]
    [set-grpc-send-status-from-server-trailing-metadata-count!
      (c:-> grpc-send-status-from-server? exact-nonnegative-integer? void?)]
    [set-grpc-send-status-from-server-trailing-metadata! (c:-> grpc-send-status-from-server? cpointer? void?)]
    [set-grpc-send-status-from-server-status! (c:-> grpc-send-status-from-server? grpc-status-code? void?)]
    [set-grpc-send-status-from-server-status-details! (c:-> grpc-send-status-from-server? grpc-slice? void?)]
    [make-grpc-recv-status-on-client (c:-> cpointer? immobile-int? immobile-grpc-slice? grpc-recv-status-on-client?)]
    [grpc-call-start-batch (c:-> grpc-call? cvector? any/c evt?)]))

(define _grpc-call _pointer)
(define grpc-call? cpointer?)

(define grpc-channel-create-call/ffi
  (get-ffi-obj "grpc_channel_create_call" lib-grpc
    (_fun _grpc-channel _grpc-call _uint32 _grpc-completion-queue
          _grpc-slice _pointer _gpr-timespec _pointer -> _grpc-call)))

(define (grpc-channel-create-call channel parent cq method deadline)
  (define method-slice (grpc-slice-from-copied-buffer method))
  (grpc-channel-create-call/ffi channel parent #xFF cq method-slice #f deadline #f))

;; TODO make this an enum
(define _grpc-call-error _int)
;; TODO make this an enum
(define _grpc-status-code _int)
(define grpc-status-code? exact-nonnegative-integer?)


(define grpc-call-error-to-string
  (get-ffi-obj "grpc_call_error_to_string" lib-grpc
    (_fun _grpc-call-error -> _string)))

(define grpc-call-start-batch/ffi
  (get-ffi-obj "grpc_call_start_batch" lib-grpc
    (_fun _grpc-call (ops : _cvector) (_size = (cvector-length ops)) _pointer _pointer -> _grpc-call-error)))
(define (grpc-call-start-batch call ops ref)
  (define-values (tag evt) (make-grpc-completion-queue-tag ref))
  (define status (grpc-call-start-batch/ffi call ops tag #f))
  (unless (zero? status)
    (free-immobile-cell tag)
    (error 'grpc-call-start-batch "Error: ~a" (grpc-call-error-to-string status)))
  evt)

;; Batch operations
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

(define grpc-op-type?
  (or/c
    'send-initial-metadata
    'send-message
    'send-close-from-client
    'send-status-from-server
    'recv-initial-metadata
    'recv-message
    'recv-status-on-client
    'recv-close-on-server))

(define-cstruct _grpc-metadata-array
  ([count _size]
   [capacity _size]
   [metadata _pointer]))


(define-cstruct _grpc-send-initial-metadata-maybe-compression-level
  ([is-set _uint8]
   [level _int]))

(define-cstruct _grpc-send-initial-metadata
  ([count _size]
   [metadata _pointer]
   [maybe-compression _grpc-send-initial-metadata-maybe-compression-level]))

;; grpc_byte_buffer *send_message
(define _grpc-send-message
  (make-ctype _pointer
    grpc-byte-buffer-pointer
    (lambda (x) (error '_grpc-send-message "Cannot make values"))))

(define-cstruct _grpc-send-status-from-server
  ([trailing-metadata-count _size]
   [trailing-metadata _pointer]
   [status _grpc-status-code]
   ;; This should be a pointer to a grpc-slice
   [status-details _pointer]))

;; grpc_metadata_array *recv-initial-metadata;
(define _grpc-recv-initial-metadata _grpc-metadata-array-pointer)
;; grpc_byte_buffer **recv-message;
(define _grpc-recv-message _pointer)
(define-cstruct _grpc-recv-status-on-client
  ([trailing-metadata _grpc-metadata-array-pointer]
   [status _immobile-int]
   ;; This should be a pointer to a grpc-slice
   [status-details _immobile-grpc-slice-pointer]))

(define-cstruct _grpc-recv-close-on-server
  ([cancelled _pointer]))

(define _grpc-op-data
  (_union
    _grpc-send-initial-metadata
    _grpc-send-message
    _grpc-send-status-from-server
    _grpc-recv-initial-metadata
    _grpc-recv-message
    _grpc-recv-status-on-client
    _grpc-recv-close-on-server
    (_array _pointer 8)))

(define-cstruct _grpc-op
  ([op _grpc-op-type]
   [flags _uint32]
   [reserved _pointer]
   [data _grpc-op-data]))
