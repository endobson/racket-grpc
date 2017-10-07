#lang racket/base

(require
  "base-lib.rkt"
  "slice.rkt"
  (submod "slice.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/atomic
  racket/bytes
  racket/list
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-byte-buffer? predicate/c]
    [immobile-indirect-grpc-byte-buffer? predicate/c]))

(module* unsafe #f
  (provide
    (contract-out
      [_grpc-byte-buffer ctype?]
      [_immobile-indirect-grpc-byte-buffer ctype?]
      [malloc-grpc-byte-buffer (c:-> bytes? grpc-byte-buffer?)]
      [free-grpc-byte-buffer (c:-> grpc-byte-buffer? void)]
      [malloc-immobile-indirect-grpc-byte-buffer (c:-> immobile-indirect-grpc-byte-buffer?)]
      [consume-immobile-indirect-grpc-byte-buffer
        (c:-> immobile-indirect-grpc-byte-buffer? (or/c bytes? #f))])))

(struct grpc-byte-buffer (pointer))
(define _grpc-byte-buffer
  (make-ctype _pointer
    grpc-byte-buffer-pointer
    (lambda (x) (error '_grpc-send-message "Cannot make values"))))

(define grpc-byte-buffer-destroy
  (get-ffi-obj "grpc_byte_buffer_destroy" lib-grpc
    (_fun _pointer -> _void)))
(define grpc-raw-byte-buffer-create/ffi
  (get-ffi-obj "grpc_raw_byte_buffer_create" lib-grpc
    (_fun _grpc-slice-pointer/arg (_int = 1) -> _pointer)))
(define (malloc-grpc-byte-buffer bytes)
  (grpc-byte-buffer (grpc-raw-byte-buffer-create/ffi bytes)))
(define (free-grpc-byte-buffer buffer)
  (grpc-byte-buffer-destroy (grpc-byte-buffer-pointer buffer)))


(define-cstruct _grpc-byte-buffer-reader/ffi
  ([buffer-in _pointer]
   [buffer-out _pointer]
   [current _uint]))
(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _grpc-byte-buffer-reader/ffi-pointer -> _void)))
(define grpc-byte-buffer-reader-init
  (get-ffi-obj "grpc_byte_buffer_reader_init" lib-grpc
    (_fun _grpc-byte-buffer-reader/ffi-pointer _pointer -> _void)))

(define grpc-byte-buffer-reader-next
  (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
    (_fun _grpc-byte-buffer-reader/ffi-pointer
          (slice : _grpc-slice-pointer/output-arg)
          -> (success : _bool)
          -> (and success slice))))

(define (grpc-byte-buffer->bytes buffer-ptr)
  (define reader (make-grpc-byte-buffer-reader/ffi #f #f 0))
  (grpc-byte-buffer-reader-init reader buffer-ptr)
  (let loop ([acc empty])
    (define next-bytes (grpc-byte-buffer-reader-next reader))
    (if next-bytes
        (loop (cons next-bytes acc))
        (begin
          (grpc-byte-buffer-reader-destroy reader)
          (bytes-append* (reverse acc))))))

;; This holds pointer to a possibly null byte-buffer pointer.
(struct immobile-indirect-grpc-byte-buffer (pointer))

(define _immobile-indirect-grpc-byte-buffer
  (make-ctype _pointer
    immobile-indirect-grpc-byte-buffer-pointer
    (lambda (x) (error '_immobile-indirect-grpc-byte-buffer "Cannot make values"))))

(define (malloc-immobile-indirect-grpc-byte-buffer)
  (define p (malloc _pointer 'raw))
  (ptr-set! p _pointer #f)
  (immobile-indirect-grpc-byte-buffer p))

(define (consume-immobile-indirect-grpc-byte-buffer imm)
  (define buffer-ptr (ptr-ref (immobile-indirect-grpc-byte-buffer-pointer imm) _pointer))
  (and buffer-ptr (grpc-byte-buffer->bytes buffer-ptr)))
