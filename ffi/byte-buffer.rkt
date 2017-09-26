#lang racket/base

(require
  "base-lib.rkt"
  "slice.rkt"
  ffi/unsafe
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-byte-buffer? (c:-> any/c boolean?)]
    [make-grpc-byte-buffer (c:-> bytes? grpc-byte-buffer?)]
    [grpc-byte-buffer-destroy (c:-> grpc-byte-buffer? void?)]
    [grpc-byte-buffer->input-port (c:-> grpc-byte-buffer? input-port?)]))

(define _grpc-byte-buffer _pointer)
(define grpc-byte-buffer? cpointer?)


(define grpc-raw-byte-buffer-create/ffi
  (get-ffi-obj "grpc_raw_byte_buffer_create" lib-grpc
    (_fun _pointer _int -> _pointer)))
(define (make-grpc-byte-buffer bytes)
  (define slice (grpc-slice-from-copied-buffer bytes))
  (define buffer (grpc-raw-byte-buffer-create/ffi slice 1))
  (grpc-slice-unref slice)
  buffer)

(define grpc-byte-buffer-destroy
  (get-ffi-obj "grpc_byte_buffer_destroy" lib-grpc
    (_fun _grpc-byte-buffer -> _void)))

(define-cstruct _grpc-byte-buffer-reader
  ([buffer-in _pointer]
   [buffer-out _pointer]
   [current _uint]))

(define grpc-byte-buffer-reader-init
  (get-ffi-obj "grpc_byte_buffer_reader_init" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer _grpc-byte-buffer -> _void)))

(define grpc-byte-buffer-reader-next
  (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer _grpc-slice-pointer -> _bool)))

(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer -> _void)))

;; TODO(endobson) add an allocator to ensure correct cleanup
;; TODO(endobson) make this thread safe
(define (grpc-byte-buffer->input-port buffer)
  (define reader (ptr-ref (malloc _grpc-byte-buffer-reader) _grpc-byte-buffer-reader))
  (grpc-byte-buffer-reader-init reader buffer)
  (define location-in-slice 0)
  (define slice (make-empty-grpc-slice))
  (define more-buffer #t)
  (define (read-next-slice!)
    (set! more-buffer (grpc-byte-buffer-reader-next reader slice))
    (set! location-in-slice 0)
    (unless more-buffer
      (grpc-byte-buffer-reader-destroy reader)
      (grpc-byte-buffer-destroy buffer)))
  (read-next-slice!)

  (make-input-port
    'grpc-buffer
    (lambda (bytes)
      (if more-buffer
          (let ()
            (define bytes-read (grpc-slice->bytes! bytes 0 slice location-in-slice))
            (set! location-in-slice (+ location-in-slice bytes-read))
            (when (= location-in-slice (grpc-slice-length slice))
              (grpc-slice-unref slice)
              (read-next-slice!))
            bytes-read)
          eof))
    #f
    (lambda ()
      (void))))

