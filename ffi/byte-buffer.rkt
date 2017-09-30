#lang racket/base

(require
  "base-lib.rkt"
  "slice.rkt"
  (submod "slice.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/alloc
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [_grpc-byte-buffer ctype?]
    [grpc-byte-buffer? (c:-> any/c boolean?)]
    [make-grpc-byte-buffer (c:-> bytes? grpc-byte-buffer?)]
    [grpc-byte-buffer-destroy (c:-> grpc-byte-buffer? void?)]
    [grpc-byte-buffer->input-port (c:-> grpc-byte-buffer? input-port?)]))

(define _grpc-byte-buffer _pointer)
(define grpc-byte-buffer? cpointer?)


(define grpc-raw-byte-buffer-create/ffi
  (get-ffi-obj "grpc_raw_byte_buffer_create" lib-grpc
    (_fun _grpc-slice-pointer _int -> _pointer)))
(define (make-grpc-byte-buffer bytes)
  (grpc-raw-byte-buffer-create/ffi
    (grpc-slice-from-copied-buffer bytes)
    1))

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
  (let ([raw ((allocator grpc-slice-unref)
              (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
                (_fun _grpc-byte-buffer-reader-pointer (slice : (_ptr o _grpc-slice/ffi))
                      -> (out : _bool)
                      -> (and out slice))))])
    (lambda (buffer) (let ([v (raw buffer)]) (and v (grpc-slice v))))))

(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _grpc-byte-buffer-reader-pointer -> _void)))

;; TODO(endobson) add an allocator to ensure correct cleanup
;; TODO(endobson) make this thread safe
(define (grpc-byte-buffer->input-port buffer)
  (define reader (ptr-ref (malloc _grpc-byte-buffer-reader) _grpc-byte-buffer-reader))
  (grpc-byte-buffer-reader-init reader buffer)
  (define location-in-slice 0)
  (define slice #f)
  (define (read-next-slice!)
    (set! slice (grpc-byte-buffer-reader-next reader))
    (set! location-in-slice 0)
    (unless slice
      (grpc-byte-buffer-reader-destroy reader)
      (grpc-byte-buffer-destroy buffer)))
  (read-next-slice!)

  (make-input-port
    'grpc-buffer
    (lambda (bytes)
      (if slice
          (let ()
            (define bytes-read (grpc-slice->bytes! bytes 0 slice location-in-slice))
            (set! location-in-slice (+ location-in-slice bytes-read))
            (when (= location-in-slice (grpc-slice-length slice))
              (read-next-slice!))
            bytes-read)
          eof))
    #f
    (lambda ()
      (void))))

