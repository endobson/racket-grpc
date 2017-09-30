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
    [grpc-byte-buffer? predicate/c]
    [make-grpc-byte-buffer (c:-> bytes? grpc-byte-buffer?)]
    [grpc-byte-buffer->input-port (c:-> grpc-byte-buffer? input-port?)]))

(module* unsafe #f
  (provide
    (contract-out
      [grpc-byte-buffer-pointer (c:-> grpc-byte-buffer? cpointer? )]
      [pointer->grpc-byte-buffer (c:-> cpointer? grpc-byte-buffer?)])))

(struct grpc-byte-buffer (pointer))
(define-fun-syntax _grpc-byte-buffer
  (syntax-id-rules (_grpc-byte-buffer)
    [_grpc-byte-buffer (type: _pointer pre: (x => (grpc-byte-buffer-pointer x)))]))

(define grpc-byte-buffer-destroy
  (get-ffi-obj "grpc_byte_buffer_destroy" lib-grpc
    (_fun _pointer -> _void)))
(define grpc-raw-byte-buffer-create/ffi
  (get-ffi-obj "grpc_raw_byte_buffer_create" lib-grpc
    (_fun _grpc-slice-pointer _int -> _pointer)))
(define make-grpc-byte-buffer
  (let ([raw
          ((allocator grpc-byte-buffer-destroy)
           grpc-raw-byte-buffer-create/ffi)])
    (lambda (bytes) (grpc-byte-buffer (raw (grpc-slice-from-copied-buffer bytes) 1)))))

(define (pointer->grpc-byte-buffer pointer)
  (register-finalizer pointer grpc-byte-buffer-destroy)
  (grpc-byte-buffer pointer))

(define-cstruct _grpc-byte-buffer-reader/ffi
  ([buffer-in _pointer]
   [buffer-out _pointer]
   [current _uint]))

(define (make-grpc-byte-buffer-reader byte-buffer)
  (define reader (make-grpc-byte-buffer-reader/ffi #f #f 0))
  (grpc-byte-buffer-reader-init reader byte-buffer)
  reader)

(define grpc-byte-buffer-reader-destroy
  (get-ffi-obj "grpc_byte_buffer_reader_destroy" lib-grpc
    (_fun _grpc-byte-buffer-reader/ffi-pointer -> _void)))

(define grpc-byte-buffer-reader-init
  ((allocator grpc-byte-buffer-reader-destroy)
   (get-ffi-obj "grpc_byte_buffer_reader_init" lib-grpc
     (_fun _grpc-byte-buffer-reader/ffi-pointer _grpc-byte-buffer -> _void))))


(define grpc-byte-buffer-reader-next
  (let ([raw ((allocator grpc-slice-unref)
              (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
                (_fun _grpc-byte-buffer-reader/ffi-pointer (slice : (_ptr o _grpc-slice/ffi))
                      -> (out : _bool)
                      -> (and out slice))))])
    (lambda (buffer) (let ([v (raw buffer)]) (and v (grpc-slice v))))))

;; TODO(endobson) make this thread safe
(define (grpc-byte-buffer->input-port buffer)
  (define reader (make-grpc-byte-buffer-reader buffer))
  (define location-in-slice 0)
  (define slice #f)
  (define (read-next-slice!)
    (set! slice (grpc-byte-buffer-reader-next reader))
    (set! location-in-slice 0))
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

