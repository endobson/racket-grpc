#lang racket/base

(require
  "lib.rkt"
  ffi/unsafe)

(provide grpc-buffer->input-port)


;; TODO(endobson) add an allocator to ensure correct cleanup
(define (grpc-buffer->input-port buffer)
  (define reader (malloc _grpc-byte-buffer-reader))
  (grpc-byte-buffer-reader-init reader buffer)
  (define location-in-slice 0)
  (define slice (make-gpr-slice #f #f 0))
  (define more-buffer 1)
  (define (read-next-slice!)
    (set! more-buffer (grpc-byte-buffer-reader-next reader slice))
    (set! location-in-slice 0)
    (when (zero? more-buffer)
      (grpc-byte-buffer-reader-destroy reader)))

  (read-next-slice!)

  (make-input-port
    'grpc-buffer
    (lambda (bytes)
      (cond
        [(zero? more-buffer) eof]
        [else
          (define refcount (gpr-slice-refcount slice))
          (define slice-length
            (if refcount
                (gpr-slice-length slice)
                (ptr-ref slice _uint8 'abs 8)))
          (define slice-start
            (if refcount
                (gpr-slice-bytes slice)
                (ptr-add slice 9)))
          (define amount-to-read
            (min (bytes-length bytes)
                 (- slice-length location-in-slice)))
          (memcpy bytes 0 slice-start location-in-slice amount-to-read)
          (set! location-in-slice (+ location-in-slice amount-to-read))
          (when (= location-in-slice slice-length)
            (gpr-slice-unref slice)
            (read-next-slice!))
          amount-to-read]))
    #f
    (lambda ()
      (void))))


