#lang racket/base

(require
  "base-lib.rkt"
  "slice.rkt"
  (submod "slice.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/alloc
  ffi/unsafe/atomic
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  (contract-out
    [grpc-byte-buffer? predicate/c]
    [make-grpc-byte-buffer (c:-> bytes? grpc-byte-buffer?)]
    [grpc-byte-buffer->input-port (c:-> grpc-byte-buffer? input-port?)]
    [immobile-indirect-grpc-byte-buffer? predicate/c]
    [make-immobile-indirect-grpc-byte-buffer (c:-> immobile-indirect-grpc-byte-buffer?)]
    [immobile-indirect-grpc-byte-buffer-ref
      (c:-> immobile-indirect-grpc-byte-buffer?  grpc-byte-buffer?)]))

(module* unsafe #f
  (provide
    (contract-out
      [_grpc-byte-buffer ctype?]
      [_immobile-indirect-grpc-byte-buffer ctype?])))

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
(define make-grpc-byte-buffer
  (let ([raw
          ((allocator grpc-byte-buffer-destroy)
           grpc-raw-byte-buffer-create/ffi)])
    (lambda (bytes) (grpc-byte-buffer (raw bytes)))))

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
  (let ([raw (get-ffi-obj "grpc_byte_buffer_reader_next" lib-grpc
               (_fun _grpc-byte-buffer-reader/ffi-pointer
                     (slice : _grpc-slice-pointer/output-arg)
                     -> (success : _bool)
                     -> (and success slice)))])
    (lambda (reader)
      (call-as-atomic (lambda () (raw reader))))))

;; TODO(endobson) make this thread safe
(define (grpc-byte-buffer->input-port buffer)
  (define reader (make-grpc-byte-buffer-reader buffer))
  (define location 0)
  (define local-bytes #f)
  (define (read-next!)
    (set! local-bytes (grpc-byte-buffer-reader-next reader))
    (set! location 0))
  (read-next!)

  (make-input-port
    'grpc-buffer
    (lambda (bytes)
      (if local-bytes
          (let ()
            (define new-location
              (min (bytes-length local-bytes)
                   (+ location (bytes-length bytes))))
            (bytes-copy! bytes 0 local-bytes location new-location)
            (define bytes-read (- new-location location))
            (set! location new-location)
            (when (= location (bytes-length local-bytes))
              (read-next!))
            bytes-read)
          eof))
    #f
    (lambda ()
      (void))))

;; This holds pointer to a possibly null byte-buffer pointer.
(struct immobile-indirect-grpc-byte-buffer (pointer owned-box))

(define _immobile-indirect-grpc-byte-buffer
  (make-ctype _pointer
    immobile-indirect-grpc-byte-buffer-pointer
    (lambda (x) (error '_immobile-indirect-grpc-byte-buffer "Cannot make values"))))

(define (make-immobile-indirect-grpc-byte-buffer)
  (define p (malloc _pointer 'atomic-interior))
  (define owned-box (box #t))
  (register-finalizer p
    (lambda (p)
      (call-as-atomic
        (lambda ()
          (when (unbox owned-box)
            (define byte-buffer (ptr-ref p _pointer))
            (when byte-buffer
              (grpc-byte-buffer-destroy byte-buffer)))))))
  (immobile-indirect-grpc-byte-buffer p owned-box))

(define (immobile-indirect-grpc-byte-buffer-ref imm)
  (define v
    (call-as-atomic
      (lambda ()
        (define box (immobile-indirect-grpc-byte-buffer-owned-box imm))
        (if (unbox box)
            (let ()
              (set-box! box #f)
              (define p (ptr-ref (immobile-indirect-grpc-byte-buffer-pointer imm) _pointer))
              (and p
                   (let ()
                     (register-finalizer p grpc-byte-buffer-destroy)
                     (grpc-byte-buffer p))))
            'double-ref))))
  (if (eq? v 'double-ref)
      (error 'immobile-indirect-grpc-byte-buffer-ref "Cannot extract the buffer twice.")
      v))
