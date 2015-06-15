#lang racket/base

(require
  "lib.rkt"
  "place.rkt"
  "grpc-op-batch.rkt"
  "buffer-reader.rkt"
  racket/format
  racket/port
  racket/place
  ffi/unsafe
  ffi/cvector
  ffi/unsafe/cvector
  racket/list)

(module+ main
  (define server (grpc-server-create #f))
  (define cq (start-completion-queue))

  (grpc-server-register-completion-queue server cq)

  (define method-string "/grpc.testing.TestService/EmptyCall")
  (define method (grpc-server-register-method server method-string #f))

  (void (grpc-server-add-http2-port server "localhost:8000"))

  (grpc-server-start server)

  (define call (malloc-immobile-cell #f))
  (define payload (malloc-immobile-cell #f))
  (define deadline (make-gpr-timespec 0 0))
  (define details (make-grpc-call-details #f 0 #f 0 (make-gpr-timespec 0 0)))
  (define sema (make-semaphore))

  (define request-metadata (make-grpc-metadata-array 0 0 #f))
  (grpc-metadata-array-init request-metadata)


  (let loop ()
    (grpc-server-request-registered-call
      server
      method
      call
      deadline
      request-metadata
      payload
      cq
      cq
      (malloc-immobile-cell sema))
    (sync sema)

    (define send-message-slice (gpr-slice-from-copied-buffer #"\x08\x00\x10\x12"))
    (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))
    (define close-on-server (make-grpc-recv-close-on-server 
                              (malloc _int)))

    (define ops
      (grpc-op-batch
        #:send-initial-metadata 0 #f
        #:send-message send-message-buffer
        #:send-status-from-server 0 #f 0 #f
        #:recv-close-on-server close-on-server))

    (grpc-call-start-batch (ptr-ref call _pointer) ops (malloc-immobile-cell sema))
    (sync sema)




    (loop))


  )




