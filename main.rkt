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

(define-cstruct _server-context
  ([call _pointer]
   [payload _pointer]
   [details _grpc-call-details]
   [metadata _grpc-metadata-array]
   [cancelled _int]))


(module+ main
  (define server (grpc-server-create #f))
  (define cq (start-completion-queue))

  (grpc-server-register-completion-queue server cq)

  ;(define method-string "/grpc.testing.TestService/EmptyCall")
  ;(define method (grpc-server-register-method server method-string #f))

  (void (grpc-server-add-http2-port server "localhost:8000"))

  (grpc-server-start server)

  (define ctx (cast (malloc _server-context 'raw) _pointer _server-context-pointer))

  (define call (server-context-call-pointer ctx))
  (set-server-context-call! ctx #f)
  (define payload (server-context-payload-pointer ctx))
  (set-server-context-payload! ctx #f)
  (define details (server-context-details ctx))
  (set-grpc-call-details-method! details #f)
  (set-grpc-call-details-method-capacity! details 0)
  (set-grpc-call-details-host! details #f)
  (set-grpc-call-details-host-capacity! details 0)
  (define deadline (grpc-call-details-deadline details))
  (define metadata (server-context-metadata ctx))
  (grpc-metadata-array-init metadata)

  (define sema (make-semaphore))



  (let loop ()
    (grpc-server-request-call
      server
      call
      details
      metadata
      cq
      cq
      (malloc-immobile-cell sema))
    (sync sema)

    (define ops
      (grpc-op-batch
        #:send-initial-metadata 0 #f
        #:recv-message payload))
    (grpc-call-start-batch (ptr-ref call _pointer) ops (malloc-immobile-cell sema))
    (sync sema)

    (define send-message-slice (gpr-slice-from-copied-buffer #"\x08\x00\x10\x12"))
    (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))

    (define ops2
      (grpc-op-batch
        #:send-message send-message-buffer
        #:send-status-from-server 0 #f 0 #f
        #:recv-close-on-server (server-context-cancelled-pointer ctx)))
    (grpc-call-start-batch (ptr-ref call _pointer) ops2 (malloc-immobile-cell sema))
    (sync sema)




    (loop))


  )




