#lang racket/base

(require
  "lib.rkt"
  "place.rkt"
  "server-call.rkt"
  "timestamp.rkt"
  "grpc-op-batch.rkt"
  "buffer-reader.rkt"
  "status.rkt"
  racket/port
  racket/async-channel
  racket/match
  ffi/unsafe
  racket/list)

(define-cstruct _server-context
  ([call _pointer]
   [payload _pointer]
   [details _grpc-call-details]
   [metadata _grpc-metadata-array]
   [cancelled _int]))

(struct server-config
        (methods addresses))

(provide start-server server-config)

(define (start-server config)
  (define server (grpc-server-create #f))
  (define cq (start-completion-queue))

  (grpc-server-register-completion-queue server cq)
  (for ([address (server-config-addresses config)])
    (grpc-server-add-http2-port server address))

  (grpc-server-start server)

  (define methods
    (for/hash ([(k v) (in-hash (server-config-methods config))])
      (values
        (string->immutable-string k)
        v)))

  (define ctx (cast (malloc _server-context 'raw) _pointer _server-context-pointer))

  (define call-pointer (server-context-call-pointer ctx))
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

  (define (server-fun input)
    (port->bytes input))

  (let loop ()
    (grpc-server-request-call
      server
      call-pointer
      details
      metadata
      cq
      cq
      (malloc-immobile-cell sema))
    (sync sema)

    (define method (cast (grpc-call-details-method details) _pointer _string))
    (define call (ptr-ref call-pointer _pointer))

    (thread
      (lambda ()
        (define server-call (create-server-call (timestamp 0 0) call method cq))
        (define message (sync (server-call-recv-message-evt server-call)))

        (define output
          (match (hash-ref methods (server-call-method server-call) 'unimplemented)
            ['unimplemented
             ;; TODO(endobson) send back unimplemented status code]
             #""]
            [fun
              (fun message)]))

        (define send-message-slice (gpr-slice-from-copied-buffer output))
        (define send-message-buffer (grpc-raw-byte-buffer-create send-message-slice 1))
        (gpr-slice-unref send-message-slice)

        (server-call-send-initial-metadata server-call (hash))
        (server-call-send-message server-call send-message-buffer)
        (server-call-send-status server-call ok-status (hash))



        (grpc-byte-buffer-destroy send-message-buffer)
        (server-call-wait server-call)
        (grpc-call-destroy call)))

    (loop)))
