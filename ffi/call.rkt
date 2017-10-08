#lang racket/base

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse)

  "base-lib.rkt"
  "byte-buffer.rkt"
  (submod "byte-buffer.rkt" unsafe)
  "channel.rkt"
  (submod "channel.rkt" unsafe)
  "completion-queue.rkt"
  (submod "completion-queue.rkt" unsafe)
  "immobile-pointers.rkt"
  (submod "immobile-pointers.rkt" unsafe)
  "metadata-array.rkt"
  (submod "metadata-array.rkt" unsafe)
  "slice.rkt"
  (submod "slice.rkt" unsafe)
  "timespec.rkt"
  (submod "timespec.rkt" unsafe)
  ffi/unsafe
  ffi/unsafe/cvector
  ffi/unsafe/atomic
  racket/list
  racket/port
  racket/promise
  racket/match
  (rename-in
    racket/contract
    [-> c:->]))

(provide
  grpc-op-batch
  (contract-out
    [grpc-channel-create-call
      (c:-> grpc-channel? (or/c #f grpc-call?) grpc-completion-queue? bytes? gpr-timespec? grpc-call?)]
    [grpc-call-client-send-unary (c:-> grpc-call? bytes? void?)]
    [grpc-call-client-receive-unary (c:-> grpc-call? (promise/c bytes?))]))

(define _grpc-call/ffi _pointer)
(struct grpc-call (pointer cq))

(define grpc-channel-create-call/ffi
  (get-ffi-obj "grpc_channel_create_call" lib-grpc
    (_fun _grpc-channel _grpc-call/ffi _uint32 _grpc-completion-queue
          _grpc-slice/arg _pointer _gpr-timespec _pointer -> _grpc-call/ffi)))

(define (grpc-channel-create-call channel parent cq method deadline)
  (call-as-atomic
    (lambda ()
      (grpc-call
        (grpc-channel-create-call/ffi
          channel (and parent (grpc-call-pointer parent)) #xFF cq method #f deadline #f)
        cq))))

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
    (_fun _grpc-call/ffi (ops : _cvector) (_size = (cvector-length ops))
          _grpc-completion-queue-tag _pointer -> _grpc-call-error)))
(define (grpc-call-start-batch call ops callback)
  (match-define (grpc-call call-ptr cq) call)
  (define-values (tag evt) (make-grpc-completion-queue-tag cq callback))
  (define status (grpc-call-start-batch/ffi call-ptr ops tag #f))
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


(define-cstruct _grpc-send-initial-metadata-maybe-compression-level
  ([is-set _uint8]
   [level _int]))

(define-cstruct _grpc-send-initial-metadata
  ([count _size]
   [metadata _pointer]
   [maybe-compression _grpc-send-initial-metadata-maybe-compression-level]))

;; grpc_byte_buffer *send_message
(define _grpc-send-message _grpc-byte-buffer)

(define-cstruct _grpc-send-status-from-server
  ([trailing-metadata-count _size]
   [trailing-metadata _pointer]
   [status _grpc-status-code]
   ;; This should be a pointer to a grpc-slice
   [status-details _pointer]))

;; grpc_metadata_array *recv-initial-metadata;
(define _grpc-recv-initial-metadata _immobile-grpc-metadata-array)
;; grpc_byte_buffer **recv-message;
(define _grpc-recv-message _immobile-indirect-grpc-byte-buffer)
(define-cstruct _grpc-recv-status-on-client
  ([trailing-metadata _immobile-grpc-metadata-array]
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

(begin-for-syntax
  (define-splicing-syntax-class base-op^
    #:attributes (initialize)
    (pattern
      (~seq #:send-initial-metadata count metadata)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-initial-metadata)
            (set-grpc-send-initial-metadata-count!
              (union-ref (grpc-op-data op) 0)
              count)
            (set-grpc-send-initial-metadata-metadata!
              (union-ref (grpc-op-data op) 0)
              metadata)))
    (pattern
      (~seq #:send-message message)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-message)
            (union-set! (grpc-op-data op) 1 message)))
    (pattern
      (~seq #:send-status-from-server metadata-count metadata status status-details)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-status-from-server)
            (define data (union-ref (grpc-op-data op) 2))
            (set-grpc-send-status-from-server-trailing-metadata-count! data metadata-count)
            (set-grpc-send-status-from-server-trailing-metadata! data metadata)
            (set-grpc-send-status-from-server-status! data status)
            (set-grpc-send-status-from-server-status-details! data status-details)))
    (pattern
      (~seq #:send-close-from-client)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'send-close-from-client)))
    (pattern
      (~seq #:recv-initial-metadata metadata)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-initial-metadata)
            (union-set! (grpc-op-data op) 3 metadata)))
    (pattern
      (~seq #:recv-message message-ptr)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-message)
            (union-set! (grpc-op-data op) 4 message-ptr)))
    (pattern
      (~seq #:recv-status-on-client recv-status)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-status-on-client)
            (union-set! (grpc-op-data op) 5 recv-status)))
    (pattern
      (~seq #:recv-close-on-server close-ptr)
      #:with initialize
        #'(lambda (op)
            (set-grpc-op-op! op 'recv-close-on-server)
            (set-grpc-recv-close-on-server-cancelled! (union-ref (grpc-op-data op) 6) close-ptr))))

  (define-splicing-syntax-class op^
    #:attributes (test initialize)
    (pattern :base-op^
      #:with test #'#t)
    (pattern
      (~seq #:cond test :base-op^))))


(define-syntax grpc-op-batch
  (syntax-parser
    [(_ ops:op^ ...)
     (define num-ops (length (syntax->list #'(ops ...))))
     (define/with-syntax (tests ...) (generate-temporaries #'(ops.test ...)))
     #`(let ()
         (define tests ops.test) ...
         (define ops-length (count values (list tests ...)))

         (define index 0)
         (define ops-vector (make-cvector _grpc-op ops-length))
         (when tests
            (define op (cvector-ref ops-vector index))
            (set-grpc-op-flags! op 0)
            (set-grpc-op-reserved! op #f)
            (ops.initialize op)
            (set! index (add1 index))) ...
         ops-vector)]))

(define (grpc-call-client-receive-unary call)
  (call-as-atomic
    (lambda ()
      ;; The raw pointers that are passed to grpc-core. These are freed by the parse-callback.
      (define payload-pointer (malloc-immobile-indirect-grpc-byte-buffer))
      (define trailers-pointer (malloc-immobile-grpc-metadata-array))
      (define status-code-pointer (malloc-immobile-int))
      (define status-details-pointer (malloc-immobile-grpc-slice))

      ;; The safe racket values set by the parse-callback
      (define status-code #f)
      (define payload #f)
      (define status-details #f)

      (define (parse-callback success)
        (when success
          (set! status-code (immobile-int-ref status-code-pointer))
          (unless (zero? status-code)
            (set! status-details (immobile-grpc-slice->bytes status-details-pointer))))
        ;; Always consume the possible payload as it cannot be freed seperately.
        (set! payload
          (consume-immobile-indirect-grpc-byte-buffer payload-pointer))
        (free-immobile-int status-code-pointer)
        (free-immobile-grpc-metadata-array trailers-pointer)
        (free-immobile-grpc-slice status-details-pointer))

      (define (read-status)
        (if (zero? status-code)
            (or payload (error 'rpc "No message received"))
            (error 'rpc "Error: ~a ~s" status-code )))

      (define grpc-recv-status
        (make-grpc-recv-status-on-client
          trailers-pointer
          status-code-pointer
          status-details-pointer))
      (define op-batch
        (grpc-op-batch
          #:recv-message payload-pointer
          #:recv-status-on-client grpc-recv-status))

      (define evt (grpc-call-start-batch call op-batch parse-callback))

      (delay/sync
        (unless (sync evt)
          (error 'unary-recv "Error in call-batch"))
        (read-status)))))

(define (grpc-call-client-send-unary call request)
  (define evt
    (call-as-atomic
      (lambda ()
        (define recv-metadata (malloc-immobile-grpc-metadata-array))
        (define buffer (malloc-grpc-byte-buffer request))
        (grpc-call-start-batch call
          (grpc-op-batch
             #:send-initial-metadata 0 #f
             #:send-message buffer
             #:send-close-from-client
             #:recv-initial-metadata recv-metadata)
          (lambda (success)
            (free-immobile-grpc-metadata-array recv-metadata)
            (free-grpc-byte-buffer buffer))))))
  (unless (sync evt)
    (error 'unary-send "Error in call-batch")))
