#lang racket/base
;;; SRFI-112 Environment Inquiry

;;; Original code. Such that it is.

(require racket/contract racket/symbol racket/os)

(define srfi-112/c (-> (or/c (and/c string? immutable?) #f)))
(provide
 (contract-out
  [implementation-name srfi-112/c]
  [implementation-version srfi-112/c]
  [cpu-architecture srfi-112/c]
  [machine-name srfi-112/c]
  [os-name srfi-112/c]
  [os-version srfi-112/c]))

(define (implementation-name) (string->immutable-string "Racket"))
(define (implementation-version) (string->immutable-string (version)))
(define (cpu-architecture) (symbol->immutable-string (system-type 'arch)))
(define (machine-name) (string->immutable-string (gethostname)))
(define (os-name) (symbol->immutable-string (system-type 'os*)))
(define (os-version) #f)
