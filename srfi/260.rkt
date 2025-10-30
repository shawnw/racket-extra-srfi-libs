#lang racket/base

;;; SRFI-260 Generated Symbols
;;; custom implementation inspired by the reference one

(require racket/contract racket/random "207.rkt")
(provide
 (contract-out
  [generate-symbol (->* () (string?) symbol?)]))


(define (generate-symbol [name "gs"])
  (string->symbol (string-append name ":" (bytevector->hex-string (crypto-random-bytes 16)))))
