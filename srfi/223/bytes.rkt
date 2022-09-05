#lang racket/base
;;; SRFI-223 specialization for byte strings

(require "../223.rkt" racket/contract)
(module+ test (require rackunit))

(provide
 (contract-out
  [bytes-bisect-left (case-> (-> bytes? byte? (-> byte? byte? any/c) integer?)
                             (-> bytes? byte? (-> byte? byte? any/c) integer? integer? integer?))]
  [bytes-bisect-right (case-> (-> bytes? byte? (-> byte? byte? any/c) integer?)
                              (-> bytes? byte? (-> byte? byte? any/c) integer? integer? integer?))]))

(define-values (bytes-bisect-left bytes-bisect-right)
  (bisection bytes-ref (lambda (bs) (values 0 (bytes-length bs)))))

(module+ test
  (define bs #"abcd")
  (test-equal? "bytes-bisect-left" (bytes-bisect-left bs (char->integer #\c) <) 2)
  (test-equal? "bytes-bisect-right" (bytes-bisect-right bs (char->integer #\c) <) 3))
