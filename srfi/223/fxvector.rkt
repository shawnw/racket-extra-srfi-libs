#lang racket/base
;;; SRFI-223 specialization for fxvectors

(require "../223.rkt" racket/contract racket/fixnum)
(module+ test (require rackunit))

(provide
 (contract-out
  [fxvector-bisect-left (case-> (-> fxvector? fixnum? (-> fixnum? fixnum? any/c) integer?)
                             (-> fxvector? fixnum? (-> fixnum? fixnum? any/c) integer? integer? integer?))]
  [fxvector-bisect-right (case-> (-> fxvector? fixnum? (-> fixnum? fixnum? any/c) integer?)
                              (-> fxvector? fixnum? (-> fixnum? fixnum? any/c) integer? integer? integer?))]))

(define-values (fxvector-bisect-left fxvector-bisect-right)
  (bisection fxvector-ref (lambda (fxv) (values 0 (fxvector-length fxv)))))

(module+ test
  (define fv (fxvector 1 2 3 4))
  (test-equal? "fxvector-bisect-left" (fxvector-bisect-left fv 3 fx<) 2)
  (test-equal? "fxvector-bisect-right" (fxvector-bisect-right fv 3 <) 3))
