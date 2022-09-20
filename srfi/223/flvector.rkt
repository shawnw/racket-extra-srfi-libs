#lang racket/base
;;; SRFI-223 specialization for flvectors

(require "../223.rkt" racket/contract racket/flonum)
(module+ test (require rackunit))

(provide
 (contract-out
  [flvector-bisect-left (case-> (-> flvector? flonum? (-> flonum? flonum? any/c) integer?)
                             (-> flvector? flonum? (-> flonum? flonum? any/c) integer? integer? integer?))]
  [flvector-bisect-right (case-> (-> flvector? flonum? (-> flonum? flonum? any/c) integer?)
                              (-> flvector? flonum? (-> flonum? flonum? any/c) integer? integer? integer?))]))

(define-values (flvector-bisect-left flvector-bisect-right)
  (bisection flvector-ref (lambda (flv) (values 0 (flvector-length flv)))))

(module+ test
  (define fv (flvector 1.0 2.0 3.0 4.0))
  (test-equal? "flvector-bisect-left" (flvector-bisect-left fv 3.0 fl<) 2)
  (test-equal? "flvector-bisect-right" (flvector-bisect-right fv 3.0 <) 3))
