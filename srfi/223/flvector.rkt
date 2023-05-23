#lang typed/racket/base
;;; SRFI-223 specialization for flvectors

(require "../223.rkt" racket/flonum)
(module+ test (require typed/rackunit))

(provide flvector-bisect-left flvector-bisect-right)

(define-values (flvector-bisect-left flvector-bisect-right)
  ((inst bisection FlVector Flonum) flvector-ref (lambda ([flv : FlVector]) (values 0 (flvector-length flv)))))

(module+ test
  (define fv (flvector 1.0 2.0 3.0 4.0))
  (test-equal? "flvector-bisect-left" (flvector-bisect-left fv 3.0 fl<) 2)
  (test-equal? "flvector-bisect-right" (flvector-bisect-right fv 3.0 <) 3))
