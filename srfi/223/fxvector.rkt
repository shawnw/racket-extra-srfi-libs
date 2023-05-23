#lang typed/racket/base
;;; SRFI-223 specialization for fxvectors

(require "../223.rkt" racket/fixnum)
(module+ test (require typed/rackunit))

(provide fxvector-bisect-left fxvector-bisect-right)

(define-values (fxvector-bisect-left fxvector-bisect-right)
  ((inst bisection FxVector Fixnum) fxvector-ref (lambda ([fxv : FxVector]) (values 0 (fxvector-length fxv)))))

(module+ test
  (define fv (fxvector 1 2 3 4))
  (test-equal? "fxvector-bisect-left" (fxvector-bisect-left fv 3 fx<) 2)
  (test-equal? "fxvector-bisect-right" (fxvector-bisect-right fv 3 <) 3))
