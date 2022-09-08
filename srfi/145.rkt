#lang racket/base
;;; SRFI-145 Assumptions

(module+ test (require rackunit))
(provide assume)

(define-syntax-rule (assume expression message ...)
  (or expression
      (error "invalid assumption" (quote expression) (list message ...))))

(module+ test
  (test-not-false "successful assume" (assume (= 1 1) "this shouldn't fail"))
  (test-exn "failed assume" exn:fail? (lambda () (assume (= 1 0) "this should fail"))))
