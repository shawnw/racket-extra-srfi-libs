#lang racket/base

;;; SRFI-229 Tagged procedures

(require syntax/parse/define "259.rkt"
         (for-syntax syntax/parse/lib/function-header))
(module+ test (require rackunit))
(provide case-lambda/tag lambda/tag procedure/tag? procedure-tag)

(define-procedure-tag srfi229 make-tagged-procedure procedure/tag? procedure-tag)

(define-syntax-parse-rule (lambda/tag tag:expr args:formals body:expr ...+)
  (make-tagged-procedure tag (lambda args body ...)))

(define-syntax-parse-rule (case-lambda/tag tag:expr [args:formals body:expr ...+] ...+)
  (make-tagged-procedure tag (case-lambda [args body ...] ...)))

(module+ test
  (define f
    (lambda/tag 42
                (x)
                (* x x)))
  (define f*
    (lambda/tag 43
                (x)
                (* x x)))
  (check-true (procedure/tag? f))
  (check-eqv? (f 3) 9)
  (check-eqv? (procedure-tag f) 42)
  (check-false (equal? f f*))

  (define g
    (let ((y 10))
      (lambda/tag y ()
                  (set! y (+ y 1))
                  y)))
  (check-eqv? (procedure-tag g) 10)
  (check-eqv? (let ((y 9)) (procedure-tag g)) 10)
  (check-eqv? (g) 11)
  (check-eqv? (procedure-tag g) 10)

  (define h
    (let ((b (box #f)))
      (case-lambda/tag b
                       (() (unbox b))
                       ((val) (set-box! b val) val))))

  (check-eqv? (h 1) 1)
  (check-eqv? (unbox (procedure-tag h)) 1)
  (check-eqv? (h) 1)
  )
