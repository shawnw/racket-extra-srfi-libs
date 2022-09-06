#lang racket/base
;;; SRFI-190  Coroutine generators

(require racket/contract racket/stxparam (only-in "158.rkt" make-coroutine-generator)
         (for-syntax racket/base))
(module+ test (require rackunit))

(provide yield coroutine-generator define-coroutine-generator)

(define-syntax-parameter yield
  (lambda (stx) (raise-syntax-error #f "yield used outside coroutine generator" stx)))

(define-syntax coroutine-generator
  (lambda (stx)
    (syntax-case stx ()
      ((_ . body)
       #'(make-coroutine-generator
          (lambda (%yield)
            (syntax-parameterize ([yield (make-rename-transformer #'%yield)])
              . body)))))))

(define-syntax define-coroutine-generator
  (lambda (stx)
    (syntax-case stx ()
      ((_ name . body) #'(define name (coroutine-generator . body))))))

(module+ test
  (require (only-in "158.rkt" generator->list))
  (define g
    (coroutine-generator
     (do ((i 0 (+ i 1)))
         ((<= 3 i))
       (yield i))))

  (define-coroutine-generator (h n)
    (do ((i 0 (+ i 1)))
        ((<= 3 i))
      (yield i)))

  (test-equal? "coroutine-generator" (generator->list g) '(0 1 2))
  (test-equal? "define-coroutine-generator" (generator->list (h 3)) '(0 1 2))
  )
