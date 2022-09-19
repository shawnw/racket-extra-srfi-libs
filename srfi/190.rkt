#lang racket/base
;;; SRFI-190  Coroutine generators

(require racket/contract syntax/parse syntax/parse/define racket/stxparam
         (only-in "158.rkt" make-coroutine-generator)
         (for-syntax racket/base))
(module+ test (require rackunit))

(provide yield coroutine-generator define-coroutine-generator)

(define-syntax-parameter yield
  (lambda (stx) (raise-syntax-error #f "yield used outside coroutine generator" stx)))

(define-syntax-parse-rule (coroutine-generator body:expr ...+)
  (make-coroutine-generator
   (lambda (%yield)
     (syntax-parameterize ([yield (make-rename-transformer #'%yield)])
       body ...))))

(define-syntax (define-coroutine-generator stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...) body:expr ...+)
     #'(define (name arg ...) (coroutine-generator body ...))]
    [(_ name:id body:expr ...+)
     #'(define name (coroutine-generator body ...))]))

(module+ test
  (require (only-in "158.rkt" generator->list))
  (define g
    (coroutine-generator
     (do ((i 0 (+ i 1)))
         ((<= 3 i))
       (yield i))))

  (define-coroutine-generator (h n)
    (do ((i 0 (+ i 1)))
        ((<= n i))
      (yield i)))

  (define-coroutine-generator hh
    (do ((i 0 (+ i 1)))
      ((<= 3 i))
      (yield i)))

  (test-equal? "coroutine-generator" (generator->list g) '(0 1 2))
  (test-equal? "define-coroutine-generator-func" (generator->list (h 3)) '(0 1 2))
  (test-equal? "define-coroutine-generator-direct" (generator->list hh) '(0 1 2)))
