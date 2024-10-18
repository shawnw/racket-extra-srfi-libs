#lang typed/racket/base
;; Wrappers for SRFI-64 style test macros to Rackunit

(require (rename-in typed/rackunit [test-true rackunit:test-true] [test-false rackunit:test-false])
         syntax/parse/define (for-syntax syntax/parse racket/base racket/syntax-srcloc))
(provide test-begin test-end test-group
         test test-with-=
         test-equal test-eqv test-eq
         test-approximate
         test-assert test-true test-false
         test-error
         (rename-out
          [test-false test-not]))

(: test-begin : Any -> Void)
(define (test-begin name) (void))
(: test-end : Any * -> Void)
(define (test-end . args) (void))
(define-syntax-parse-rule (test-group name:string test:expr ...+)
  (let () test ...))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-equal? test-expr expected))]
    [(_ name:expr expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-equal? name test-expr expected))]))

(define-syntax (test-with-= stx)
  (syntax-parse stx
    [(_ =?:id expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check =? test-expr expected))]
    [(_ name:string =?:id expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-check name =? test-expr expected))]))
     
(define-syntax (test-equal stx)
  (syntax-parse stx
    [(_ expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-equal? test-expr expected))]
    [(_ name:string expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-equal? name test-expr expected))]))

(define-syntax (test-eqv stx)
  (syntax-parse stx
    [(_ expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-eqv? test-expr expected))]
    [(_ name:string expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-eqv? name expected test-expr))]))

(define-syntax (test-eq stx)
  (syntax-parse stx
    [(_ expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-eq? test-expr expected))]
    [(_ name:string expected:expr test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-eq? name test-expr expected))]))

(define-syntax (test-approximate stx)
  (syntax-parse stx
    [(_ target:expr value:expr max-delta:expr)
     (syntax/loc (syntax-srcloc stx) (check-= target value max-delta))]
    [(_ name:string target:expr value:expr max-delta:expr)
     (syntax/loc (syntax-srcloc stx) (test-= name target value max-delta))]))

(define-syntax (test-assert stx)
  (syntax-parse stx
    [(_ test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-not-false test-expr))]
    [(_ name:string test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-not-false name test-expr))]))

(define-syntax (test-true stx)
  (syntax-parse stx
    [(_ test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-true test-expr))]
    [(_ name:string test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (rackunit:test-true name test-expr))]))
(define-syntax (test-false stx)
  (syntax-parse stx
    [(_ test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-false test-expr))]
    [(_ name:string test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (rackunit:test-false name test-expr))]))

(define-syntax (test-error stx)
  (syntax-parse stx
    [(_ test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-exn exn? (lambda () test-expr)))]
    [(_ #t test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-exn exn? (lambda () test-expr)))]
    [(_ ex?:id test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (check-exn ex? (lambda () test-expr)))]
    [(_ name:string test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-exn name exn? (lambda () test-expr)))]
    [(_ name:string #t test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-exn name exn:fail? (lambda () test-expr)))]
    [(_ name:string ex?:id test-expr:expr)
     (syntax/loc (syntax-srcloc stx) (test-exn name ex? (lambda () test-expr)))]))

