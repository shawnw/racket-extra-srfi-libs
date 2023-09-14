#lang racket/base

(require (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(provide (rename-out [srfi-87:case case]))

(define-syntax (srfi-87:case stx)
  (syntax-parse stx
    #:literals (else =>)
    [(_ (key ...+) clauses ...)
     #'(let ([evaluated-key (key ...)])
         (srfi-87:case evaluated-key clauses ...))]
    [(_ key:expr)
     #'(void)]
    [(_ key:expr (else => e:expr))
     #'(e key)]
    [(_ key:expr (else e:expr ...+))
     #'((lambda () e ...))]
    [(_ key:expr ((val:expr) => e:expr) clauses ...) ; Special case a single-element list
     #'(if (equal? key 'val)
           (e key)
           (srfi-87:case key clauses ...))]
    [(_ key:expr ((val:expr ...+) => e:expr) clauses ...)
     #'(if (member key '(val ...))
           (e key)
           (srfi-87:case key clauses ...))]
    [(_ key:expr ((val:expr) e:expr ...+) clauses ...) ; Special case a single-element list
     #'(if (equal? key 'val)
           ((lambda () e ...))
           (srfi-87:case key clauses ...))]
    [(_ key:expr ((val:expr ...+) e:expr ...) clauses ...)
     #'(if (member key '(val ...))
           ((lambda () e ...))
           (srfi-87:case key clauses ...))]))

(module+ test
  (check-equal? (srfi-87:case 4 ((1 2 3) 'foo) (else => number->string)) "4")
  (check-equal? (srfi-87:case 3 ((1 2 3) => number->string) (else 'bad)) "3")
  (check-equal? (srfi-87:case 3 ((3) => number->string) (else 'bad)) "3")
  (check-equal? (srfi-87:case 3 ((1 2 3) 'good) (else 'bad)) 'good)

  )
