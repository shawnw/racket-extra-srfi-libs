#lang racket/base

;;; (C) 2022 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;; Converted to Racket syntax-parse by Shawn Wagner

(require (for-syntax racket/base syntax/parse)
         syntax/parse/define)
(module+ test (require rackunit))

(provide curried define-curried)

;;; Too bad there's not a predefined simple-formals syntax class
(define-syntax-parse-rule (curried formals exp:expr ...+)
  (curried-1 formals (begin exp ...)))

(define-syntax (curried-1 stx)
  (syntax-parse stx
    ((curried-1 () exp:expr) #'exp)
    ((curried-1 (args ...+) exp:expr)
     #'(one-or-more (args ...) exp))
    ((curried-1 (args ...+ . rest:id) exp:expr)
     #'(rest-args (args ...) rest exp))
    ((curried-1 args:id exp:expr) #'(lambda args exp))))

(define-syntax-parse-rule (one-or-more (args:id ...+) exp:expr)
  (letrec
      ((f (case-lambda
            (() f)       ; app. to no args -> original function
            ((args ...) exp)
            ((args ... . rest)
             (apply (f args ...) rest))
            (arg-list (more-args f arg-list)))))
    f))

(define-syntax-parse-rule (rest-args (args:id ...+) rest:id exp:expr)
  (letrec ((f (case-lambda
                (() f)
                ((args ...) (let ([rest '()]) exp))
                ((args ... . rest) exp)
                (arg-list (more-args f arg-list)))))
    f))

(define (more-args f current)
  (lambda args (apply f (append current args))))

(define-syntax (define-curried stx)
  (syntax-parse stx
    ((define-curried (name:id args:id ...) exp:expr ...+)
     #'(define name
         (curried-1 (args ...) (begin exp ...))))
    ((define-curried (name:id args:id ...+ . rest:id) exp:expr ...+)
     #'(define name
         (curried-1 (args ... . rest) (begin exp ...))))
    ((define-curried (name:id . rest:id) exp:expr ...+)
     #'(define name
         (curried-1 rest (begin exp ...))))))


(module+ test
  (define-syntax-parse-rule (test-group name:string test:expr ...)
    (begin test ...))
  (define-syntax-parse-rule (test-eqv expected:expr test-expr:expr)
    (check-eqv? test-expr expected))
  (define-syntax-parse-rule (test-equal expected:expr test-expr:expr)
    (check-equal? test-expr expected))

  (test-group "Simple currying"
              (test-eqv 5 ((curried (x y) (+ x y)) 2 3))
              (test-eqv 5 (((curried (x y) (+ x y)) 2) 3))
              (test-eqv 5 ((curried (w x y z) (+ w x y z)) 1 1 1 2))
              (test-eqv 5 ((((curried (w x y z) (+ w x y z)) 1) 1) 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1) 1 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1) 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1 1) 2))
              (test-eqv 5 (((((curried (w x y z) (+ w x y z)) 1) 1) 1) 2))
              )

  (test-group "Variadic"
              (test-equal '(3 (3 4))
                          ((curried (a b . rest) (list (+ a b) rest)) 1 2 3 4))
              (test-equal
               '(3 (3 4))
               (((curried (a b . more) (list (+ a b) more)) 1) 2 3 4))
              (test-equal '(3 ()) ((curried (a b . rest) (list (+ a b) rest)) 1 2))
              )

  (test-group "Nullary"
              (test-eqv 3 ((curried () (curried (x y) (+ x y))) 1 2))
              (test-eqv 3 (((curried () (curried (x y) (+ x y))) 1) 2))

              ;; "... while these behaviors are decidedly not wrong, they are
              ;;  perhaps mildly unsettling."
              (test-eqv 2
                        ((curried (a)
                                  (curried ()
                                           (curried ()
                                                    (curried (b) b)))) 1 2))
              (test-eqv 4 (((((((((curried (a b c) 4)))))))) 1 2 3))
              )

  (test-group "Extra arguments"
              (test-eqv 20 ((curried (x y) (curried (z) (* z (+ x y)))) 2 3 4))
              (test-eqv 20 (((curried (x y) (curried (z) (* z (+ x y)))) 2) 3 4))
              ))
