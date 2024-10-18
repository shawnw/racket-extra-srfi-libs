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
(module+ test (require "private/testwrappers.rkt"))

(provide curried define-curried)

;;; Tried using this alternative implementation using Racket's curry
;;; procedure, since it has some nice features like accepting keyword
;;; arguments, but it fails a bunch of tests - doesn't handle rest
;;; arguments correctly, and doesn't accept more than X arguments.
#|
(require (only-in racket/function curry)
         (for-syntax syntax/parse/lib/function-header))

(define-syntax-parse-rule (curried fmls:formals exp:expr ...+)
  (curry (lambda fmls exp ...)))

(define-syntax-parse-rule (define-curried head:function-header exp:expr ...+)
  (define head.name (curried head.params exp ...)))
|#


;;; Adapted from syntax/parse/lib/function-header.rkt

(begin-for-syntax
  (define-splicing-syntax-class simple-formals-no-rest
    #:attributes (params)
    (pattern (~seq arg:id ...)
             #:attr params #'(arg ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'params))
             "duplicate argument name"))

  (define-syntax-class simple-formals
    #:attributes (params)
    (pattern (~or* (args:simple-formals-no-rest)
                   (args:simple-formals-no-rest . rest-id:id))
             #:attr params #'((~@ . args.params) (~? rest-id))
             #:fail-when (and (attribute rest-id)
                              (member #'rest-id (syntax->list #'args.params) bound-identifier=?)
                              #'rest-id)
             "duplicate argument identifier"))

  (define-syntax-class simple-header
    #:attributes (name params)
    (pattern ((~or header:simple-header name*:id) . args:simple-formals)
             #:attr params #'((~@ . (~? header.params ())) . args.params)
             #:attr name   #'(~? header.name name*))))

(define-syntax-parse-rule (curried fmls:simple-formals exp:expr ...+)
  (curried-1 fmls (begin exp ...)))

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
                ((args ...) (let ([rest '()]) exp)) ; Without this to match exact args, doesn't work right.
                ((args ... . rest) exp)
                (arg-list (more-args f arg-list)))))
    f))

(define (more-args f current)
  (lambda args (apply f (append current args))))

(define-syntax-parse-rule (define-curried hdr:simple-header exp:expr ...+)
  (define hdr.name (curried hdr.params exp ...)))

(module+ test
  (define-curried (add-curry x y) (+ x y))

  (test-group "Simple currying"
              (test-eqv 5 ((curried (x y) (+ x y)) 2 3))
              (test-eqv 5 (((curried (x y) (+ x y)) 2) 3))
              (test-eqv 5 ((curried (w x y z) (+ w x y z)) 1 1 1 2))
              (test-eqv 5 ((((curried (w x y z) (+ w x y z)) 1) 1) 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1) 1 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1) 1 2))
              (test-eqv 5 (((curried (w x y z) (+ w x y z)) 1 1 1) 2))
              (test-eqv 5 (((((curried (w x y z) (+ w x y z)) 1) 1) 1) 2))
              (test-eqv 5 ((add-curry 2) 3))
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
