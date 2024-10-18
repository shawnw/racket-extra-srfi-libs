#lang racket/base

;;; SRFI-235 Combinators
;;; Original implementation.

(require racket/contract
         (rename-in racket/function
                    [conjoin rkt:conjoin]
                    [disjoin rkt:disjoin]
                    [const* constantly]
                    [negate complement])
         (rename-in racket/list [group-by rkt:group-by])
         (only-in srfi/1 map-in-order))
(module+ test (require "private/testwrappers.rkt"))

(provide
 constantly
 complement
 apply-chain
 left-section right-section ; Not sure how to make a contract that takes arbitrary keyword arguments for these
 (contract-out
  [flip (-> procedure? procedure?)]
  [swap (-> procedure? procedure?)]
  [on-left (-> (-> any/c any) (-> any/c any/c any))]
  [on-right (-> (-> any/c any) (-> any/c any/c any))]
  [conjoin (-> (-> any/c any/c) ... (-> any/c ... any/c))]
  [disjoin (-> (-> any/c any/c) ... (-> any/c ... any/c))]
  [each-of (-> procedure? ... procedure?)]
  [all-of (-> procedure? (-> any/c ... any/c))]
  [any-of (-> procedure? (-> any/c ... any/c))]
  [on (-> procedure? procedure? (-> any/c any/c ... any/c))]
  ;[left-section (->* (procedure?) #:rest (listof any/c) procedure?)]
  ;[right-section (->* (procedure?) #:rest (listof any/c) procedure?)]
  [arguments-drop (-> procedure? exact-nonnegative-integer? procedure?)]
  [arguments-drop-right (-> procedure? exact-nonnegative-integer? procedure?)]
  [arguments-take (-> procedure? exact-nonnegative-integer? procedure?)]
  [arguments-take-right (-> procedure? exact-nonnegative-integer? procedure?)]
  [group-by (->* ((-> any/c any/c)) ((-> any/c any/c any/c)) (-> list? (listof list?)))]
  [begin-procedure (-> (-> any) ... any)]
  [if-procedure (-> any/c (-> any) (-> any) any)]
  [when-procedure (-> any/c (-> any) ... void?)]
  [unless-procedure (-> any/c (-> any) ... void?)]
  [value-procedure (-> any/c (-> any/c any) (-> any) any)]
  [case-procedure (-> any/c (listof (cons/c any/c (-> any))) (-> any) any)]
  [and-procedure (-> (-> any/c) ... any/c)]
  [or-procedure (-> (-> any/c) ... any/c)]
  [eager-and-procedure (-> (-> any/c) ... any/c)]
  [eager-or-procedure (-> (-> any/c) ... any/c)]
  [funcall-procedure (-> (-> any) any)]
  [loop-procedure (-> (-> any) void?)]
  [while-procedure (-> (-> any/c) void?)]
  [until-procedure (-> (-> any/c) void?)]
  [always (unconstrained-domain-> #t)]
  [never (unconstrained-domain-> #f)]
  [boolean (-> any/c boolean?)]
  ))

(define apply-chain compose)

(define (no-keyword-args? proc)
  (let-values ([(required optional) (procedure-keywords proc)])
    (and (eq? required '()) (eq? optional '()))))

(define (exactly-two-args? proc)
  (and (eqv? (procedure-arity proc) 2)
       (no-keyword-args? proc)))

(define (flip proc)
  (cond
    ((exactly-two-args? proc)
     (lambda (arg1 arg2) (proc arg2 arg1)))
    ((no-keyword-args? proc)
     (procedure-reduce-arity-mask
      (lambda args
        (apply proc (reverse args)))
      (procedure-arity-mask proc)
      (string->symbol (format "(flip ~A)" (object-name proc)))))
    (else
     (make-keyword-procedure
      (lambda (kw-lst kw-lst-vals . args)
        (keyword-apply proc kw-lst kw-lst-vals (reverse args)))))))

(define (swap proc)
  (unless (procedure-arity-includes? proc 2 #t)
    (raise-argument-error 'swap "(procedure-arity-includes/c 2)" proc))
  (cond
    ((exactly-two-args? proc)
     (lambda (arg1 arg2) (proc arg2 arg1)))
    ((no-keyword-args? proc)
     (procedure-reduce-arity-mask
      (lambda (arg1 arg2 . args)
        (apply proc arg2 arg1 args))
      (bitwise-and (procedure-arity-mask proc) (bitwise-not #b11))
      (string->symbol (format "(swap ~A)" (object-name proc)))))
    (else
     (make-keyword-procedure
      (lambda (kw-lst kw-val-lst arg1 arg2 . args)
        (keyword-apply proc kw-lst kw-val-lst arg2 arg1 args))))))

(define (on-left proc)
  (lambda (a b) (proc a)))

(define (on-right proc)
  (lambda (a b) (proc b)))

(define (conjoin . preds)
  (if (null? preds)
      (lambda args #t)
      (let ([all-preds? (apply rkt:conjoin preds)])
        (lambda args
          (andmap all-preds? args)))))

(define (disjoin . preds)
  (if (null? preds)
      (lambda args #f)
      (let ([all-preds? (apply rkt:disjoin preds)])
        (lambda args
          (if (null? args)
              #t
              (ormap all-preds? args))))))

(define (each-of . procs)
  (make-keyword-procedure
   (lambda (kw-lst kw-lst-vals . args)
     (for-each (lambda (proc) (keyword-apply proc kw-lst kw-lst-vals args)) procs))))

(define (all-of proc)
  (lambda (list) (andmap proc list)))

(define (any-of proc)
  (lambda (list) (ormap proc list)))

(define (on reducer mapper)
  (lambda (obj1 . objs)
    (apply reducer (map mapper (cons obj1 objs)))))

(define left-section
  (make-keyword-procedure
   (lambda (kw-lst1 kw-lst-vals1 proc . args)
     (make-keyword-procedure
      (lambda (kw-lst2 kw-lst-vals2 . objs)
        (keyword-apply proc (append kw-lst1 kw-lst2) (append kw-lst-vals1 kw-lst-vals2) (append args objs)))
      (lambda objs
        (keyword-apply proc kw-lst1 kw-lst-vals1 (append args objs)))))
   (lambda (proc . args)
     (make-keyword-procedure
      (lambda (kw-lst kw-lst-vals . objs)
        (keyword-apply proc kw-lst kw-lst-vals (append args objs)))))))

(define right-section
  (make-keyword-procedure
   (lambda (kw-lst1 kw-lst-vals1 proc . args)
     (let ([args (reverse args)])
       (make-keyword-procedure
        (lambda (kw-lst2 kw-lst-vals2 . objs)
          (keyword-apply proc (append kw-lst1 kw-lst2) (append kw-lst-vals1 kw-lst-vals2) (append objs args)))
        (lambda objs
          (keyword-apply proc kw-lst1 kw-lst-vals1 (append objs args))))))
   (lambda (proc . args)
     (let ([args (reverse args)])
       (make-keyword-procedure
        (lambda (kw-lst kw-lst-vals . objs)
          (keyword-apply proc kw-lst kw-lst-vals (append objs args))))))))

(define (arguments-drop proc n)
  (if (no-keyword-args? proc)
      (procedure-reduce-arity
       (lambda args
         (apply proc (drop args n)))
       (make-arity-at-least n)
       (string->symbol (format "(arguments-drop ~A ~A)" (object-name proc) n)))
      (make-keyword-procedure
       (lambda (kw-lst kw-lst-vals . args)
         (keyword-apply proc kw-lst kw-lst-vals (drop args n))))))

(define (arguments-drop-right proc n)
  (if (no-keyword-args? proc)
      (procedure-reduce-arity
       (lambda args
         (apply proc (drop-right args n)))
       (make-arity-at-least n)
       (string->symbol (format "(arguments-drop-right ~A ~A)" (object-name proc) n)))
      (make-keyword-procedure
       (lambda (kw-lst kw-lst-vals . args)
         (keyword-apply proc kw-lst kw-lst-vals (drop-right args n))))))

(define (arguments-take proc n)
  (unless (procedure-arity-includes? proc n #t)
    (raise-argument-error 'arguments-take (format "(procedure-arity-includes/c ~A)" n) proc))
  (if (no-keyword-args? proc)
      (procedure-reduce-arity
       (lambda args
         (apply proc (take args n)))
       (make-arity-at-least n)
       (string->symbol (format "(arguments-take ~A ~A)" (object-name proc) n)))
      (make-keyword-procedure
       (lambda (kw-lst kw-lst-vals . args)
         (keyword-apply proc kw-lst kw-lst-vals (take args n))))))

(define (arguments-take-right proc n)
  (unless (procedure-arity-includes? proc n #t)
    (raise-argument-error 'arguments-take-right (format "(procedure-arity-includes/c ~A)" n) proc))
  (if (no-keyword-args? proc)
      (procedure-reduce-arity
       (lambda args
         (apply proc (take-right args n)))
       (make-arity-at-least n)
       (string->symbol (format "(arguments-take-right ~A ~A)" (object-name proc) n)))
      (make-keyword-procedure
       (lambda (kw-lst kw-lst-vals . args)
         (keyword-apply proc kw-lst kw-lst-vals (take-right args n))))))

(define (group-by key [= equal?])
  (lambda (list) (rkt:group-by key list =)))

(define (begin-procedure . thunks)
  (for/last ([thunk (in-list thunks)]) (thunk)))

(define (if-procedure value then-thunk else-thunk)
  (if value (then-thunk) (else-thunk)))

(define (when-procedure value . thunks)
  (when value
    (for-each (lambda (thunk) (thunk)) thunks)))

(define (unless-procedure value . thunks)
  (unless value
    (for-each (lambda (thunk) (thunk)) thunks)))

(define (value-procedure value then-proc else-thunk)
  (if value
      (then-proc value)
      (else-thunk)))

(define (case-procedure value thunk-alist [else-thunk void])
  (let loop ([thunk-alist thunk-alist])
    (cond
      ((null? thunk-alist) (else-thunk))
      ((equal? value (caar thunk-alist))
       ((cdar thunk-alist)))
      (else (loop (cdr thunk-alist))))))

(define (and-procedure . thunks)
  (for/and ([thunk (in-list thunks)]) (thunk)))

(define (or-procedure . thunks)
  (for/or ([thunk (in-list thunks)]) (thunk)))

(define (eager-and-procedure . thunks)
  (andmap identity (map-in-order (lambda (thunk) (thunk)) thunks)))

(define (eager-or-procedure . thunks)
  (ormap identity (map-in-order (lambda (thunk) (thunk)) thunks)))

(define (funcall-procedure thunk) (thunk))

(define (loop-procedure thunk)
  (thunk)
  (loop-procedure thunk))

(define (while-procedure thunk)
  (when (thunk)
    (while-procedure thunk)))

(define (until-procedure thunk)
  (unless (thunk)
      (until-procedure thunk)))

(define always (constantly #t))
(define never (constantly #f))
(define (boolean obj)
  (if obj #t #f))

(module+ test
  (require srfi/1)

(test-begin "Combinators")

(test-group
 "constantly"

 (test-equal '(1 2)
   (call-with-values
       (lambda () ((constantly 1 2) 'a 'b))
     list))

 (test-equal '(1)
   (call-with-values
       (lambda () ((constantly 1) 'a 'b))
     list))

 (test-equal '()
   (call-with-values
       (lambda () ((constantly) 'a 'b))
     list)))



(test-group
 "complement"

 (test-equal #f
   ((complement symbol?) 'a))

 (test-equal #t
   ((complement symbol?) 1)))



(test-group
 "swap"

 (test-equal '(2 1 3 4)
   ((swap list) 1 2 3 4)))



(test-group
 "flip"

 (test-equal '(4 3 2 1)
   ((flip list) 1 2 3 4)))



(test-group
 "on-left"

 (test-equal '(1)
   ((on-left list) 1 2)))



(test-group
 "on-right"

 (test-equal '(2)
   ((on-right list) 1 2)))



(test-group
 "conjoin"

 (test-assert
     ((conjoin number? exact?)))

 (test-assert
     ((conjoin number? exact?) 1 2))

 (test-assert
     (not ((conjoin number? exact?) 1 2.)))

 (test-assert
     ((conjoin) 1 2)))



(test-group
 "disjoin"

 (test-assert
     ((disjoin number? string?)))

 (test-assert
     ((disjoin number? string?) 1 "a"))

 (test-assert
     (not ((disjoin number? string?) 'a 'b)))

 (test-assert
     (not ((disjoin) 1 2))))



(test-group
 "each-of"

 (let ((r1 #f)
       (r2 #f))
   ((each-of
     (lambda args (set! r1 args))
     (lambda args (set! r2 args)))
    1 2)
   (test-equal r1 '(1 2))
   (test-equal r2 '(1 2))))



(test-group
 "all-of"

 (test-assert
     ((all-of string?) '()))

 (test-assert
     ((all-of string?) '("a" "b")))

 (test-equal
   "b"
   ((all-of values) '("a" "b")))

 (test-assert
     (not ((all-of string?) '("a" b))))

 (test-assert
     (not ((all-of (lambda (x)
                     (when (equal? x 'c)
                       ;; should short circuit before this point
                       (test-assert #f))
                     (string? x)))
           '("a" b c)))))



(test-group
 "any-of"

 (test-assert
     (not ((any-of string?) '())))

 (test-assert
     ((any-of string?) '("a" b)))

 (test-equal
   "a"
   ((any-of values) '("a" "b")))

 (test-assert
     (not ((any-of string?) '(a b))))

 (test-assert
     ((any-of (lambda (x)
                (when (equal? x 'b)
                  ;; should short circuit before this point
                  (test-assert #f))
                (string? x)))
      '("a" b))))



(test-group
 "on"

 (test-equal '(2 3 4)
   ((on list (lambda (x) (+ 1 x))) 1 2 3)))



(test-group
 "left-section"

 (test-equal '(1 2 3 4)
   ((left-section list 1 2) 3 4)))



(test-group
 "right-section"

 (test-equal '(3 4 2 1)
   ((right-section list 1 2) 3 4)))



(test-group
  "apply-chain"
  (define cadr* (apply-chain car cdr))
  (define factorial ;;test multivalue
    (apply-chain
      *
      (lambda (n) (apply values (cdr (iota (+ 1 n)))))))

  (test-equal 2 (cadr* (list 1 2 3)))
  (test-equal 120 (factorial 5)))



(test-group
  "arguments-drop"

  (test-equal
    '(4)
    ((arguments-drop list 3) 1 2 3 4)))



(test-group
  "arguments-drop-right"

  (test-equal
    '(1)
    ((arguments-drop-right list 3) 1 2 3 4)))



(test-group
  "arguments-take"

  (test-equal
    '(1 2 3)
    ((arguments-take list 3) 1 2 3 4)))



(test-group
  "arguments-take-right"

  (test-equal
    '(2 3 4)
    ((arguments-take-right list 3) 1 2 3 4)))


(test-group
  "group-by"

  (test-equal
    '((1 3)
      (2 4))
    ((group-by odd?) '(1 2 3 4)))

  (test-equal
    '(("aa" "ab")
      ("ba" "bb"))
    ((group-by (lambda (str) (string-ref str 0))
               char=?)
     (list "aa" "ba" "bb" "ab"))))




(test-group
 "begin-procedure"

 (test-equal 2
   (begin-procedure
    (lambda () 1)
    (lambda () 2))))



(test-group
 "if-procedure"

 (test-equal 1
   (if-procedure #t
                 (lambda () 1)
                 (lambda () (test-assert #f))))

 (test-equal 2
   (if-procedure #f
                 (lambda () (test-assert #f))
                 (lambda () 2))))



(test-group
 "when-procedure"

 (define lst1 '())
 (define lst2 '())

 (when-procedure #t
                 (lambda () (set! lst1 (cons 1 lst1)))
                 (lambda () (set! lst1 (cons 2 lst1))))

 (when-procedure #f
                 (lambda () (set! lst2 (cons 1 lst2)))
                 (lambda () (set! lst2 (cons 2 lst2))))

 (test-equal '(2 1) lst1)
 (test-equal '() lst2))



(test-group
 "unless-procedure"

 (define lst1 '())
 (define lst2 '())

 (unless-procedure #t
                 (lambda () (set! lst1 (cons 1 lst1)))
                 (lambda () (set! lst1 (cons 2 lst1))))

 (unless-procedure #f
                 (lambda () (set! lst2 (cons 1 lst2)))
                 (lambda () (set! lst2 (cons 2 lst2))))

 (test-equal '() lst1)
 (test-equal '(2 1) lst2))



(test-group
 "value-procedure"

 (test-equal "1"
   (value-procedure 1
                    number->string
                    (lambda () (test-assert #f))))

 (test-equal 2
   (value-procedure #f
                    (lambda args (test-assert #f))
                    (lambda () 2))))



(test-group
 "case-procedure"

 (test-equal 2
   (case-procedure 'b
                   `((a . ,(lambda () 1))
                     (b . ,(lambda () 2)))))

 (test-equal 3
   (case-procedure 'c
                   `((a . ,(lambda () 1))
                     (b . ,(lambda () 2)))
                   (lambda () 3))))



(test-group
 "and-procedure"

 (test-assert
     (and-procedure))

 (test-equal 2
   (and-procedure (lambda () 1)
                       (lambda () 2)))

 (test-assert
     (not (and-procedure (lambda () #f)
                              (lambda () (test-assert #f))))))



(test-group
 "eager-and-procedure"

 (test-assert
     (eager-and-procedure))

 (test-equal 2
   (eager-and-procedure (lambda () 1)
                        (lambda () 2)))

 (let ((second-called? #f))
  (test-assert
     (not (eager-and-procedure (lambda () #f)
                               (lambda ()
                                 (set! second-called? #t)
                                 #t))))
  (test-assert second-called?)))



(test-group
 "or-procedure"

 (test-assert
     (not (or-procedure)))

 (test-equal 2
   (or-procedure (lambda () #f)
                 (lambda () 2)))

 (test-assert
     (or-procedure (lambda () 1)
                   (lambda () (test-assert #f)))))



(test-group
 "eager-or-procedure"

 (test-assert
     (not (eager-or-procedure)))

 (test-equal 2
   (eager-or-procedure (lambda () #f)
                       (lambda () 2)))

 (let ((second-called? #f))
  (test-equal 1
     (eager-or-procedure (lambda () 1)
                        (lambda ()
                          (set! second-called? #t)
                          #f)))
  (test-assert second-called?)))

(test-group
 "funcall-procedure"

 (test-equal 1
    (funcall-procedure (lambda () 1))))

(test-group
 "loop-procedure"

 (void (call/cc (lambda (k)
            (define v 0)
            (define (thunk)
              (when (> v 5)
                (k #t))
              (set! v (+ 1 v)))
            (loop-procedure thunk)
            (test-assert #t)))))



(test-group
 "while-procedure"

 (define v 0)
 (define (thunk)
   (set! v (+ 1 v))
   (< v 5))
 (while-procedure thunk)
 (test-equal 5 v))



(test-group
 "until-procedure"

 (define v 0)
 (define (thunk)
   (set! v (+ 1 v))
   (>= v 5))
 (until-procedure thunk)
 (test-equal 5 v))


(test-group
 "always"

 (test-assert (always))
 (test-assert (always 'a)))



(test-group
 "never"

 (test-assert (not (never)))
 (test-assert (not (never 'a))))



(test-group
 "boolean"

 (test-equal #t (boolean 1))
 (test-equal #f (boolean #f)))



(test-group
 "values"

 (test-equal 1 (values 1))
 (test-equal 'a (values 'a)))


(test-end)
)