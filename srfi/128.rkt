#lang racket/base

(require racket/contract (only-in racket/math nan? infinite? exact-round)
         (only-in racket/bool boolean=? symbol=?)
         (only-in racket/list index-where remove-duplicates))
(module+ test (require rackunit))
(provide
 hash-bound hash-salt with-hash-salt comparator-if<=>
 (contract-out
  [comparator? predicate/c]
  [comparator-ordered? (-> comparator? boolean?)]
  [comparator-hashable? (-> comparator? boolean?)]
  [make-comparator (->* ((or/c predicate/c #t) (-> any/c any/c any/c) (or/c (-> any/c any/c any/c) #f) (or/c (-> any/c exact-integer?) #f)) (#:secondary-hash (or/c (-> any/c exact-integer?) #f)) comparator?)]
  [make-pair-comparator (-> comparator? comparator? comparator?)]
  [make-list-comparator (-> comparator? predicate/c (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) comparator?)]
  [make-vector-comparator (-> comparator? predicate/c (-> any/c exact-integer?) (-> any/c exact-integer? any/c) comparator?)]
  [make-eq-comparator (-> comparator?)]
  [make-eqv-comparator (-> comparator?)]
  [make-equal-comparator (-> comparator?)]
  [make-equal-always-comparator (-> comparator?)]
  [boolean-hash (-> boolean? exact-integer?)]
  [char-hash (-> char? exact-integer?)]
  [char-ci-hash (-> char? exact-integer?)]
  [string-hash (-> string? exact-integer?)]
  [string-ci-hash (-> string? exact-integer?)]
  [symbol-hash (-> symbol? exact-integer?)]
  [number-hash (-> number? exact-integer?)]
  [make-default-comparator (-> comparator?)]
  [default-hash (-> any/c exact-integer?)]
  [comparator-register-default! (-> comparator? void?)]
  [comparator-type-test-predicate (-> comparator? predicate/c)]
  [comparator-equality-predicate (-> comparator? (-> any/c any/c any/c))]
  [comparator-ordering-predicate (-> comparator? (-> any/c any/c any/c))]
  [comparator-hash-function (-> comparator? (-> any/c exact-integer?))]
  [comparator-secondary-hash-function (-> comparator? (-> any/c exact-integer?))]
  [comparator-test-type (-> comparator? any/c any/c)]
  [comparator-check-type (-> comparator? any/c #t)]
  [comparator-hash (-> comparator? any/c exact-integer?)]
  [comparator-secondary-hash (-> comparator? any/c exact-integer?)]
  [=? (-> comparator? any/c any/c any/c ... boolean?)]
  [<? (-> comparator? any/c any/c any/c ... boolean?)]
  [>? (-> comparator? any/c any/c any/c ... boolean?)]
  [<=? (-> comparator? any/c any/c any/c ... boolean?)]
  [>=? (-> comparator? any/c any/c any/c ... boolean?)]

  ;;; SRFI-162 routines
  [comparator-min (->* (comparator? any/c) () #:rest list? any/c)]
  [comparator-max (->* (comparator? any/c) () #:rest list? any/c)]
  [comparator-min-in-list (-> comparator? list? any/c)]
  [comparator-max-in-list (-> comparator? list? any/c)]
  [default-comparator comparator?]
  [boolean-comparator comparator?]
  [real-comparator comparator?]
  [char-comparator comparator?]
  [char-ci-comparator comparator?]
  [string-comparator comparator?]
  [string-ci-comparator comparator?]
  [pair-comparator comparator?]
  [list-comparator comparator?]
  [vector-comparator comparator?]
  [eq-comparator comparator?]
  [eqv-comparator comparator?]
  [equal-comparator comparator?]
  [equal-always-comparator comparator?]

  ;;; SRFI-228 routines
  [make-wrapper-comparator (-> predicate/c (-> any/c any/c) comparator? comparator?)]
  [make-product-comparator (-> comparator? ... comparator?)]
  [make-sum-comparator (-> comparator? ... comparator?)]
  [comparator-one comparator?]
  [comparator-zero comparator?]
  ))


;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; Main part of the SRFI 114 reference implementation

;;; "There are two ways of constructing a software design: One way is to
;;; make it so simple that there are obviously no deficiencies, and the
;;; other way is to make it so complicated that there are no *obvious*
;;; deficiencies." --Tony Hoare

;;; Syntax (because syntax must be defined before it is used, contra Dr. Hardcase)

;; Arithmetic if
(define-syntax comparator-if<=>
  (syntax-rules ()
    ((if<=> a b less equal greater)
     (comparator-if<=> (make-default-comparator) a b less equal greater))
    ((comparator-if<=> comparator a b less equal greater)
     (cond
       ((=? comparator a b) equal)
       ((<? comparator a b) less)
       (else greater)))))

;; Upper bound of hash functions is 2^25-1
(define-syntax hash-bound
  (syntax-rules ()
    ((hash-bound) 33554432)))

(define %salt% (make-parameter 16064047))

(define-syntax hash-salt
   (syntax-rules ()
     ((hash-salt) (%salt%))))

(define-syntax with-hash-salt
  (syntax-rules ()
    ((with-hash-salt new-salt hash-func obj)
     (parameterize ((%salt% new-salt)) (hash-func obj)))))

;;; Definition of comparator records with accessors and basic comparator

(define (apply-hash c hash-fun)
  (bitwise-xor
   (hash-fun (comparator-type-test-predicate c))
   (hash-fun (comparator-equality-predicate c))
   (if (comparator-ordered? c)
       (hash-fun (comparator-ordering-predicate c))
       0)
   (if (comparator-hashable? c)
       (+ (hash-fun (comparator-hash-function c))
          (hash-fun (comparator-secondary-hash-function c)))
       0)))

(struct comparator (type-test-predicate equality-predicate ordering-predicate hash-function secondary-hash-function ordered? hashable?)
  #:methods gen:equal+hash
  [(define (equal-proc a b real-equal?)
     (and
      (real-equal? (comparator-type-test-predicate a) (comparator-type-test-predicate b))
      (real-equal? (comparator-equality-predicate a) (comparator-equality-predicate b))
      (cond
        ((and (comparator-ordered? a) (comparator-ordered? b))
         (real-equal? (comparator-ordering-predicate a) (comparator-ordering-predicate b)))
        ((and (eq? (comparator-ordered? a) #f) (eq? (comparator-ordered? b) #f))
         #t)
        (else #f))
      (cond
        ((and (comparator-hashable? a) (comparator-hashable? b))
         (and
          (real-equal? (comparator-hash-function a) (comparator-hash-function b))
          (real-equal? (comparator-secondary-hash-function a) (comparator-secondary-hash-function b))))
        ((and (eq? (comparator-hashable? a) #f) (eq? (comparator-hashable? b) #f))
         #t)
        (else #f))))
   (define (hash-proc cmp hash-fun) (apply-hash cmp hash-fun))
   (define (hash2-proc cmp hash-fun) (apply-hash cmp hash-fun))])

(define (default-type-test x) #t)
(define (default-ordering-func a b) (error "ordering not supported"))
(define (default-hash-func x) (error "hashing not supported"))
(define (default-secondary-hash-func x) 42)

;; Public constructor
(define (make-comparator type-test equality ordering hash #:secondary-hash [secondary-hash #f])
  (comparator
    (if (eq? type-test #t) default-type-test type-test)
    (if (eq? equality #t) equal? equality)
    (if ordering ordering default-ordering-func)
    (if hash hash default-hash-func)
    (if secondary-hash secondary-hash (if hash default-secondary-hash-func default-hash-func))
    (if ordering #t #f)
    (if hash #t #f)))

;;; Invokers

;; Invoke the test type
(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

;; Invoke the test type and throw an error if it fails
(define (comparator-check-type comparator obj)
  (if (comparator-test-type comparator obj)
    #t
    (error "comparator type check failed" comparator obj)))

;; Invoke the hash function
(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

(define (comparator-secondary-hash comparator obj)
  ((comparator-secondary-hash-function comparator) obj))

;;; Comparison predicates

;; Binary versions for internal use

(define (binary=? comparator a b)
  ((comparator-equality-predicate comparator) a b))

(define (binary<? comparator a b)
  ((comparator-ordering-predicate comparator) a b))

(define (binary>? comparator a b)
  (binary<? comparator b a))

(define (binary<=? comparator a b)
  (not (binary>? comparator a b)))

(define (binary>=? comparator a b)
  (not (binary<? comparator a b)))

;; General versions for export

(define (=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))


;;; Simple ordering and hash functions

(define (boolean<? a b)
  ;; #f < #t but not otherwise
  (and (not a) b))


(define (boolean-hash obj)
  (if obj (%salt%) 0))

(define (char-hash obj)
  (modulo (* (%salt%) (char->integer obj)) (hash-bound)))

(define (char-ci-hash obj)
  (modulo (* (%salt%) (char->integer (char-foldcase obj))) (hash-bound)))

(define (number-hash obj)
  (cond
    ((nan? obj) (%salt%))
    ((and (infinite? obj) (positive? obj)) (* 2 (%salt%)))
    ((infinite? obj) (* (%salt%) 3))
    ((real? obj) (abs (exact-round obj)))
    (else (+ (number-hash (real-part obj)) (number-hash (imag-part obj))))))

;; Lexicographic ordering of complex numbers
(define (complex<? a b)
  (if (= (real-part a) (real-part b))
    (< (imag-part a) (imag-part b))
    (< (real-part a) (real-part b))))

(define (string-ci-hash obj)
    (string-hash (string-foldcase obj)))

#;(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (symbol-hash obj)
  (string-hash (symbol->string obj)))

;;; Wrapped equality predicates
;;; These comparators don't have ordering functions.

(define (make-eq-comparator)
  (make-comparator #t eq? #f eq-hash-code))

(define (make-eqv-comparator)
  (make-comparator #t eqv? #f eqv-hash-code))

(define (make-equal-comparator)
  (make-comparator #t equal? #f equal-hash-code #:secondary-hash equal-secondary-hash-code))

(define (make-equal-always-comparator)
  (make-comparator #t equal-always? #f equal-always-hash-code #:secondary-hash equal-secondary-hash-code))

;;; Sequence ordering and hash functions
;; The hash functions are based on djb2, but
;; modulo 2^25 instead of 2^32 in hopes of sticking to fixnums.

(define (make-hasher)
  (let ((result (%salt%)))
    (case-lambda
     (() result)
     ((n) (set! result (+ (modulo (* result 33) (hash-bound)) n))
          result))))

;;; Pair comparator
(define (make-pair-comparator car-comparator cdr-comparator)
   (make-comparator
     (make-pair-type-test car-comparator cdr-comparator)
     (make-pair=? car-comparator cdr-comparator)
     (make-pair<? car-comparator cdr-comparator)
     (make-pair-hash car-comparator cdr-comparator)))

(define (make-pair-type-test car-comparator cdr-comparator)
  (lambda (obj)
    (and (pair? obj)
         (comparator-test-type car-comparator (car obj))
         (comparator-test-type cdr-comparator (cdr obj)))))

(define (make-pair=? car-comparator cdr-comparator)
   (lambda (a b)
     (and ((comparator-equality-predicate car-comparator) (car a) (car b))
          ((comparator-equality-predicate cdr-comparator) (cdr a) (cdr b)))))

(define (make-pair<? car-comparator cdr-comparator)
   (lambda (a b)
      (if (=? car-comparator (car a) (car b))
        (<? cdr-comparator (cdr a) (cdr b))
        (<? car-comparator (car a) (car b)))))

(define (make-pair-hash car-comparator cdr-comparator)
   (lambda (obj)
     (let ((acc (make-hasher)))
       (acc (comparator-hash car-comparator (car obj)))
       (acc (comparator-hash cdr-comparator (cdr obj)))
       (acc))))

;;; List comparator

;; Cheap test for listness
(define (norp? obj) (or (null? obj) (pair? obj)))

(define (make-list-comparator element-comparator type-test empty? head tail)
   (make-comparator
     (make-list-type-test element-comparator type-test empty? head tail)
     (make-list=? element-comparator type-test empty? head tail)
     (make-list<? element-comparator type-test empty? head tail)
     (make-list-hash element-comparator type-test empty? head tail)))


(define (make-list-type-test element-comparator type-test empty? head tail)
  (lambda (obj)
    (and
      (type-test obj)
      (let ((elem-type-test (comparator-type-test-predicate element-comparator)))
        (let loop ((obj obj))
          (cond
            ((empty? obj) #t)
            ((not (elem-type-test (head obj))) #f)
            (else (loop (tail obj)))))))))

(define (make-list=? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((elem=? (comparator-equality-predicate element-comparator)))
      (let loop ((a a) (b b))
        (cond
          ((and (empty? a) (empty? b) #t))
          ((empty? a) #f)
          ((empty? b) #f)
          ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
          (else #f))))))

(define (make-list<? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((elem=? (comparator-equality-predicate element-comparator))
          (elem<? (comparator-ordering-predicate element-comparator)))
      (let loop ((a a) (b b))
        (cond
          ((and (empty? a) (empty? b) #f))
          ((empty? a) #t)
          ((empty? b) #f)
          ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
          ((elem<? (head a) (head b)) #t)
          (else #f))))))

(define (make-list-hash element-comparator type-test empty? head tail)
  (lambda (obj)
    (let ((elem-hash (comparator-hash-function element-comparator))
          (acc (make-hasher)))
      (let loop ((obj obj))
        (cond
          ((empty? obj) (acc))
          (else (acc (elem-hash (head obj))) (loop (tail obj))))))))


;;; Vector comparator

(define (make-vector-comparator element-comparator type-test length ref)
     (make-comparator
       (make-vector-type-test element-comparator type-test length ref)
       (make-vector=? element-comparator type-test length ref)
       (make-vector<? element-comparator type-test length ref)
       (make-vector-hash element-comparator type-test length ref)))

(define (make-vector-type-test element-comparator type-test length ref)
  (lambda (obj)
    (and
      (type-test obj)
      (let ((elem-type-test (comparator-type-test-predicate element-comparator))
            (len (length obj)))
        (let loop ((n 0))
          (cond
            ((= n len) #t)
            ((not (elem-type-test (ref obj n))) #f)
            (else (loop (+ n 1)))))))))

(define (make-vector=? element-comparator type-test length ref)
   (lambda (a b)
     (and
       (= (length a) (length b))
       (let ((elem=? (comparator-equality-predicate element-comparator))
             (len (length b)))
         (let loop ((n 0))
           (cond
             ((= n len) #t)
             ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
             (else #f)))))))

(define (make-vector<? element-comparator type-test length ref)
   (lambda (a b)
     (cond
       ((< (length a) (length b)) #t)
       ((> (length a) (length b)) #f)
        (else
         (let ((elem=? (comparator-equality-predicate element-comparator))
             (elem<? (comparator-ordering-predicate element-comparator))
             (len (length a)))
         (let loop ((n 0))
           (cond
             ((= n len) #f)
             ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
             ((elem<? (ref a n) (ref b n)) #t)
             (else #f))))))))

(define (make-vector-hash element-comparator type-test length ref)
  (lambda (obj)
    (let ((elem-hash (comparator-hash-function element-comparator))
          (acc (make-hasher))
          (len (length obj)))
      (let loop ((n 0))
        (cond
          ((= n len) (acc))
          (else (acc (elem-hash (ref obj n))) (loop (+ n 1))))))))

(define (string-hash obj)
  (let ((acc (make-hasher))
        (len (string-length obj)))
    (let loop ((n 0))
      (cond
        ((= n len) (acc))
        (else (acc (char->integer (string-ref obj n))) (loop (+ n 1)))))))
;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; The default comparator

;;; Standard comparators and their functions

;; The unknown-object comparator, used as a fallback to everything else
;; Everything compares exactly the same and hashes to 0
(define unknown-object-comparator
  (make-comparator
    (lambda (obj) #t)
    (lambda (a b) #t)
    (lambda (a b) #f)
    (lambda (obj) 0)))

;; Next index for added comparator

(define first-comparator-index 9)
(define *next-comparator-index* 9)
(define *registered-comparators* (list unknown-object-comparator))

;; Register a new comparator for use by the default comparator.
(define (comparator-register-default! comparator)
  (set! *registered-comparators* (cons comparator *registered-comparators*))
  (set! *next-comparator-index* (+ *next-comparator-index* 1)))

;; Return ordinal for object types: null sorts before pairs, which sort
;; before booleans, etc.  Implementations can extend this.
;; People who call comparator-register-default! effectively do extend it.
(define (object-type obj)
  (cond
    ((null? obj) 0)
    ((pair? obj) 1)
    ((boolean? obj) 2)
    ((char? obj) 3)
    ((string? obj) 4)
    ((symbol? obj) 5)
    ((number? obj) 6)
    ((vector? obj) 7)
    ((bytes? obj) 8)
    ; Add more here if you want: be sure to update comparator-index variables
    (else (registered-index obj))))

;; Return the index for the registered type of obj.
(define (registered-index obj)
  (let loop ((i 0) (registry *registered-comparators*))
    (cond
      ((null? registry) (+ first-comparator-index i))
      ((comparator-test-type (car registry) obj) (+ first-comparator-index i))
      (else (loop (+ i 1) (cdr registry))))))

;; Given an index, retrieve a registered conductor.
;; Index must be >= first-comparator-index.
(define (registered-comparator i)
  (list-ref *registered-comparators* (- i first-comparator-index)))

(define (dispatch-equality type a b)
  (case type
    ((0) #t) ; All empty lists are equal
    ((1) ((make-pair=? (make-default-comparator) (make-default-comparator)) a b))
    ((2) (boolean=? a b))
    ((3) (char=? a b))
    ((4) (string=? a b))
    ((5) (symbol=? a b))
    ((6) (= a b))
    ((7) ((make-vector=? (make-default-comparator)
                         vector? vector-length vector-ref) a b))
    ((8) ((make-vector=? (make-comparator exact-integer? = < default-hash)
                         bytes? bytes-length bytes-ref) a b))
    ; Add more here
    (else (binary=? (registered-comparator type) a b))))

(define (dispatch-ordering type a b)
  (case type
    ((0) 0) ; All empty lists are equal
    ((1) ((make-pair<? (make-default-comparator) (make-default-comparator)) a b))
    ((2) (boolean<? a b))
    ((3) (char<? a b))
    ((4) (string<? a b))
    ((5) (symbol<? a b))
    ((6) (complex<? a b))
    ((7) ((make-vector<? (make-default-comparator) vector? vector-length vector-ref) a b))
    ((8) ((make-vector<? (make-comparator exact-integer? = < default-hash)
			 bytes? bytes-length bytes-ref) a b))
    ; Add more here
    (else (binary<? (registered-comparator type) a b))))

;;; The author of SRFI 128 has suggested a post-finalization note
;;; saying the first and third bullet items stating "must" requirements
;;; for default-hash may be weakened.  That allows a much faster hash
;;; function to be used for lists and vectors.

(define (default-hash obj)
  (case (object-type obj)
    ((0 1 7) ; empty list, pair, or vector
     ((make-hasher) (equal-hash-code obj)))
    ((2) (boolean-hash obj))
    ((3) (char-hash obj))
    ((4) (string-hash obj))
    ((5) (symbol-hash obj))
    ((6) (number-hash obj))
    ((8) ((make-vector-hash (make-default-comparator)
                             bytes? bytes-length bytes-ref) obj))
    ; Add more here
    (else (comparator-hash (registered-comparator (object-type obj)) obj))))

(define (default-ordering a b)
  (let ((a-type (object-type a))
        (b-type (object-type b)))
    (cond
      ((< a-type b-type) #t)
      ((> a-type b-type) #f)
      (else (dispatch-ordering a-type a b)))))

(define (default-equality a b)
  (let ((a-type (object-type a))
        (b-type (object-type b)))
    (if (= a-type b-type) (dispatch-equality a-type a b) #f)))

(define (make-default-comparator)
  (make-comparator
    (lambda (obj) #t)
    default-equality
    default-ordering
    default-hash))

;;; SRFI-162

(define (comparator-max-in-list comp list)
  (let ((< (comparator-ordering-predicate comp)))
    (let loop ((max (car list)) (list (cdr list)))
      (if (null? list)
        max
        (if (< max (car list))
          (loop (car list) (cdr list))
          (loop max (cdr list)))))))

(define (comparator-min-in-list comp list)
  (let ((< (comparator-ordering-predicate comp)))
    (let loop ((min (car list)) (list (cdr list)))
      (if (null? list)
        min
        (if (< min (car list))
          (loop min (cdr list))
          (loop (car list) (cdr list)))))))

(define (comparator-max comp . args)
  (comparator-max-in-list comp args))

(define (comparator-min comp . args)
  (comparator-min-in-list comp args))

(define default-comparator
  (make-default-comparator))

(define boolean-comparator
  (make-comparator
    boolean?
    boolean=?
    (lambda (x y) (and (not x) y))
    boolean-hash))

(define real-comparator
  (make-comparator
    real?
    =
    <
    number-hash))

(define char-comparator
  (make-comparator
    char?
    char=?
    char<?
    (lambda (c) (number-hash (char->integer c)))))

(define char-ci-comparator
  (make-comparator
    char?
    char-ci=?
    char-ci<?
    (lambda (c) (number-hash (char->integer (char-downcase c))))))

(define string-comparator
  (make-comparator
    string?
    string=?
    string<?
    string-hash))

(define string-ci-comparator
  (make-comparator
    string?
    string-ci=?
    string-ci<?
    string-ci-hash))

(define pair-comparator
  (make-pair-comparator
    default-comparator
    default-comparator))

(define list-comparator
  (make-list-comparator
    default-comparator
    list?
    null?
    car
    cdr))

(define vector-comparator
  (make-vector-comparator
    default-comparator
    vector?
    vector-length
    vector-ref))

(define eq-comparator (make-eq-comparator))
(define eqv-comparator (make-eqv-comparator))
(define equal-comparator (make-equal-comparator))
(define equal-always-comparator (make-equal-always-comparator))

;;; SRFI-228
;;; Mix of reference and custom implementation

(define (make-wrapper-comparator type? unwrap comp)
  (make-comparator
   type?
   (lambda (a b) ((comparator-equality-predicate comp) (unwrap a) (unwrap b)))
   (if (comparator-ordered? comp)
       (lambda (a b) ((comparator-ordering-predicate comp) (unwrap a) (unwrap b)))
       #f)
   (if (comparator-hashable? comp)
       (lambda (v) ((comparator-hash-function comp) (unwrap v)))
       #f)
   #:secondary-hash
   (if (comparator-hashable? comp)
       (lambda (v) ((comparator-secondary-hash-function comp) v))
       #f)))

(define comparator-one (make-comparator (lambda (x) #t) (lambda (a b) #t) (lambda (a b) #f) (lambda (x) 0) #:secondary-hash (lambda (x) 0)))
(define comparator-zero (make-comparator (lambda (x) #f) (lambda (a b) (error "can't compare" a b)) #f #f))

(define (make-product-comparator . comparators)
  (if (null? comparators)
      comparator-one
      (let* ((type-tests
              (remove-duplicates
               (map comparator-type-test-predicate comparators)
               eq?))
             (type-test
              (lambda (val)
                (andmap (lambda (test) (test val)) type-tests))))
        (make-comparator
         type-test
         (lambda (a b)
           (andmap (lambda (cmp)
                     ((comparator-equality-predicate cmp) a b))
                   comparators))
         (if (andmap comparator-ordered? comparators)
             (lambda (a b)
               (let loop ((cmps comparators))
                 (cond ((null? cmps) #f)
                       (((comparator-ordering-predicate (car cmps)) a b) #t)
                       (((comparator-equality-predicate (car cmps)) a b) (loop (cdr cmps)))
                       (else #f))))
             #f)
         (if (andmap comparator-hashable? comparators)
             (lambda (x)
               (foldl bitwise-xor
                     0
                     (map (lambda (cmp)
                            ((comparator-hash-function cmp) x))
                          comparators)))
             #f)))))

(define (comparator-index comparators val)
  (index-where comparators
   (lambda (cmp)
     ((comparator-type-test-predicate cmp) val))))

(define (make-sum-comparator . comparators)
  (if (null? comparators)
      comparator-zero
      (make-comparator
       (lambda (x)
         (ormap
          (lambda (cmp)
            ((comparator-type-test-predicate cmp) x))
          comparators))
       (lambda (a b)
         (let ((a-cmp-idx (comparator-index comparators a))
               (b-cmp-idx (comparator-index comparators b)))
           (if (not (= a-cmp-idx b-cmp-idx))
               #f
               (let ((cmp (list-ref comparators a-cmp-idx)))
                 ((comparator-equality-predicate cmp) a b)))))
       (if (andmap comparator-ordered? comparators)
           (lambda (a b)
             (let ((a-cmp-idx (comparator-index comparators a))
                   (b-cmp-idx (comparator-index comparators b)))
               (cond ((< a-cmp-idx b-cmp-idx) #t)
                     ((> a-cmp-idx b-cmp-idx) #f)
                     (else
                      (let ((cmp (list-ref comparators a-cmp-idx)))
                        ((comparator-ordering-predicate cmp) a b))))))
           #f)
       (if (andmap comparator-hashable? comparators)
           (lambda (x)
             (let ((cmp (memf (lambda (cmp) ((comparator-type-test-predicate cmp) x))
                              comparators)))
               ((comparator-hash-function cmp) x)))
           #f))))

(module+ test
  (require "132.rkt")
  (define-syntax-rule (test-assert tst)
    (check-true tst))
  (define-syntax-rule (test expected tst)
    (check-equal? tst expected))
  (define-syntax-rule (test-error tst)
    (check-exn exn:fail? (lambda () tst)))
  (define-syntax-rule (test-group name tests ...)
    (begin tests ...))
  (define-syntax-rule (test-eq expected tst)
    (check-eq? tst expected))
  (define-syntax-rule (test-equal expected tst)
    (check-equal? tst expected))
  (define (print msg) (void))

  (test-group "comparators"

              (define (vector-cdr vec)
                (let* ((len (vector-length vec))
                       (result (make-vector (- len 1))))
                  (let loop ((n 1))
                    (cond
                      ((= n len) result)
                      (else (vector-set! result (- n 1) (vector-ref vec n))
                            (loop (+ n 1)))))))

              (test '#(2 3 4) (vector-cdr '#(1 2 3 4)))
              (test '#() (vector-cdr '#(1)))

              (print "default-comparator")
              (define default-comparator (make-default-comparator))
              (print "real-comparator")
              (define real-comparator (make-comparator real? = < number-hash))
              (print "degenerate comparator")
              (define degenerate-comparator (make-comparator (lambda (x) #t) equal? #f #f))
              (print "boolean comparator")
              (define boolean-comparator
                (make-comparator boolean? eq? (lambda (x y) (and (not x) y)) boolean-hash))
              (print "bool-pair-comparator")
              (define bool-pair-comparator (make-pair-comparator boolean-comparator boolean-comparator))
              (print "num-list-comparator")
              (define num-list-comparator
                (make-list-comparator real-comparator list? null? car cdr))
              (print "num-vector-comparator")
              (define num-vector-comparator
                (make-vector-comparator real-comparator vector? vector-length vector-ref))
              (print "vector-qua-list comparator")
              (define vector-qua-list-comparator
                (make-list-comparator
                 real-comparator
                 vector?
                 (lambda (vec) (= 0 (vector-length vec)))
                 (lambda (vec) (vector-ref vec 0))
                 vector-cdr))
              (print "list-qua-vector-comparator")
              (define list-qua-vector-comparator
                (make-vector-comparator default-comparator list? length list-ref))
              ;(print "eq-comparator")
              ;(define eq-comparator (make-eq-comparator))
              ;(print "eqv-comparator")
              ;(define eqv-comparator (make-eqv-comparator))
              ;(print "equal-comparator")
              ;(define equal-comparator (make-equal-comparator))
              (print "symbol-comparator")
              (define symbol-comparator
                (make-comparator
                 symbol?
                 eq?
                 (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 symbol-hash))

              (test-group "comparators/predicates"
                          (test-assert (comparator? real-comparator))
                          (test-assert (not (comparator? =)))
                          (test-assert (comparator-ordered? real-comparator))
                          (test-assert (comparator-hashable? real-comparator))
                          (test-assert (not (comparator-ordered? degenerate-comparator)))
                          (test-assert (not (comparator-hashable? degenerate-comparator)))
                          ) ; end comparators/predicates

              (test-group "comparators/constructors"
                          (test-assert (=? boolean-comparator #t #t))
                          (test-assert (not (=? boolean-comparator #t #f)))
                          (test-assert (<? boolean-comparator #f #t))
                          (test-assert (not (<? boolean-comparator #t #t)))
                          (test-assert (not (<? boolean-comparator #t #f)))

                          (test-assert (comparator-test-type bool-pair-comparator '(#t . #f)))
                          (test-assert (not (comparator-test-type bool-pair-comparator 32)))
                          (test-assert (not (comparator-test-type bool-pair-comparator '(32 . #f))))
                          (test-assert (not (comparator-test-type bool-pair-comparator '(#t . 32))))
                          (test-assert (not (comparator-test-type bool-pair-comparator '(32 . 34))))
                          (test-assert (=? bool-pair-comparator '(#t . #t) '(#t . #t)))
                          (test-assert (not (=? bool-pair-comparator '(#t . #t) '(#f . #t))))
                          (test-assert (not (=? bool-pair-comparator '(#t . #t) '(#t . #f))))
                          (test-assert (<? bool-pair-comparator '(#f . #t) '(#t . #t)))
                          (test-assert (<? bool-pair-comparator '(#t . #f) '(#t . #t)))
                          (test-assert (not (<? bool-pair-comparator '(#t . #t) '(#t . #t))))
                          (test-assert (not (<? bool-pair-comparator '(#t . #t) '(#f . #t))))
                          (test-assert (not (<? bool-pair-comparator '(#f . #t) '(#f . #f))))

                          (test-assert (comparator-test-type num-vector-comparator '#(1 2 3)))
                          (test-assert (comparator-test-type num-vector-comparator '#()))
                          (test-assert (not (comparator-test-type num-vector-comparator 1)))
                          (test-assert (not (comparator-test-type num-vector-comparator '#(a 2 3))))
                          (test-assert (not (comparator-test-type num-vector-comparator '#(1 b 3))))
                          (test-assert (not (comparator-test-type num-vector-comparator '#(1 2 c))))
                          (test-assert (=? num-vector-comparator '#(1 2 3) '#(1 2 3)))
                          (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(4 5 6))))
                          (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(1 5 6))))
                          (test-assert (not (=? num-vector-comparator '#(1 2 3) '#(1 2 6))))
                          (test-assert (<? num-vector-comparator '#(1 2) '#(1 2 3)))
                          (test-assert (<? num-vector-comparator '#(1 2 3) '#(2 3 4)))
                          (test-assert (<? num-vector-comparator '#(1 2 3) '#(1 3 4)))
                          (test-assert (<? num-vector-comparator '#(1 2 3) '#(1 2 4)))
                          (test-assert (<? num-vector-comparator '#(3 4) '#(1 2 3)))
                          (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 2 3))))
                          (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 2))))
                          (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(0 2 3))))
                          (test-assert (not (<? num-vector-comparator '#(1 2 3) '#(1 1 3))))

                          (test-assert (not (<? vector-qua-list-comparator '#(3 4) '#(1 2 3))))
                          (test-assert (<? list-qua-vector-comparator '(3 4) '(1 2 3)))

                          (define bool-pair (cons #t #f))
                          (define bool-pair-2 (cons #t #f))
                          (define reverse-bool-pair (cons #f #t))
                          (test-assert (=? eq-comparator #t #t))
                          (test-assert (not (=? eq-comparator #f #t)))
                          (test-assert (=? eqv-comparator bool-pair bool-pair))
                          (test-assert (not (=? eqv-comparator bool-pair bool-pair-2)))
                          (test-assert (=? equal-comparator bool-pair bool-pair-2))
                          (test-assert (not (=? equal-comparator bool-pair reverse-bool-pair)))
                          (test-assert (=? equal-always-comparator bool-pair bool-pair-2))
                          (test-assert (not (=? equal-always-comparator bool-pair reverse-bool-pair)))

                          ) ; end comparators/constructors

              (test-group "comparators/hash"
                          (test-assert (exact-integer? (boolean-hash #f)))
                          (test-assert (not (negative? (boolean-hash #t))))
                          (test-assert (exact-integer? (char-hash #\a)))
                          (test-assert (not (negative? (char-hash #\b))))
                          (test-assert (exact-integer? (char-ci-hash #\a)))
                          (test-assert (not (negative? (char-ci-hash #\b))))
                          (test-assert (= (char-ci-hash #\a) (char-ci-hash #\A)))
                          (test-assert (exact-integer? (string-hash "f")))
                          (test-assert (not (negative? (string-hash "g"))))
                          (test-assert (exact-integer? (string-ci-hash "f")))
                          (test-assert (not (negative? (string-ci-hash "g"))))
                          (test-assert (= (string-ci-hash "f") (string-ci-hash "F")))
                          (test-assert (exact-integer? (symbol-hash 'f)))
                          (test-assert (not (negative? (symbol-hash 't))))
                          (test-assert (exact-integer? (number-hash 3)))
                          (test-assert (not (negative? (number-hash 3))))
                          (test-assert (exact-integer? (number-hash -3)))
                          (test-assert (not (negative? (number-hash -3))))
                          (test-assert (exact-integer? (number-hash 3.0)))
                          (test-assert (not (negative? (number-hash 3.0))))

                          ) ; end comparators/hash

              (test-group "comparators/default"
                          (test-assert (<? default-comparator '() '(a)))
                          (test-assert (not (=? default-comparator '() '(a))))
                          (test-assert (=? default-comparator #t #t))
                          (test-assert (not (=? default-comparator #t #f)))
                          (test-assert (<? default-comparator #f #t))
                          (test-assert (not (<? default-comparator #t #t)))
                          (test-assert (=? default-comparator #\a #\a))
                          (test-assert (<? default-comparator #\a #\b))

                          (test-assert (comparator-test-type default-comparator '()))
                          (test-assert (comparator-test-type default-comparator #t))
                          (test-assert (comparator-test-type default-comparator #\t))
                          (test-assert (comparator-test-type default-comparator '(a)))
                          (test-assert (comparator-test-type default-comparator 'a))
                          (test-assert (comparator-test-type default-comparator (make-bytes 10)))
                          (test-assert (comparator-test-type default-comparator 10))
                          (test-assert (comparator-test-type default-comparator 10.0))
                          (test-assert (comparator-test-type default-comparator "10.0"))
                          (test-assert (comparator-test-type default-comparator '#(10)))

                          (test-assert (=? default-comparator '(#t . #t) '(#t . #t)))
                          (test-assert (not (=? default-comparator '(#t . #t) '(#f . #t))))
                          (test-assert (not (=? default-comparator '(#t . #t) '(#t . #f))))
                          (test-assert (<? default-comparator '(#f . #t) '(#t . #t)))
                          (test-assert (<? default-comparator '(#t . #f) '(#t . #t)))
                          (test-assert (not (<? default-comparator '(#t . #t) '(#t . #t))))
                          (test-assert (not (<? default-comparator '(#t . #t) '(#f . #t))))
                          (test-assert (not (<? default-comparator '#(#f #t) '#(#f #f))))

                          (test-assert (=? default-comparator '#(#t #t) '#(#t #t)))
                          (test-assert (not (=? default-comparator '#(#t #t) '#(#f #t))))
                          (test-assert (not (=? default-comparator '#(#t #t) '#(#t #f))))
                          (test-assert (<? default-comparator '#(#f #t) '#(#t #t)))
                          (test-assert (<? default-comparator '#(#t #f) '#(#t #t)))
                          (test-assert (not (<? default-comparator '#(#t #t) '#(#t #t))))
                          (test-assert (not (<? default-comparator '#(#t #t) '#(#f #t))))
                          (test-assert (not (<? default-comparator '#(#f #t) '#(#f #f))))

                          (test-assert (= (comparator-hash default-comparator #t) (boolean-hash #t)))
                          (test-assert (= (comparator-hash default-comparator #\t) (char-hash #\t)))
                          (test-assert (= (comparator-hash default-comparator "t") (string-hash "t")))
                          (test-assert (= (comparator-hash default-comparator 't) (symbol-hash 't)))
                          (test-assert (= (comparator-hash default-comparator 10) (number-hash 10)))
                          (test-assert (= (comparator-hash default-comparator 10.0) (number-hash 10.0)))

                          (comparator-register-default!
                           (make-comparator procedure? (lambda (a b) #t) (lambda (a b) #f) (lambda (obj) 200)))
                          (test-assert (=? default-comparator (lambda () #t) (lambda () #f)))
                          (test-assert (not (<? default-comparator (lambda () #t) (lambda () #f))))
                          (test 200 (comparator-hash default-comparator (lambda () #t)))

                          ) ; end comparators/default

              ;; SRFI 128 does not actually require a comparator's four procedures
              ;; to be eq? to the procedures originally passed to make-comparator.
              ;; For interoperability/interchangeability between the comparators
              ;; of SRFI 114 and SRFI 128, some of the procedures passed to
              ;; make-comparator may need to be wrapped inside another lambda
              ;; expression before they're returned by the corresponding accessor.
              ;;
              ;; So this next group of tests is incorrect, hence commented out
              ;; and replaced by a slightly less naive group of tests.

              #;
              (test-group "comparators/accessors"
                          (define ttp (lambda (x) #t))
                          (define eqp (lambda (x y) #t))
                          (define orp (lambda (x y) #t))
                          (define hf (lambda (x) 0))
                          (define comp (make-comparator ttp eqp orp hf))
                          (test ttp (comparator-type-test-predicate comp))
                          (test eqp (comparator-equality-predicate comp))
                          (test orp (comparator-ordering-predicate comp))
                          (test hf (comparator-hash-function comp))
                          ) ; end comparators/accessors

              (test-group "comparators/accessors"
                          (define x1 0)
                          (define x2 0)
                          (define x3 0)
                          (define x4 0)
                          (define ttp (lambda (x) (set! x1 111) #t))
                          (define eqp (lambda (x y) (set! x2 222) #t))
                          (define orp (lambda (x y) (set! x3 333) #t))
                          (define hf (lambda (x) (set! x4 444) 0))
                          (define comp (make-comparator ttp eqp orp hf))
                          (test #t (and ((comparator-type-test-predicate comp) x1)   (= x1 111)))
                          (test #t (and ((comparator-equality-predicate comp) x1 x2) (= x2 222)))
                          (test #t (and ((comparator-ordering-predicate comp) x1 x3) (= x3 333)))
                          (test #t (and (zero? ((comparator-hash-function comp) x1)) (= x4 444)))
                          ) ; end comparators/accessors

              (test-group "comparators/invokers"
                          (test-assert (comparator-test-type real-comparator 3))
                          (test-assert (comparator-test-type real-comparator 3.0))
                          (test-assert (not (comparator-test-type real-comparator "3.0")))
                          (test-assert (comparator-check-type boolean-comparator #t))
                          (test-error (comparator-check-type boolean-comparator 't))
                          ) ; end comparators/invokers

              (test-group "comparators/comparison"
                          (test-assert (=? real-comparator 2 2.0 2))
                          (test-assert (<? real-comparator 2 3.0 4))
                          (test-assert (>? real-comparator 4.0 3.0 2))
                          (test-assert (<=? real-comparator 2.0 2 3.0))
                          (test-assert (>=? real-comparator 3 3.0 2))
                          (test-assert (not (=? real-comparator 1 2 3)))
                          (test-assert (not (<? real-comparator 3 1 2)))
                          (test-assert (not (>? real-comparator 1 2 3)))
                          (test-assert (not (<=? real-comparator 4 3 3)))
                          (test-assert (not (>=? real-comparator 3 4 4.0)))

                          ) ; end comparators/comparison

              (test-group "comparators/syntax"
                          (test 'less (comparator-if<=> real-comparator 1 2 'less 'equal 'greater))
                          (test 'equal (comparator-if<=> real-comparator 1 1 'less 'equal 'greater))
                          (test 'greater (comparator-if<=> real-comparator 2 1 'less 'equal 'greater))
                          (test 'less (comparator-if<=> "1" "2" 'less 'equal 'greater))
                          (test 'equal (comparator-if<=> "1" "1" 'less 'equal 'greater))
                          (test 'greater (comparator-if<=> "2" "1" 'less 'equal 'greater))

                          ) ; end comparators/syntax

              (test-group "comparators/bound-salt"
                          (test-assert (exact-integer? (hash-bound)))
                          (test-assert (exact-integer? (hash-salt)))
                          (test-assert (< (hash-salt) (hash-bound)))
                          ) ; end comparators/bound-salt

              ) ; end comparators

  (test-group "comparators/min-max"
              (test 5 (comparator-max real-comparator 1 5 3 2 -2))
              (test -2 (comparator-min real-comparator 1 5 3 2 -2))
              (test 5 (comparator-max-in-list real-comparator '(1 5 3 2 -2)))
              (test -2 (comparator-min-in-list real-comparator '(1 5 3 2 -2)))
              ) ; end comparators/min-max

  (test-group "comparators/variables"
              ;; Most of the variables have been tested above.
              (test-assert (=? char-comparator #\C #\C))
              (test-assert (=? char-ci-comparator #\c #\C))
              (test-assert (=? string-comparator "ABC" "ABC"))
              (test-assert (=? string-ci-comparator "abc" "ABC"))
              (test-assert (=? eq-comparator 32 32))
              (test-assert (=? eqv-comparator 32 32))
              (test-assert (=? equal-comparator "ABC" "ABC"))
              ) ; end comparators/variables

  (struct person (first-name last-name) #:constructor-name make-person #:transparent)

  (define person-name-comparator
    (make-product-comparator
     (make-wrapper-comparator person? person-last-name string-ci-comparator)
     (make-wrapper-comparator person? person-first-name string-ci-comparator)))

  (test-group "simple"
              (test-eq
                          #t
                          (<? person-name-comparator
                              (make-person "John" "Cowan")
                              (make-person "Daphne" "Preston-Kendal")))

              (test-eq
                          #t
                          (>? person-name-comparator
                              (make-person "Tom" "Smith")
                              (make-person "John" "Smith"))))

  (struct book (author title) #:constructor-name make-book #:transparent)

  (define book-comparator
    (make-product-comparator
     (make-wrapper-comparator book? book-author person-name-comparator)
     (make-wrapper-comparator book? book-title string-ci-comparator)))

  (struct cd (artist title) #:constructor-name make-cd #:transparent)

  (define cd-comparator
    (make-product-comparator
     (make-wrapper-comparator cd? cd-artist person-name-comparator)
     (make-wrapper-comparator cd? cd-title string-ci-comparator)))

  (define item-comparator
    (make-sum-comparator book-comparator cd-comparator))

  (test-group "nested"
              (let* ((beatles (make-person "The" "Beatles"))
                     (abbey-road (make-cd beatles "Abbey Road"))
                     (deutsche-grammatik
                      (make-book (make-person "Jacob" "Grimm") "Deutsche Grammatik"))
                     (sonnets (make-book (make-person "William" "Shakespeare") "Sonnets"))
                     (mnd (make-book (make-person "William" "Shakespeare")
                                     "A Midsummer Night’s Dream"))
                     (bob (make-cd (make-person "Bob" "Dylan") "Blonde on Blonde"))
                     (revolver (make-cd (make-person "The" "Beatles") "Revolver")))
                (test-equal
                 (list deutsche-grammatik
                       mnd
                       sonnets
                       abbey-road
                       revolver
                       bob)
                 (list-sort
                  (lambda (a b) (<? item-comparator a b))
                  (list abbey-road
                        deutsche-grammatik
                        sonnets
                        mnd
                        bob
                        revolver)))))
  ) ; end comparators
