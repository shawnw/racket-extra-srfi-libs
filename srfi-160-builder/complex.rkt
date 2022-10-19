#lang racket/base
;;;; Implementation of SRFI 160 base c64vectors and c128vectors

(require
 racket/contract
 ;(only-in racket/unsafe/ops unsafe-f64vector-ref unsafe-f64vector-set!)
 (only-in racket/flonum make-flrectangular)
 (only-in ffi/vector
          make-f32vector make-f64vector
          f32vector-ref f64vector-ref
          f32vector-set! f64vector-set!
          f32vector-length f64vector-length))
(provide
 (contract-out
  [c64vector? predicate/c]
  [c128vector? predicate/c]
  [make-c64vector (->* (exact-integer?) (complex?) c64vector?)]
  [make-c128vector (->* (exact-integer?) (complex?) c128vector?)]
  [c64vector (-> complex? ... c64vector?)]
  [c128vector (-> complex? ... c128vector?)]
  [c64vector-length (-> c64vector? exact-nonnegative-integer?)]
  [c128vector-length (-> c128vector? exact-nonnegative-integer?)]
  [c64vector-ref (-> c64vector? exact-nonnegative-integer? complex?)]
  [c128vector-ref (-> c128vector? exact-nonnegative-integer? complex?)]
  [c64vector-set! (-> c64vector? exact-nonnegative-integer? complex? void?)]
  [c128vector-set! (-> c128vector? exact-nonnegative-integer? complex? void?)]
  [list->c64vector (-> (listof complex?) c64vector?)]
  [list->c128vector (-> (listof complex?) c128vector?)]))

(define (fl n) (real->double-flonum n))

;;; Main constructor

(struct c64vector (bv)
  #:name raw-c64vector
  #:constructor-name raw-make-c64vector
  #:transparent)
(struct c128vector (bv)
  #:name raw-c128vector
  #:constructor-name raw-make-c128vector
  #:transparent)

(define (make-c64vector len . maybe-fill)
  (define vec (raw-make-c64vector (make-f32vector (* len 2))))
  (when (not (null? maybe-fill))
    (c64vector-simple-fill! vec (car maybe-fill)))
  vec)

(define (make-c128vector len . maybe-fill)
  (define vec (raw-make-c128vector (make-f64vector (* len 2))))
  (when (not (null? maybe-fill))
    (c128vector-simple-fill! vec (car maybe-fill)))
  vec)

;; Simple fill! (not exported)

(define (c64vector-simple-fill! vec value)
  (define len (c64vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (c64vector-set! vec i value)
        (loop (+ i 1))))))

(define (c128vector-simple-fill! vec value)
  (define len (c128vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (c128vector-set! vec i value)
        (loop (+ i 1))))))

;;; Variable-argument constructor

(define (c64vector . list)
  (list->c64vector list))

(define (c128vector . list)
  (list->c128vector list))

;; Predicate already defined

;; Length

(define (c64vector-length vec)
  (/ (f32vector-length (c64vector-bv vec)) 2))

(define (c128vector-length vec)
  (/ (f64vector-length (c128vector-bv vec)) 2))

;; Get element

(define (c64vector-ref vec i)
  (let ((fvec (c64vector-bv vec))
        (j (* i 2)))
    (make-flrectangular
      (f32vector-ref fvec j)
      (f32vector-ref fvec (+ j 1)))))

(define (c128vector-ref vec i)
  (let ((fvec (c128vector-bv vec))
        (j (* i 2)))
    (make-flrectangular
      (f64vector-ref fvec j)
      (f64vector-ref fvec (+ j 1)))))

;; Set element

(define (c64vector-set! vec i value)
  (let ((fvec (c64vector-bv vec))
        (j (* i 2)))
    (f32vector-set! fvec j (fl (real-part value)))
    (f32vector-set! fvec (+ j 1) (fl (imag-part value)))))

(define (c128vector-set! vec i value)
  (let ((fvec (c128vector-bv vec))
        (j (* i 2)))
    (f64vector-set! fvec j (fl (real-part value)))
    (f64vector-set! fvec (+ j 1) (fl (imag-part value)))))

;; List to vec

(define (list->c64vector list)
  (define len (length list))
  (define vec (make-c64vector len))
  (let loop ((i 0) (list list))
    (if (= i len)
      vec
      (begin
        (c64vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

(define (list->c128vector list)
  (define len (length list))
  (define vec (make-c128vector len))
  (let loop ((i 0) (list list))
    (if (= i len)
      vec
      (begin
        (c128vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

;; Vec to list defined in at-vector2list
