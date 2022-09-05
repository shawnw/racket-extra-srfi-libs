#lang typed/racket/base
;;; -*- Mode: Scheme -*-

;;;; Integer Division Operators

;;; Given a QUOTIENT and REMAINDER defined for nonnegative numerators
;;; and positive denominators implementing the truncated, floored, or
;;; Euclidean integer division, this implements a number of other
;;; integer division operators.

;;; Copyright (c) 2010--2011 Taylor R. Campbell
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(provide
 floor/ floor-quotient floor-remainder
 ceiling/ ceiling-quotient ceiling-remainder
 truncate/ truncate-quotient truncate-remainder
 round/ round-quotient round-remainder
 euclidean/ euclidean-quotient euclidean-remainder
 balanced/ balanced-quotient balanced-remainder)

;;;; Shims
;;; SRFI-8
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

;;; exact-integer?
;(define (exact-integer? x) (and (integer? x) (exact? x)))

;;;; Integer Division

;;;; Ceiling

(: ceiling/ (-> Integer Integer (Values Integer Integer)))
(define (ceiling/ n d)
  (cond ((and (negative? n) (negative? d))
         (ceiling-/- n d))
        ((negative? n)
         (let ((n (- 0 n)))
           (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
        ((negative? d)
         (let ((d (- 0 d)))
           (values (- 0 (quotient n d)) (remainder n d))))
        (else
         (ceiling+/+ n d))))

(: ceiling-/- (-> Integer Integer (Values Integer Integer)))
(define (ceiling-/- n d)
  (let ((n (- 0 n)) (d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values q r)
          (values (+ q 1) (- d r))))))

(: ceiling+/+ (-> Integer Integer (Values Integer Integer)))
(define (ceiling+/+ n d)
  (let ((q (quotient n d)) (r (remainder n d)))
    (if (zero? r)
        (values q r)
        (values (+ q 1) (- r d)))))

(: ceiling-quotient (-> Integer Integer Integer))
(define (ceiling-quotient n d)
  (cond ((and (negative? n) (negative? d))
         (receive (q r) (ceiling-/- n d) r q))
        ((negative? n) (- 0 (quotient (- 0 n) d)))
        ((negative? d) (- 0 (quotient n (- 0 d))))
        (else (receive (q r) (ceiling+/+ n d) r q))))

(: ceiling-remainder (-> Integer Integer Integer))
(define (ceiling-remainder n d)
  (cond ((and (negative? n) (negative? d))
         (receive (q r) (ceiling-/- n d) q r))
        ((negative? n) (- 0 (remainder (- 0 n) d)))
        ((negative? d) (remainder n (- 0 d)))
        (else (receive (q r) (ceiling+/+ n d) q r))))

;;;; Euclidean Division

;;; 0 <= r < |d|

(: euclidean/ (-> Integer Integer (Values Integer Integer)))
(define (euclidean/ n d)
  (cond ((and (negative? n) (negative? d)) (ceiling-/- n d))
        ((negative? n) (floor-/+ n d))
        ((negative? d)
         (let ((d (- 0 d)))
           (values (- 0 (quotient n d)) (remainder n d))))
        (else (values (quotient n d) (remainder n d)))))

(: euclidean-quotient (-> Integer Integer Integer))
(define (euclidean-quotient n d)
  (cond ((and (negative? n) (negative? d))
         (receive (q r) (ceiling-/- n d) r q))
        ((negative? n) (receive (q r) (floor-/+ n d) r q))
        ((negative? d) (- 0 (quotient n (- 0 d))))
        (else (quotient n d))))

(: euclidean-remainder (-> Integer Integer Integer))
(define (euclidean-remainder n d)
  (cond ((and (negative? n) (negative? d))
         (receive (q r) (ceiling-/- n d) q r))
        ((negative? n) (receive (q r) (floor-/+ n d) q r))
        ((negative? d) (remainder n (- 0 d)))
        (else (remainder n d))))

;;;; Floor

(: floor/ (-> Integer Integer (Values Integer Integer)))
(define (floor/ n d)
  (cond ((and (negative? n) (negative? d))
         (let ((n (- 0 n)) (d (- 0 d)))
           (values (quotient n d) (- 0 (remainder n d)))))
        ((negative? n) (floor-/+ n d))
        ((negative? d) (floor+/- n d))
        (else (values (quotient n d) (remainder n d)))))

(: floor-/+ (-> Integer Integer (Values Integer Integer)))
(define (floor-/+ n d)
  (let ((n (- 0 n)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- d r))))))

(: floor+/- (-> Integer Integer (Values Integer Integer)))
(define (floor+/- n d)
  (let ((d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- r d))))))

(: floor-quotient (-> Integer Integer Integer))
(define (floor-quotient n d)
  (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
        ((negative? n) (receive (q r) (floor-/+ n d) r q))
        ((negative? d) (receive (q r) (floor+/- n d) r q))
        (else (quotient n d))))

(: floor-remainder (-> Integer Integer Integer))
(define (floor-remainder n d)
  (cond ((and (negative? n) (negative? d))
         (- 0 (remainder (- 0 n) (- 0 d))))
        ((negative? n) (receive (q r) (floor-/+ n d) q r))
        ((negative? d) (receive (q r) (floor+/- n d) q r))
        (else (remainder n d))))

;;;; Round Ties to Even

(: round/ (-> Integer Integer (Values Integer Integer)))
(define (round/ n d)
  (: divide (-> Integer Integer (-> Integer Integer (Values Integer Integer)) (-> Integer Integer (Values Integer Integer)) (Values Integer Integer)))
  (define (divide n d adjust leave)
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (and (not (zero? r))
               (or (and (odd? q) (even? d) (divisible? n (quotient d 2)))
                   (< d (* 2 r))))
          (adjust (+ q 1) (- r d))
          (leave q r))))
  (cond ((and (negative? n) (negative? d))
         (divide (- 0 n) (- 0 d)
                 (lambda (q r) (values q (- 0 r)))
                 (lambda (q r) (values q (- 0 r)))))
        ((negative? n)
         (divide (- 0 n) d
                 (lambda (q r) (values (- 0 q) (- 0 r)))
                 (lambda (q r) (values (- 0 q) (- 0 r)))))
        ((negative? d)
         (divide n (- 0 d)
                 (lambda (q r) (values (- 0 q) r))
                 (lambda (q r) (values (- 0 q) r))))
        (else
         (let ((return (lambda (q r) (values q r))))
           (divide n d return return)))))

(: divisible? (-> Integer Integer Boolean))
(define (divisible? n d)
  ;; This operation admits a faster implementation than the one given
  ;; here.
  (zero? (remainder n d)))

(: round-quotient (-> Integer Integer Integer))
(define (round-quotient n d)
  (receive (q r) (round/ n d)
    r                               ;ignore
    q))

(: round-remainder (-> Integer Integer Integer))
(define (round-remainder n d)
  (receive (q r) (round/ n d)
    q                               ;ignore
    r))

;;;; Truncate

(: truncate/ (-> Integer Integer (Values Integer Integer)))
(define (truncate/ n d)
  (cond ((and (negative? n) (negative? d))
         (let ((n (- 0 n)) (d (- 0 d)))
           (values (quotient n d) (- 0 (remainder n d)))))
        ((negative? n)
         (let ((n (- 0 n)))
           (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
        ((negative? d)
         (let ((d (- 0 d)))
           (values (- 0 (quotient n d)) (remainder n d))))
        (else
         (values (quotient n d) (remainder n d)))))

(: truncate-quotient (-> Integer Integer Integer))
(define (truncate-quotient n d)
  (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
        ((negative? n) (- 0 (quotient (- 0 n) d)))
        ((negative? d) (- 0 (quotient n (- 0 d))))
        (else (quotient n d))))

(: truncate-remainder (-> Integer Integer Integer))
(define (truncate-remainder n d)
  (cond ((and (negative? n) (negative? d))
         (- 0 (remainder (- 0 n) (- 0 d))))
        ((negative? n) (- 0 (remainder (- 0 n) d)))
        ((negative? d) (remainder n (- 0 d)))
        (else (remainder n d))))

;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: balanced/ (-> Integer Integer (Values Integer Integer)))
(define (balanced/ x y)
  (call-with-values
   (lambda () (euclidean/ x y))
   (lambda ([q : Integer] [r : Integer])
     (cond ((< r (abs (/ y 2)))
            (values q r))
           ((> y 0)
            (values (+ q 1) (- x (* (+ q 1) y))))
           (else
            (values (- q 1) (- x (* (- q 1) y))))))))

(: balanced-quotient (-> Integer Integer Integer))
(define (balanced-quotient x y)
  (call-with-values
   (lambda () (balanced/ x y))
   (lambda (q r) q)))

(: balanced-remainder (-> Integer Integer Integer))
(define (balanced-remainder x y)
  (call-with-values
   (lambda () (balanced/ x y))
   (lambda (q r) r)))
