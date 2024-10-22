#lang racket/base

;; SRFI-171 meta sublibrary


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Linus Björnstam
;;
;; You may use this code under either the license in the SRFI document or the
;; license below.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ported to Racket by Shawn Wagner

(require racket/contract racket/contract/combinator (only-in "../158.rkt" generator?))

(provide
 reducer/c reducer-any/c reduced/c
 transducer/c transducer-any/c
 preserving-reduced
 bytevector-u8-reduce
 (contract-out
  [reduced? predicate/c]
  [reduced (-> any/c reduced?)]
  [unreduce (-> reduced? any/c)]
  [ensure-reduced (-> any/c reduced?)]

  [list-reduce (-> reducer/c any/c list? any/c)]
  [vector-reduce (-> reducer/c any/c vector? any/c)]
  [string-reduce (-> reducer/c any/c string? any/c)]
  [bytes-reduce (-> reducer/c any/c bytes? any/c)]
  [port-reduce (-> reducer/c any/c (-> any/c any/c) any/c any/c)]
  [generator-reduce (-> reducer/c any/c generator? any/c)]
  ))

(define (reducer/c result/c [input/c any/c])
  (case-> (-> result/c)
          (-> result/c result/c)
          (-> result/c input/c result/c)))
(define reducer-any/c (reducer/c any/c))

(define (transducer/c result/c [input/c any/c])
  (-> (reducer/c result/c input/c) (reducer/c result/c input/c)))
(define transducer-any/c (transducer/c any/c))

;; contract to test if value is a reduced value that matches a given contract
(define (reduced/c raw-contents/c)
  (define contents/c (coerce-contract 'reduced/c raw-contents/c))
  (make-contract
   #:name (build-compound-type-name 'reduced/c contents/c)
   #:first-order (lambda (obj) (and (reduced? obj) (contract-first-order-passes? contents/c (reduced-value obj))))))

(struct reduced (value) #:sealed)

(define (unreduce r) (reduced-value r))

;; helper function which ensures x is reduced.
(define (ensure-reduced value)
  (if (reduced? value)
      value
      (reduced value)))

;; helper function that wraps a reduced value twice since reducing functions (like list-reduce)
;; unwraps them. tconcatenate is a good example: it re-uses it's reducer on it's input using list-reduce.
;; If that reduction finishes early and returns a reduced value, list-reduce would "unreduce"
;; that value and try to continue the transducing process.
(define (preserving-reduced reducer)
  (case-lambda
    [() (raise-arguments-error 'preserving-reduced "called with no arguments")]
    [(a) (raise-arguments-error 'preserving-reduced "called with one argument")]
    [(a b)
     (let ((return (reducer a b)))
       (if (reduced? return)
           (reduced return)
           return))]))


;; This is where the magic tofu is cooked
(define (list-reduce f identity lst)
  (if (null? lst)
      identity
      (let ((v (f identity (car lst))))
        (if (reduced? v)
            (unreduce v)
            (list-reduce f v (cdr lst))))))

(define (vector-reduce f identity vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (vector-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (string-reduce f identity str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (string-ref str i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (bytes-reduce f identity vec)
  (let ((len (bytes-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (bytes-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))
(define bytevector-u8-reduce bytes-reduce)

(define (port-reduce f identity reader port)
  (let loop ((val (reader port)) (acc identity))
    (if (eof-object? val)
        acc
        (let ((acc (f acc val)))
          (if (reduced? acc)
              (unreduce acc)
              (loop (reader port) acc))))))

(define (generator-reduce f identity gen)
  (let loop ((val (gen)) (acc identity))
    (if (eof-object? val)
        acc
        (let ((acc (f acc val)))
          (if (reduced? acc)
              (unreduce acc)
              (loop (gen) acc))))))
