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

(require racket/contract racket/set racket/treelist "../210.rkt")

(provide
 reducer/c transducer/c
 reduced? reduced unreduce ensure-reduced preserving-reduced
 list-reduce vector-reduce string-reduce bytes-reduce port-reduce generator-reduce
 bytevector-u8-reduce set-reduce treelist-reduce hash-reduce sequence-reduce
 )

(define reducer/c
  (case-> (-> any/c)
          (-> any/c any/c)
          (-> any/c any/c any/c)))

(define transducer/c (-> reducer/c reducer/c))

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

(define (set-reduce f identity set)
  (let loop ([set set]
             [acc identity])
    (if (set-empty? set)
        acc
        (let* ([val (set-first set)]
               [acc (f acc val)])
          (if (reduced? acc)
              (unreduce acc)
              (loop (set-rest set) acc))))))

(define (treelist-reduce f identity tl)
  (let ((len (treelist-length tl)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (treelist-ref tl i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (hash-reduce f identity ht)
  (let loop ([it (hash-iterate-first ht)]
             [acc identity])
    (if it
        (let ([acc (f acc (hash-iterate-pair ht it))])
          (if (reduced? acc)
              (unreduce acc)
              (loop (hash-iterate-next ht it) acc)))
        acc)))

(define (sequence-reduce f identity seq)
  (define (reducer acc vals next)
    (if vals
        (let ([acc (f acc (car vals))])
          (if (reduced? acc)
              (unreduce acc)
              (apply/mv reducer acc (next))))
        acc))
  (apply/mv reducer identity (sequence-generate* seq)))
