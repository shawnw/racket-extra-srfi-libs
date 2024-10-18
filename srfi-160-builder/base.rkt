#lang racket/base

(require racket/contract
  ;; SRFI 4 versions of @vector->list don't accept start/end args
         (except-in ffi/vector
                    u8vector->list s8vector->list u16vector->list s16vector->list
                    u32vector->list s32vector->list u64vector->list s64vector->list
                    f32vector->list f64vector->list)
         "private/complex.rkt"
         "private/flvector.rkt"
         "private/fxvector.rkt"
         "private/valid.rkt"
         "private/u8-vector2list.rkt"
         "private/s8-vector2list.rkt"
         "private/u16-vector2list.rkt"
         "private/s16-vector2list.rkt"
         "private/u32-vector2list.rkt"
         "private/s32-vector2list.rkt"
         "private/u64-vector2list.rkt"
         "private/s64-vector2list.rkt"
         "private/f32-vector2list.rkt"
         "private/f64-vector2list.rkt"
         "private/c64-vector2list.rkt"
         "private/c128-vector2list.rkt"
         "private/fl-vector2list.rkt"
         "private/fx-vector2list.rkt")
(module+ test (require "../private/testwrappers.rkt"))

(provide
 (all-from-out ffi/vector)
 (all-from-out "private/complex.rkt")
 (all-from-out "private/flvector.rkt")
 (all-from-out "private/fxvector.rkt")
 (all-from-out "private/valid.rkt")
 (contract-out
  [u8vector->list (->* (u8vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof byte?))]
  [s8vector->list (->* (s8vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof fixnum?))]
  [u16vector->list (->* (u16vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof exact-nonnegative-integer?))]
  [s16vector->list (->* (s16vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof fixnum?))]
  [u32vector->list (->* (u32vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof exact-nonnegative-integer?))]
  [s32vector->list (->* (s32vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof exact-integer?))]
  [u64vector->list (->* (u64vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof exact-nonnegative-integer?))]
  [s64vector->list (->* (s64vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof exact-integer?))]
  [f32vector->list (->* (f32vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof flonum?))]
  [f64vector->list (->* (f64vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof double-flonum?))]
  [c64vector->list (->* (c64vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof complex?))]
  [c128vector->list (->* (c128vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof complex?))]
  [flvector->list (->* (flvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof flonum?))]
  [fxvector->list (->* (fxvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof fixnum?))]))

(module+ unsafe
  (require
   racket/unsafe/ops
   (only-in ffi/vector
            make-u8vector u8vector u8vector? list->u8vector
            make-s8vector s8vector s8vector? list->s8vector
            s8vector-ref s8vector-set! s8vector-length
            make-u16vector u16vector u16vector? list->u16vector
            u16vector-length
            make-s16vector s16vector s16vector? list->s16vector
            s16vector-length
            make-u32vector u32vector u32vector? list->u32vector
            u32vector-ref u32vector-set! u32vector-length
            make-s32vector s32vector s32vector? list->s32vector
            s32vector-ref s32vector-set! s32vector-length
            make-u64vector u64vector u64vector? list->u64vector
            u64vector-ref u64vector-set! u64vector-length
            make-s64vector s64vector s64vector? list->s64vector
            s64vector-ref s64vector-set! s64vector-length
            make-f32vector f32vector f32vector? list->f32vector
            f32vector-ref f32vector-set! f32vector-length
            make-f64vector f64vector f64vector? list->f64vector
            f64vector-length)
   (submod "private/complex.rkt" unsafe)
   (submod "private/flvector.rkt" unsafe)
   (submod "private/fxvector.rkt" unsafe)
   (submod "private/valid.rkt" unsafe))
  (provide
   (all-from-out ffi/vector)
   (rename-out
    [unsafe-bytes-ref u8vector-ref]
    [unsafe-bytes-set! u8vector-set!]
    [unsafe-bytes-length u8vector-length]
    [unsafe-u16vector-ref u16vector-ref]
    [unsafe-u16vector-set! u16vector-set!]
    [unsafe-s16vector-ref s16vector-ref]
    [unsafe-s16vector-set! s16vector-set!]
    [unsafe-f64vector-ref f64vector-ref]
    [unsafe-f64vector-set! f64vector-set!])
   (all-from-out (submod "private/complex.rkt" unsafe))
   (all-from-out (submod "private/flvector.rkt" unsafe))
   (all-from-out (submod "private/fxvector.rkt" unsafe))
   (all-from-out (submod "private/valid.rkt" unsafe))
   u8vector->list s8vector->list
   u16vector->list s16vector->list
   u32vector->list s32vector->list
   u64vector->list s64vector->list
   f32vector->list f64vector->list
   c64vector->list c128vector->list
   flvector->list
   fxvector->list
   ))

(module+ test
  ;;;; Shared tests
  ;;; Hvector = homogeneous vector

  (define-syntax-rule (is-same? expr1 expr2)
    (test-true (same? expr1 expr2)))

  ;; Test for sameness

  (define relerr (expt 2 -24))
  (define (inexact-real? x) (and (number? x) (inexact? x) (real? x)))
  (define (inexact-complex? x) (and (number? x) (inexact? x) (not (real? x))))
  (define (realify z) (* (real-part z) (imag-part z)))

  (define (same? result expected)
    (cond
      ((and (inexact-real? result) (inexact-real? expected))
       (let ((abserr (abs (* expected relerr))))
         (<= (- expected abserr) result (+ expected abserr))))
      ((and (inexact-complex? result) (inexact-complex? expected))
       (let ((abserr (abs (* (realify expected) relerr))))
         (<= (- (realify expected) abserr) (realify result) (+ (realify expected) abserr))))
      ((and (number? result) (number? expected))
       (= result expected))
      ((and (pair? result) (pair? expected))
       (list-same? result expected))
      (else
       (equal? result expected))))

  (define (list-same? result expected)
    (cond
      ((and (null? result) (null? expected))
       #t)
      ((and (pair? result) (pair? expected))
       (and (same? (car result) (car expected)) (list-same? (cdr result) (cdr expected))))
      (else
       #f)))

  (define (test tag make-Hvector Hvector Hvector? Hvector-length
                Hvector-ref Hvector-set! Hvector->list list->Hvector)
    ;(display "STARTING ")
    ;(display tag)
    ;(display "vector TESTS:")
    ;(newline)
    (let* ((first 32.0)
           (second 32.0+47.0i)
           (third -47.0i)
           (vec0 (make-Hvector 3))
           (vec1 (make-Hvector 3 second))
           (vec2 (Hvector first second third))
           (vec3 (list->Hvector (list third second first))))
      (is-same? (Hvector? vec0) #t)
      (is-same? (Hvector? vec1) #t)
      (is-same? (Hvector? vec2) #t)
      (is-same? (Hvector? vec3) #t)
      (is-same? (Hvector-length vec0) 3)
      (is-same? (Hvector-length vec1) 3)
      (is-same? (Hvector-length vec2) 3)
      (is-same? (Hvector-length vec3) 3)
      (Hvector-set! vec0 0 second)
      (Hvector-set! vec0 1 third)
      (Hvector-set! vec0 2 first)
      (is-same? (Hvector-ref vec0 0) second)
      (is-same? (Hvector-ref vec0 1) third)
      (is-same? (Hvector-ref vec0 2) first)
      (is-same? (Hvector-ref vec1 0) second)
      (is-same? (Hvector-ref vec1 1) second)
      (is-same? (Hvector-ref vec1 2) second)
      (is-same? (Hvector-ref vec2 0) first)
      (is-same? (Hvector-ref vec2 1) second)
      (is-same? (Hvector-ref vec2 2) third)
      (is-same? (Hvector-ref vec3 0) third)
      (is-same? (Hvector-ref vec3 1) second)
      (is-same? (Hvector-ref vec3 2) first)
      (is-same? (Hvector->list vec0) (list second third first))
      (is-same? (Hvector->list vec1) (list second second second))
      (is-same? (Hvector->list vec2) (list first second third))
      (is-same? (Hvector->list vec3) (list third second first))))

  (test 'c64 make-c64vector c64vector c64vector? c64vector-length
        c64vector-ref c64vector-set! c64vector->list list->c64vector)

  (test 'c128 make-c128vector c128vector c128vector? c128vector-length
        c128vector-ref c128vector-set! c128vector->list list->c128vector)

  (define-syntax integral-tests
    (syntax-rules ()
      ((integral-tests pred lo hi)
       (begin
         (test-not (pred 1/2))
         (test-not (pred 1.0))
         (test-not (pred 1+2i))
         (test-not (pred 1.0+2.0i))
         (test-assert (pred 0))
         (test-assert (pred hi))
         (test-assert (pred lo))
         (test-not (pred (+ hi 1)))
         (test-not (pred (- lo 1)))))))

  ;(display "STARTING @? TESTS")
  ;(newline)

  (integral-tests u8? 0 255)
  (integral-tests s8? -128 127)
  (integral-tests u16? 0 65535)
  (integral-tests s16? -32768 32767)
  (integral-tests u32? 0 4294967295)
  (integral-tests s32? -2147483648 2147483647)
  (integral-tests u64? 0 18446744073709551615)
  (integral-tests s64? -9223372036854775808 9223372036854775807)

  (test-assert (f32? 1.0))
  (test-not (f32? 1))
  (test-not (f32? 1.0+2.0i))

  (test-assert (f64? 1.0))
  (test-not (f64? 1))
  (test-not (f64? 1.0+2.0i))

  (test-assert (c64? 1.0))
  (test-not (c64? 1))
  (test-assert (c64? 1.0+2.0i))

  (test-assert (c128? 1.0))
  (test-not (c128? 1))
  (test-assert (c128? 1.0+2.0i))

  )
