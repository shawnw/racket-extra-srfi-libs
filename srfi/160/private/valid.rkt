#lang racket/base

(provide u8? s8? u16? s16? u32? s32? u64? s64? f32? f64? c64? c128? fl?)

(define (u8? n) (byte? n))

(define (s8? n) (and (fixnum? n) (<= -128 n 127)))

(define (u16? n) (and (fixnum? n) (<= 0 n 65535)))

(define (s16? n) (and (fixnum? n) (<= -32768 n 32767)))

(define (u32? n) (and (exact-integer? n) (<= 0 n 4294967295)))

(define (s32? n) (and (exact-integer? n) (<= -2147483648 n 2147483647)))

(define (u64? n) (and (exact-integer? n) (<= 0 n 18446744073709551615)))

(define (s64? n) (and (exact-integer? n) (<= -9223372036854775808 n 9223372036854775807)))

(define (f32? n) (flonum? n))

(define (f64? n) (double-flonum? n))

(define (c64? n) (and (complex? n)
                      (flonum? (real-part n))
                      (or (zero? (imag-part n))
                          (flonum? (imag-part n)))))

(define (c128? n) (and (complex? n)
                      (double-flonum? (real-part n))
                      (or (zero? (imag-part n))
                          (double-flonum? (imag-part n)))))

(define (fl? n) (flonum? n))
