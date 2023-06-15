#lang racket/base

(require racket/contract racket/unsafe/ops)

(provide
 (contract-out
  [u8? predicate/c]
  [s8? predicate/c]
  [u16? predicate/c]
  [s16? predicate/c]
  [u32? predicate/c]
  [s32? predicate/c]
  [u64? predicate/c]
  [s64? predicate/c]
  [f32? predicate/c]
  [f64? predicate/c]
  [c64? predicate/c]
  [c128? predicate/c]
  [fl? predicate/c]
  [fx? predicate/c]))

(module+ unsafe
  (provide u8? s8? u16? s16? u32? s32? u64? s64? f32? f64?
           c64? c128? fl? fx?))

(define u8? (procedure-rename byte? 'u8?))

(define (s8? n) (and (fixnum? n) (unsafe-fx<= -128 n 127)))

(define (u16? n) (and (fixnum? n) (unsafe-fx<= 0 n 65535)))

(define (s16? n) (and (fixnum? n) (unsafe-fx<= -32768 n 32767)))

(define (u32? n) (and (exact-integer? n) (<= 0 n 4294967295)))

(define (s32? n) (and (exact-integer? n) (<= -2147483648 n 2147483647)))

(define (u64? n) (and (exact-integer? n) (<= 0 n 18446744073709551615)))

(define (s64? n) (and (exact-integer? n) (<= -9223372036854775808 n 9223372036854775807)))

(define f32? (procedure-rename flonum? 'f32?))

(define f64? (procedure-rename double-flonum? 'f64?))

(define (c64? n) (and (complex? n)
                      (flonum? (real-part n))
                      (or (zero? (imag-part n))
                          (flonum? (imag-part n)))))

(define (c128? n) (and (complex? n)
                      (double-flonum? (real-part n))
                      (or (zero? (imag-part n))
                          (double-flonum? (imag-part n)))))

(define fl? (procedure-rename flonum? 'fl?))
(define fx? (procedure-rename fixnum? 'fx?))
