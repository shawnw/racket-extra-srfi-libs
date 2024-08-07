#lang racket/base
;;; SRFI-208: NaN Procedures

;;; Original code by Shawn; Released under the MIT and Apache
;;; licenses; your choice.


;;; For now, only support double flonums; bc supports single flonums too, but
;;; it's legacy. Won't add unless requested. Consider adding extflonum support
;;; too, when I can find a Racket build that supports it - my x86-64 one
;;; reports (extflonum-available?) as #f

(require racket/require racket/contract (only-in math/flonum flnan?)
         (for-syntax racket/base (only-in racket/string string-prefix?)))
(require (filtered-in
          (lambda (name)
            (and (or
                  (string-prefix? name "unsafe-fx")
                  (string-prefix? name "unsafe-bytes"))
                 (substring name 7)))
            racket/unsafe/ops))

(module+ test
  (require rackunit))

(define nan/c (and/c flonum? flnan?))
(provide
 (contract-out
  [make-nan (->* (boolean? boolean? exact-nonnegative-integer?) (real?) nan/c)]
  [nan-negative? (-> nan/c boolean?)]
  [nan-quiet? (-> nan/c boolean?)]
  [nan-payload (-> nan/c exact-nonnegative-integer?)]
  [nan=? (-> nan/c nan/c boolean?)]))

;;; Use a per-thread byte buffer to hold representations of numbers
;;; instead of allocating ones on every call.
(define byte-buffer (make-thread-cell #f))
(define (get-buffer)
  (let ([buffer (thread-cell-ref byte-buffer)])
    (cond
      ((bytes? buffer) buffer)
      (else
       (let ([buf (make-bytes 16)])
         (thread-cell-set! byte-buffer buf)
         buf)))))

(define (make-nan negative? quiet? payload [float 0.0])
  (let ([bv (get-buffer)])
    (cond
      ((not (double-flonum? float))
       (raise-argument-error 'make-nan "double-flonum?" float))
      ((or (< payload 0)
           (> payload (- (arithmetic-shift 1 51) 1)))
       (raise-argument-error 'make-nan "(integer-in 0 (- (arithmetic-shift 1 51) 1))" payload))
      ((and (= payload 0) (not quiet?))
       (raise-argument-error 'make-nan "(and/c (not/c quiet?) (>/c 0))" payload))
      (else
       (if negative?
           (bytes-set! bv 0 #b11111111)
           (bytes-set! bv 0 #b01111111))
       (if quiet?
           (bytes-set! bv 1 #b11111000)
           (bytes-set! bv 1 #b11110000))
       (integer->integer-bytes payload 8 #f #t bv 8)
       (bytes-set! bv 1 (fxior (bytes-ref bv 1)
                               (fxand (bytes-ref bv 9) #b00000111)))
       (bytes-copy! bv 2 bv 10 16)
       (floating-point-bytes->real bv #t 0 8)))))

(define (nan-negative? nan)
  (let ([bv (get-buffer)])
    (real->floating-point-bytes nan 8 #t bv)
    (fx> (fxand (bytes-ref bv 0) #b10000000) 0)))

(define (nan-quiet? nan)
  (let ([bv (get-buffer)])
    (real->floating-point-bytes nan 8 #t bv)
    (fx> (fxand (bytes-ref bv 1) #b00001000) 0)))

(define (nan-payload nan)
  (let ([bv (get-buffer)])
    (real->floating-point-bytes nan 8 #t bv)
    (bytes-set! bv 8 0)
    (bytes-set! bv 9 (fxand (bytes-ref bv 1) #b00000111))
    (bytes-copy! bv 10 bv 2 8)
    (integer-bytes->integer bv #f #t 8 16)))

(define (nan=? a b)
  (let ([bv (get-buffer)])
    (real->floating-point-bytes a 8 (system-big-endian?) bv 0)
    (real->floating-point-bytes b 8 (system-big-endian?) bv 8)
    (for/and ([i (in-range 8)])
      (fx= (bytes-ref bv i) (bytes-ref bv (+ i 8))))))

(module+ test
  (define q+NaN (make-nan #f #t 0))
  (define s+NaN (make-nan #f #f 123456))
  (define q-NaN (make-nan #t #t 1))
  (define s-NaN (make-nan #t #f 2))
  (check-false (nan-negative? q+NaN))
  (check-false (nan-negative? s+NaN))
  (check-true (nan-negative? q-NaN))
  (check-true (nan-negative? s-NaN))
  (check-true (nan-quiet? +nan.0))
  (check-true (nan-quiet? q-NaN))
  (check-false (nan-quiet? s-NaN))
  (check-equal? (nan-payload q+NaN) 0)
  (check-equal? (nan-payload s+NaN) 123456)
  (check-equal? (nan-payload +nan.0) 0)
  (check-true (nan=? q+NaN +nan.0))
  (check-false (nan=? q+NaN q-NaN)))
