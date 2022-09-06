#lang typed/racket/base
;;; SRFI-151 Bitwise Operations

(require/typed racket/unsafe/ops
  [unsafe-fxpopcount (-> Nonnegative-Fixnum Exact-Nonnegative-Integer)])


(module+ test (require typed/rackunit))

;;; In racket/base:
;;; bitwise-not bitwise-and bitwise-ior bitwise-xor arithmetic-shift integer-length
(provide ;(rename-in typed/racket/base [bitwise-bit-field bit-field])
         bit-count bitwise-nand bitwise-nor bitwise-andc1
         bitwise-andc2 bitwise-orc1 bitwise-orc2 bitwise-eqv bitwise-if
         bit-set? copy-bit bit-swap any-bit-set? every-bit-set?  first-set-bit
         bit-field bit-field-any? bit-field-every? bit-field-clear
         bit-field-set bit-field-replace bit-field-replace-same
         bit-field-rotate bit-field-reverse bits->list bits->vector list->bits
         vector->bits bits bitwise-fold bitwise-for-each bitwise-unfold
         make-bitwise-generator)

(: bit-set? (-> Exact-Nonnegative-Integer Integer Boolean))
(define (bit-set? index i)
  (bitwise-bit-set? i index))

(: bit-field (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field i start end)
  (bitwise-bit-field i start end))

;;;; bitwise-core, core bitwise operations
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(: bit-count (-> Integer Exact-Nonnegative-Integer))
(define bit-count
  (letrec ((logcnt : (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer)
                   (lambda ([n : Integer] [tot : Exact-Nonnegative-Integer]) : Exact-Nonnegative-Integer
                     (if (zero? n)
                         tot
                         (logcnt (quotient n 16)
                                 (+ (vector-ref
                                     '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                     (modulo n 16))
                                    tot))))))
    (lambda (n)
      (cond ((negative? n) (logcnt (bitwise-not n) 0))
            ((fixnum? n) (unsafe-fxpopcount n)) ; Racket specific
            ((positive? n) (logcnt n 0))
            (else 0)))))

;;;; bitwise-33 - Olin Shivers's code from SRFI-33 with modified names

;;; Olin Shivers is the sole author of this code, and he has placed it in
;;; the public domain.

(: bitwise-nand (-> Integer Integer Integer))
(define (bitwise-nand  i j)  (bitwise-not (bitwise-and i j)))
(: bitwise-nor (-> Integer Integer Integer))
(define (bitwise-nor   i j)  (bitwise-not (bitwise-ior i j)))
(: bitwise-andc1 (-> Integer Integer Integer))
(define (bitwise-andc1 i j)  (bitwise-and (bitwise-not i) j))
(: bitwise-andc2 (-> Integer Integer Integer))
(define (bitwise-andc2 i j)  (bitwise-and i (bitwise-not j)))
(: bitwise-orc1 (-> Integer Integer Integer))
(define (bitwise-orc1  i j)  (bitwise-ior (bitwise-not i) j))
(: bitwise-orc2 (-> Integer Integer Integer))
(define (bitwise-orc2  i j)  (bitwise-ior i (bitwise-not j)))

(: bitwise-eqv (->* () () #:rest Integer Integer))
(define (bitwise-eqv . args)
  (let lp ((args args) (ans -1))
    (if (pair? args)
        (lp (cdr args) (bitwise-not (bitwise-xor ans (car args))))
	ans)))

(: any-bit-set? (-> Integer Integer Boolean))
(define (any-bit-set? test-bits n) (not (zero? (bitwise-and test-bits n))))
(: every-bit-set? (-> Integer Integer Boolean))
(define (every-bit-set? test-bits n) (= test-bits (bitwise-and test-bits n)))

;;; Helper function -- make a mask of SIZE 1-bits, e.g. (%MASK 3) = #b111.
;;; Suppose your Scheme's fixnums are N bits wide (counting the sign bit,
;;; not counting any tag bits). This version, due to Marc Feeley, will
;;; handle SIZE in the range [0,N-1] without overflowing to bignums.
;;; (For SIZE >= N, the correct bignum value is also produced.)
(: mask (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (mask start end) (bitwise-not (arithmetic-shift -1 (- end start))))

(: bit-field-any? (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Boolean))
(define (bit-field-any? n start end)
  (not (zero? (bitwise-and (arithmetic-shift n (- start)) (mask start end)))))

;; Part of Olin's late revisions; code by John Cowan; public domain.
(: bit-field-every? (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Boolean))
(define (bit-field-every? n start end)
  (let ((m (mask start end)))
    (eqv? m (bitwise-and (arithmetic-shift n (- start)) m))))

;; Integrating i-b-f reduces nicely.
(: bit-field-clear (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-clear n start end)
  (bit-field-replace n 0 start end))

;; Counterpart to above, not in SRFI 33, written by John Cowan, public domain
(: bit-field-set (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-set n start end)
  (bit-field-replace n -1 start end))

;;; This three-line version won't fixnum-overflow on fixnum args.
(: bit-field-replace (-> Integer Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-replace n newfield start end)
  (let ((m (mask start end)))
    (bitwise-ior (bitwise-and n (bitwise-not (arithmetic-shift m start)))
		 (arithmetic-shift (bitwise-and newfield m) start))))

(: bit-field-replace-same (-> Integer Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-replace-same to from start end)
  (bitwise-if (arithmetic-shift (mask start end) start) from to))

;;; Clever definition, assuming you have a fast BIT-COUNT.
(: first-set-bit (-> Integer Integer))
(define (first-set-bit i) (- (bit-count (bitwise-xor i (- i 1))) 1))

;;;; bitwise-60 - SRFI-60 procedures without SRFI-33 analogues, renamed
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(: bitwise-if (-> Integer Integer Integer Integer))
(define (bitwise-if mask n0 n1)
  (bitwise-ior (bitwise-and mask n0)
               (bitwise-and (bitwise-not mask) n1)))

(: copy-bit (-> Exact-Nonnegative-Integer Integer Boolean Integer))
(define (copy-bit index to bool)
  (if bool
      (bitwise-ior to (arithmetic-shift 1 index))
      (bitwise-and to (bitwise-not (arithmetic-shift 1 index)))))

(: bit-field-rotate (-> Integer Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-rotate n count start end)
  (define width (- end start))
  (set! count (modulo count width))
  (let ((mask (bitwise-not (arithmetic-shift -1 width))))
    (define zn (bitwise-and mask (arithmetic-shift n (- start))))
    (bitwise-ior (arithmetic-shift
             (bitwise-ior (bitwise-and mask (arithmetic-shift zn count))
                     (arithmetic-shift zn (- count width)))
             start)
                 (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))

(: bit-reverse (-> Integer Integer Integer))
(define (bit-reverse k n)
  (do ((m (if (negative? n) (bitwise-not n) n) (arithmetic-shift m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (bitwise-ior (arithmetic-shift rvs 1) (bitwise-and 1 m))))
      ((negative? k) (if (negative? n) (bitwise-not rvs) rvs))))

(: bit-field-reverse (-> Integer Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer))
(define (bit-field-reverse n start end)
  (define width (- end start))
  (let ((mask (bitwise-not (arithmetic-shift -1 width))))
    (define zn (bitwise-and mask (arithmetic-shift n (- start))))
    (bitwise-ior (arithmetic-shift (bit-reverse width zn) start)
            (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))

(: bits->list (->* (Exact-Nonnegative-Integer) (Integer) (Listof Boolean)))
(define (bits->list k [len -1])
  (if (< len 0)
      (do ((k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((<= k 0) (cast (reverse lst) (Listof Boolean))))
      (do ((idx (+ -1 len) (+ -1 idx))
           (k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((negative? idx) (cast (reverse lst) (Listof Boolean))))))

(: list->bits (-> (Listof Boolean) Exact-Nonnegative-Integer))
(define (list->bits bools)
  (do ((bs (reverse bools) (cdr bs))
       (acc : Exact-Nonnegative-Integer 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))

(: bits (->* () () #:rest Boolean Exact-Nonnegative-Integer))
(define (bits . bools)
  (list->bits bools))

;;;; bitwise-other - functions not from SRFI 33 or SRFI 60
;;; Copyright John Cowan 2017

(: bit-swap (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer Integer Integer))
(define (bit-swap n1 n2 i)
  (let ((n1-bit (bit-set? n1 i))
        (n2-bit (bit-set? n2 i)))
    (copy-bit n2 (copy-bit n1 i n2-bit) n1-bit)))

(: bits->vector (case-> (-> Exact-Nonnegative-Integer (Vectorof Boolean))
                        (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer (Vectorof Boolean))))
(define bits->vector
  (case-lambda
    ((i) (list->vector (bits->list i)))
    ((i len) (list->vector (bits->list i len)))))

(: vector->bits (-> (Vectorof Boolean) Exact-Nonnegative-Integer))
(define (vector->bits vector) (list->bits (vector->list vector)))

(: bitwise-fold (All (a) (-> (-> Boolean a a) a Integer a)))
(define (bitwise-fold proc seed i)
  (let ((len (integer-length i)))
    (let loop ((n : Exact-Nonnegative-Integer 0) (r seed))
      (if (= n len)
        r
        (loop (+ n 1) (proc (bit-set? n i) r))))))

(: bitwise-for-each (-> (-> Boolean Any) Integer Void))
(define (bitwise-for-each proc i)
  (let ((len (integer-length i)))
    (let loop ((n : Exact-Nonnegative-Integer 0))
      (when (< n len)
        (proc (bit-set? n i))
        (loop (+ n 1))))))

(: bitwise-unfold (-> (-> Exact-Nonnegative-Integer Any) (-> Exact-Nonnegative-Integer Any) (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer) Exact-Nonnegative-Integer Integer))
(define (bitwise-unfold stop? mapper successor seed)
  (let loop ((n : Exact-Nonnegative-Integer 0) (result : Integer 0) (state : Exact-Nonnegative-Integer seed))
    (if (stop? state)
      result
        (loop (+ n 1)
              (copy-bit n result (if (mapper state) #t #f))
              (successor state)))))

(: make-bitwise-generator (-> Integer (-> Boolean)))
(define (make-bitwise-generator i)
  (lambda ()
    (let ((bit (bit-set? 0 i)))
       (set! i (arithmetic-shift i -1))
       bit)))

(module+ test
  (test-equal? "test-1" -1 (bitwise-not 0))
  (test-equal? "test-122" 0 (bitwise-not -1))
  (test-equal? "test-248" -11 (bitwise-not 10))
  (test-equal? "test-249" 36 (bitwise-not -37))
  (test-equal? "test-2" 0 (bitwise-and #b0 #b1))
  (test-equal? "test-10" 1680869008 (bitwise-and -193073517 1689392892))
  (test-equal? "test-20" 3769478 (bitwise-and 1694076839 -4290775858))
  (test-equal? "test-115" 6 (bitwise-and 14 6))
  (test-equal? "test-251" 10 (bitwise-and 11 26))
  (test-equal? "test-254" 4 (bitwise-and 37 12))
  (test-equal? "test-288" 1 (bitwise-and #b1 #b1))
  (test-equal? "test-289" 0 (bitwise-and #b1 #b10))
  (test-equal? "test-290" #b10 (bitwise-and #b11 #b10))
  (test-equal? "test-291" #b101 (bitwise-and #b101 #b111))
  (test-equal? "test-292" #b111 (bitwise-and -1 #b111))
  (test-equal? "test-293" #b110 (bitwise-and -2 #b111))
  (test-equal? "test-294" 3769478 (bitwise-and -4290775858 1694076839))
  (test-equal? "test-11" -4294967295 (bitwise-ior 1 (- -1 #xffffffff)))
  (test-equal? "test-12" -18446744073709551615 (bitwise-ior 1 (- -1 #xffffffffffffffff)))
  (test-equal? "test-117" 14 (bitwise-ior 10 12))
  (test-equal? "test-250" 11 (bitwise-ior 3  10))
  (test-equal? "test-13" -4294967126 (bitwise-xor #b10101010 (- -1 #xffffffff)))
  (test-equal? "test-15" -18446744073709551446 (bitwise-xor #b10101010 (- -1 #xffffffffffffffff)))
  (test-equal? "test-16" -2600468497 (bitwise-ior 1694076839 -4290775858))
  (test-equal? "test-17" -184549633 (bitwise-ior -193073517 1689392892))
  (test-equal? "test-18" -2604237975 (bitwise-xor 1694076839 -4290775858))
  (test-equal? "test-19" -1865418641 (bitwise-xor -193073517 1689392892))
  (test-equal? "test-119" 6 (bitwise-xor 10 12))
  (test-equal? "test-252" 9 (bitwise-xor 3 10))
  (test-equal? "test-14" (bitwise-not -4294967126) (bitwise-eqv #b10101010 (- -1 #xffffffff)))
  (test-equal? "test-253" -42 (bitwise-eqv 37 12))
  (test-equal? "test-27" -1 (bitwise-nand 0 0))
  (test-equal? "test-28" -1 (bitwise-nand 0 -1))
  (test-equal? "test-29" -124 (bitwise-nand -1 123))
  (test-equal? "test-326" -11 (bitwise-nand 11 26))
  (test-equal? "test-327" -28 (bitwise-nor  11 26))
  (test-equal? "test-317" 0 (bitwise-nor -1 123))
  (test-equal? "test-328" 16 (bitwise-andc1 11 26))
  (test-equal? "test-329" 1 (bitwise-andc2 11 26))
  (test-equal? "test-330" -2 (bitwise-orc1 11 26))
  (test-equal? "test-30" -1 (bitwise-nor 0 0))
  (test-equal? "test-31" 0 (bitwise-nor 0 -1))
  (test-equal? "test-22" 0 (bitwise-andc1 0 0))
  (test-equal? "test-23" -1 (bitwise-andc1 0 -1))
  (test-equal? "test-24" 123 (bitwise-andc1 0 123))
  (test-equal? "test-25" 0 (bitwise-andc2 0 0))
  (test-equal? "test-26" -1 (bitwise-andc2 -1 0))
  (test-equal? "test-318" -1 (bitwise-orc1 0 0))
  (test-equal? "test-319" -1 (bitwise-orc1 0 -1))
  (test-equal? "test-320" 0 (bitwise-orc1 -1 0))
  (test-equal? "test-321" -124 (bitwise-orc1 123 0))
  (test-equal? "test-322" -1 (bitwise-orc2 0 0))
  (test-equal? "test-323" -1 (bitwise-orc2 -1 0))
  (test-equal? "test-324" 0 (bitwise-orc2 0 -1))
  (test-equal? "test-325" -124 (bitwise-orc2 0 123))
  (test-equal? "test-78" #x1000000000000000100000000000000000000000000000000
               (arithmetic-shift #x100000000000000010000000000000000 64))
  (test-equal? "test-79" #x8e73b0f7da0e6452c810f32b809079e5
               (arithmetic-shift #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b -64))
  (test-equal? "test-196" 2 (arithmetic-shift 1 1))
  (test-equal? "test-197" 0 (arithmetic-shift 1 -1))
  (test-equal? "test-331" 1 (arithmetic-shift 1 0))
  (test-equal? "test-333" 4 (arithmetic-shift 1 2))
  (test-equal? "test-334" 8 (arithmetic-shift 1 3))
  (test-equal? "test-335" 16 (arithmetic-shift 1 4))
  (test-equal? "test-336" (expt 2 31) (arithmetic-shift 1 31))
  (test-equal? "test-337" (expt 2 32) (arithmetic-shift 1 32))
  (test-equal? "test-338" (expt 2 33) (arithmetic-shift 1 33))
  (test-equal? "test-339" (expt 2 63) (arithmetic-shift 1 63))
  (test-equal? "test-340" (expt 2 64) (arithmetic-shift 1 64))
  (test-equal? "test-341" (expt 2 65) (arithmetic-shift 1 65))
  (test-equal? "test-342" (expt 2 127) (arithmetic-shift 1 127))
  (test-equal? "test-343" (expt 2 128) (arithmetic-shift 1 128))
  (test-equal? "test-344" (expt 2 129) (arithmetic-shift 1 129))
  (test-equal? "test-345" 3028397001194014464 (arithmetic-shift 11829675785914119 8))
  (test-equal? "test-346" -1 (arithmetic-shift -1 0))
  (test-equal? "test-347" -2 (arithmetic-shift -1 1))
  (test-equal? "test-348" -4 (arithmetic-shift -1 2))
  (test-equal? "test-349" -8 (arithmetic-shift -1 3))
  (test-equal? "test-350" -16 (arithmetic-shift -1 4))
  (test-equal? "test-351" (- (expt 2 31)) (arithmetic-shift -1 31))
  (test-equal? "test-352" (- (expt 2 32)) (arithmetic-shift -1 32))
  (test-equal? "test-353" (- (expt 2 33)) (arithmetic-shift -1 33))
  (test-equal? "test-354" (- (expt 2 63)) (arithmetic-shift -1 63))
  (test-equal? "test-355" (- (expt 2 64)) (arithmetic-shift -1 64))
  (test-equal? "test-356" (- (expt 2 65)) (arithmetic-shift -1 65))
  (test-equal? "test-357" (- (expt 2 127)) (arithmetic-shift -1 127))
  (test-equal? "test-358" (- (expt 2 128)) (arithmetic-shift -1 128))
  (test-equal? "test-359" (- (expt 2 129)) (arithmetic-shift -1 129))
  (test-equal? "test-360" 0 (arithmetic-shift 1 -63))
  (test-equal? "test-361" 0 (arithmetic-shift 1 -64))
  (test-equal? "test-362" 0 (arithmetic-shift 1 -65))
  (test-equal? "test-255" 32 (arithmetic-shift 8 2))
  (test-equal? "test-256" 4 (arithmetic-shift 4 0))
  (test-equal? "test-257" 4 (arithmetic-shift 8 -1))
  (test-equal? "test-258" -79 (arithmetic-shift -100000000000000000000000000000000 -100))
  (test-equal? "test-135" 2 (bit-count 12))
  (test-equal? "test-263" 0 (integer-length  0))
  (test-equal? "test-264" 1 (integer-length  1))
  (test-equal? "test-265" 0 (integer-length -1))
  (test-equal? "test-266" 3 (integer-length  7))
  (test-equal? "test-267" 3 (integer-length -7))
  (test-equal? "test-268" 4 (integer-length  8))
  (test-equal? "test-269" 3 (integer-length -8))
  (test-equal? "test-125" 9 (bitwise-if 3 1 8))
  (test-equal? "test-126" 0 (bitwise-if 3 8 1))
  (test-equal? "test-373" 3 (bitwise-if 1 1 2))
  (test-equal? "test-378" #b00110011 (bitwise-if #b00111100 #b11110000 #b00001111))
  (test-equal? "test-160" #t (bit-set? 0 1))
  (test-equal? "test-161" #f (bit-set? 1 1))
  (test-equal? "test-162" #f (bit-set? 1 8))
  (test-equal? "test-163" #t (bit-set? 10000 -1))
  (test-equal? "test-167" #t (bit-set? 1000 -1))
  (test-equal? "test-541" #t (bit-set? 64 #x10000000000000000))
  (test-equal? "test-542" #f (bit-set? 64 1))
  (test-equal? "test-272" #t (bit-set? 3 10))
  (test-equal? "test-273" #t (bit-set? 2 6))
  (test-equal? "test-274" #f (bit-set? 0 6))
  (test-equal? "test-168" 0 (copy-bit 0 0 #f))
  (test-equal? "test-169" 0 (copy-bit 30 0 #f))
  (test-equal? "test-170" 0 (copy-bit 31 0 #f))
  (test-equal? "test-171" 0 (copy-bit 62 0 #f))
  (test-equal? "test-172" 0 (copy-bit 63 0 #f))
  (test-equal? "test-173" 0 (copy-bit 128 0 #f))
  (test-equal? "test-174" -1 (copy-bit 0 -1 #t))
  (test-equal? "test-175" -1 (copy-bit 30 -1 #t))
  (test-equal? "test-176" -1 (copy-bit 31 -1 #t))
  (test-equal? "test-177" -1 (copy-bit 62 -1 #t))
  (test-equal? "test-178" -1 (copy-bit 63 -1 #t))
  (test-equal? "test-179" -1 (copy-bit 128 -1 #t))
  (test-equal? "test-180" 1 (copy-bit 0 0 #t))
  (test-equal? "test-181" #x106 (copy-bit 8 6 #t))
  (test-equal? "test-182" 6 (copy-bit 8 6 #f))
  (test-equal? "test-183" -2 (copy-bit 0 -1 #f))
  (test-equal? "test-184" 0 (copy-bit 128 #x100000000000000000000000000000000 #f))
  (test-equal? "test-185" #x100000000000000000000000000000000
               (copy-bit 128 #x100000000000000000000000000000000 #t))
  (test-equal? "test-186" #x100000000000000000000000000000000
               (copy-bit 64 #x100000000000000000000000000000000 #f))
  (test-equal? "test-187" #x-100000000000000000000000000000000
               (copy-bit 64 #x-100000000000000000000000000000000 #f))
  (test-equal? "test-188" #x-100000000000000000000000000000000
               (copy-bit 256 #x-100000000000000000000000000000000 #t))
  (test-equal? "test-276" #b100 (copy-bit 2 0 #t))
  (test-equal? "test-277" #b1011 (copy-bit 2 #b1111 #f))
  (test-equal? "test-379" #b1 (copy-bit 0 0 #t))
  (test-equal? "test-100" #b1011 (bit-swap 1 2 #b1101))
  (test-equal? "test-101" #b1011 (bit-swap 2 1 #b1101))
  (test-equal? "test-382" #b1110 (bit-swap 0 1 #b1101))
  (test-equal? "test-102" #b10000000101 (bit-swap 3 10 #b1101))
  (test-equal? "test-278" 1 (bit-swap 0 2 4))
  (test-equal? "test-129" #t (any-bit-set? 3 6))
  (test-equal? "test-130" #f (any-bit-set? 3 12))
  (test-equal? "test-133" #t (every-bit-set? 4 6))
  (test-equal? "test-134" #f (every-bit-set? 7 6))
  (test-equal? "test-141" -1 (first-set-bit 0))
  (test-equal? "test-142" 0 (first-set-bit 1))
  (test-equal? "test-143" 0 (first-set-bit 3))
  (test-equal? "test-144" 2 (first-set-bit 4))
  (test-equal? "test-145" 1 (first-set-bit 6))
  (test-equal? "test-146" 0 (first-set-bit -1))
  (test-equal? "test-147" 1 (first-set-bit -2))
  (test-equal? "test-148" 0 (first-set-bit -3))
  (test-equal? "test-149" 2 (first-set-bit -4))
  (test-equal? "test-150" 128 (first-set-bit #x100000000000000000000000000000000))
  (test-equal? "test-280" 1 (first-set-bit 2))
  (test-equal? "test-282" 3 (first-set-bit 40))
  (test-equal? "test-283" 2 (first-set-bit -28))
  (test-equal? "test-284" 99 (first-set-bit (expt  2 99)))
  (test-equal? "test-285" 99 (first-set-bit (expt -2 99)))
  (test-equal? "test-189" 0 (bit-field 6 0 1))
  (test-equal? "test-190" 3 (bit-field 6 1 3))
  (test-equal? "test-191" 1 (bit-field 6 2 999))
  (test-equal? "test-192" 1 (bit-field #x100000000000000000000000000000000 128 129))
  (test-equal? "test-363" #b1010 (bit-field #b1101101010 0 4))
  (test-equal? "test-364" #b101101 (bit-field #b1101101010 3 9))
  (test-equal? "test-365" #b10110 (bit-field #b1101101010 4 9))
  (test-equal? "test-366" #b110110 (bit-field #b1101101010 4 10))
  (test-equal? "test-367" #t (bit-field-any? #b101101 0 2))
  (test-equal? "test-368" #t (bit-field-any? #b101101 2 4))
  (test-equal? "test-369" #f (bit-field-any? #b101101 1 2))
  (test-equal? "test-370" #f (bit-field-every? #b101101 0 2))
  (test-equal? "test-371" #t (bit-field-every? #b101101 2 4))
  (test-equal? "test-372" #t (bit-field-every? #b101101 0 1))
  (test-equal? "test-374" #b100000 (bit-field-clear #b101010 1 4))
  (test-equal? "test-375" #b101110 (bit-field-set #b101010 1 4))
  (test-equal? "test-193" #b111 (bit-field-replace #b110 1 0 1))
  (test-equal? "test-194" #b110 (bit-field-replace #b110 1 1 2))
  (test-equal? "test-195" #b010 (bit-field-replace #b110 1 1 3))
  (test-equal? "test-376" #b100100 (bit-field-replace #b101010 #b010 1 4))
  (test-equal? "test-377" #b1001 (bit-field-replace-same #b1111 #b0000 1 3))
  (test-equal? "test-200" #b110  (bit-field-rotate #b110 1 1 2))
  (test-equal? "test-201" #b1010 (bit-field-rotate #b110 1 2 4))
  (test-equal? "test-202" #b1011 (bit-field-rotate #b0111 -1 1 4))
  (test-equal? "test-203" #b0  (bit-field-rotate #b0 128 0 256))
  (test-equal? "test-204" #b1  (bit-field-rotate #b1 128 1 256))
  (test-equal? "test-205" #x100000000000000000000000000000000
               (bit-field-rotate #x100000000000000000000000000000000 128 0 64))
  (test-equal? "test-206" #x100000000000000000000000000000008
               (bit-field-rotate #x100000000000000000000000000000001 3 0 64))
  (test-equal? "test-207" #x100000000000000002000000000000000
               (bit-field-rotate #x100000000000000000000000000000001 -3 0 64))
  (test-equal? "test-208" #b110 (bit-field-rotate #b110 0 0 10))
  (test-equal? "test-209" #b110 (bit-field-rotate #b110 0 0 256))
  (test-equal? "test-475" 1 (bit-field-rotate #x100000000000000000000000000000000 1 0 129))
  (test-equal? "test-211" 6 (bit-field-reverse 6 1 3))
  (test-equal? "test-212" 12 (bit-field-reverse 6 1 4))
  (test-equal? "test-213" #x80000000 (bit-field-reverse 1 0 32))
  (test-equal? "test-214" #x40000000 (bit-field-reverse 1 0 31))
  (test-equal? "test-215" #x20000000 (bit-field-reverse 1 0 30))
  (test-equal? "test-216" (bitwise-ior (arithmetic-shift -1 32) #xFBFFFFFF)
               (bit-field-reverse -2 0 27))
  (test-equal? "test-217" (bitwise-ior (arithmetic-shift -1 32) #xF7FFFFFF)
               (bit-field-reverse -2 0 28))
  (test-equal? "test-218" (bitwise-ior (arithmetic-shift -1 32) #xEFFFFFFF)
               (bit-field-reverse -2 0 29))
  (test-equal? "test-219" (bitwise-ior (arithmetic-shift -1 32) #xDFFFFFFF)
               (bit-field-reverse -2 0 30))
  (test-equal? "test-220" (bitwise-ior (arithmetic-shift -1 32) #xBFFFFFFF)
               (bit-field-reverse -2 0 31))
  (test-equal? "test-221" (bitwise-ior (arithmetic-shift -1 32) #x7FFFFFFF)
               (bit-field-reverse -2 0 32))
  (test-equal? "test-222" 5 (bit-field-reverse #x140000000000000000000000000000000 0 129))
  (test-equal? "test-103" '(#t #f #t #f #t #t #t) (bits->list #b1110101))
  (test-equal? "test-104" '(#f #t #f #t) (bits->list #b111010 4))
  (test-equal? "test-106" #b1110101 (list->bits '(#t #f #t #f #t #t #t)))
  (test-equal? "test-107" #b111010100 (list->bits '(#f #f #t #f #t #f #t #t #t)))
  (test-equal? "test-223" '(#t #t) (bits->list 3))
  (test-equal? "test-224" '(#f #t #t #f) (bits->list 6 4))
  (test-equal? "test-225" '(#f #t) (bits->list 6 2))
  (test-equal? "test-226" '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
               (bits->list 1 128))
  (test-equal? "test-228" '(#f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t)
               (bits->list #x100000000000000000000000000000000))
  (test-equal? "test-229" 6 (list->bits '(#f #t #t)))
  (test-equal? "test-230" 12 (list->bits '(#f #f #t #t)))
  (test-equal? "test-231" 6 (list->bits '(#f #t #t #f)))
  (test-equal? "test-232" 2 (list->bits '(#f #t)))
  (test-equal? "test-233" 1 (list->bits
                             '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
  (test-equal? "test-234" #x100000000000000000000000000000000
               (list->bits
                '(#f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t)))
  (test-equal? "test-235" #x03FFFFFF (list->bits '(#t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-236" #x07FFFFFF (list->bits '(#t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-237" #x0FFFFFFF (list->bits '(#t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-238" #x1FFFFFFF (list->bits '(#t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-239" #x3FFFFFFF (list->bits '(#t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-240" #x7FFFFFFF (list->bits '(#t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-241" #xFFFFFFFF (list->bits '(#t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t
                                                   #t #t #t #t #t #t #t #t)))
  (test-equal? "test-242" #x1FFFFFFFF (list->bits '(#t
                                                    #t #t #t #t #t #t #t #t
                                                    #t #t #t #t #t #t #t #t
                                                    #t #t #t #t #t #t #t #t
                                                    #t #t #t #t #t #t #t #t)))
  (test-equal? "test-490" 1 (list->bits '(#t #f)))
  (test-equal? "test-108" #b1110101 (vector->bits '#(#t #f #t #f #t #t #t)))
  (test-equal? "test-109" #b00011010100 (vector->bits '#(#f #f #t #f #t #f #t #t)))
  (test-equal? "test-105" '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9))
  (test-equal? "test-105" '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9))
  (test-equal? "test-110" #b1110101 (bits #t #f #t #f #t #t #t))
  (test-equal? "test-243" 0 (bits))
  (test-equal? "test-111" #b111010100 (bits #f #f #t #f #t #f #t #t #t))
  (test-equal? "test-112" '(#t #f #t #f #t #t #t) (bitwise-fold (inst cons Boolean (Listof Boolean)) '() #b1010111))
  (test-equal? "test-113" 5
               (let ((count 0))
                 (bitwise-for-each (lambda (b) (when b (set! count (+ count 1))))
                                   #b1010111)
                 count))
  (test-equal? "test-114" #b101010101
               (bitwise-unfold (lambda (i) (= i 10)) even? (lambda (i) (+ i 1)) 0))
  (let ((g (make-bitwise-generator #b110)))
      (test-equal? "test-244a" #f (g))
      (test-equal? "test-244b" #t (g))
      (test-equal? "test-244c" #t (g))
      (test-equal? "test-244d" #f (g)))
)
