#lang racket/base
;;; SRFI-143 Fixnums
;;; Implementations of arithmetic functions

(require racket/contract racket/require
         (for-syntax racket/base)
         (only-in racket/fixnum
                  [fx= fx=?] [fx< fx<?] [fx<= fx<=?] [fx> fx>?] [fx>= fx>=?]
                  [fxlshift fxarithmetic-shift-left]
                  [fxrshift fxarithmetic-shift-right]
                  fxmax fxmin
                  fx+ fx- fx* fxquotient fxremainder fxmodulo
                  fxabs
                  [fxpopcount fxbit-count]
                  fxnot fxand fxior fxxor
                  most-positive-fixnum most-negative-fixnum)
         (only-in "141.rkt" balanced/))
(require (filtered-in (lambda (name) (and (regexp-match? #rx"^unsafe-fx" name) name)) racket/unsafe/ops))

(module+ test (require "private/testwrappers.rkt"))

(provide fixnum?
         fx=? fx<? fx<=? fx>? fx>=?
         fxmax fxmin
         fx+ fx- fx* fxquotient fxremainder
         fxabs
         fxnot fxand fxior fxxor
         fxarithmetic-shift-left fxarithmetic-shift-right
         fxbit-count
         (contract-out
          [fx-width fixnum?]
          [fx-greatest fixnum?]
          [fx-least fixnum?]
          [fxzero? (-> fixnum? boolean?)]
          [fxpositive? (-> fixnum? boolean?)]
          [fxnegative? (-> fixnum? boolean?)]
          [fxeven? (-> fixnum? boolean?)]
          [fxodd? (-> fixnum? boolean?)]
          [fxneg (-> fixnum? fixnum?)]
          [fxsquare (-> fixnum? fixnum?)]
          [fxsqrt (-> fixnum? (values fixnum? fixnum?))]
          [fx+/carry (-> fixnum? fixnum? fixnum? (values fixnum? fixnum?))]
          [fx-/carry (-> fixnum? fixnum? fixnum? (values fixnum? fixnum?))]
          [fx*/carry (-> fixnum? fixnum? fixnum? (values fixnum? fixnum?))]
          [fxarithmetic-shift (-> fixnum? fixnum? fixnum?)]
          [fxlength (-> fixnum? fixnum?)]
          [fxif (-> fixnum? fixnum? fixnum? fixnum?)]
          [fxbit-set? (-> fixnum? fixnum? boolean?)]
          [fxcopy-bit (-> fixnum? fixnum? boolean? fixnum?)]
          [fxfirst-set-bit (-> (and/c fixnum? (not/c zero?)) fixnum?)]
          [fxbit-field (-> fixnum? fixnum? fixnum? fixnum?)]
          [fxbit-field-rotate (-> fixnum? fixnum? fixnum? fixnum? fixnum?)]
          [fxbit-field-reverse (-> fixnum? fixnum? fixnum? fixnum?)]))

(define fx-width (fx+ (fxbit-count (most-positive-fixnum)) 1))
(define fx-greatest (most-positive-fixnum))
(define fx-least (most-negative-fixnum))

(define (fxzero? i) (unsafe-fx= i 0))

(define (fxpositive? i) (unsafe-fx> i 0))

(define (fxnegative? i) (unsafe-fx< i 0))

(define (fxeven? i)
  (unsafe-fx= (unsafe-fxand i 1) 0))

(define (fxodd? i)
  (unsafe-fx= (unsafe-fxand i 1) 1))

(define (fxneg i) (unsafe-fx- 0 i))

(define (fxsquare i) (fx* i i))

(define (fxsqrt i) (integer-sqrt/remainder i))

;;;; Generic implementation of carry functions from the R6RS standard.

;;; These implementations of fx+/carry, fx-/carry, and fx*/carry
;;; are very inefficient, and should be replaced by proper
;;; assembly language operations if at all possible.
;;; Furthermore, there are no tests for them,
;;; because of their dependency on fx-width.

(define exp-width (fxarithmetic-shift-left 1 (fx- fx-width 2)))

(define (fx+/carry i j k)
  (let*-values (((s) (fx+ i (fx+ j k)))
         ((q r) (balanced/ s exp-width)))
  (values r q)))

(define (fx-/carry i j k)
  (let*-values (((d) (fx- (fx- i j) k))
         ((q r) (balanced/ d exp-width)))
    (values r q)))

(define (fx*/carry i j k)
  (let*-values (((s) (fx+ (fx* i j) k))
         ((q r) (balanced/ s exp-width)))
    (values r q)))

(define (fxarithmetic-shift n places)
  (if (fxnegative? places)
      (fxarithmetic-shift-right n (fx- 0 places))
      (fxarithmetic-shift-left n places)))

(define (fxlength i) (integer-length i))

(define (fxif mask i j)
  (unsafe-fxior (unsafe-fxand mask i) (unsafe-fxand (unsafe-fxnot mask) j)))

(define (fxbit-set? index n)
  (not (fxzero? (unsafe-fxand (fxarithmetic-shift-left 1 index) n))))

(define (fxcopy-bit index to bool)
  (if bool
      (unsafe-fxior to (fxarithmetic-shift-left 1 index))
      (unsafe-fxand to (unsafe-fxnot (fxarithmetic-shift-left 1 index)))))

(define (fxfirst-set-bit i) (unsafe-fx- (unsafe-fxpopcount (unsafe-fxxor i (unsafe-fx- i 1))) 1))

;; Helper function
(define (mask start end) (unsafe-fxnot (fxarithmetic-shift-left -1 (unsafe-fx- end start))))

(define (fxbit-field n start end)
  (unsafe-fxand (mask start end) (fxarithmetic-shift n (unsafe-fx- 0 start))))

(define (fxbit-field-rotate n count start end)
  (define width (unsafe-fx- end start))
  (set! count (unsafe-fxmodulo count width))
  (let ((mask (unsafe-fxnot (fxarithmetic-shift -1 width))))
    (define zn (unsafe-fxand mask (fxarithmetic-shift n (unsafe-fx- 0 start))))
    (unsafe-fxior (fxarithmetic-shift
                   (unsafe-fxior (unsafe-fxand mask (fxarithmetic-shift-left zn count))
                                 (fxarithmetic-shift zn (unsafe-fx- count width)))
                   start)
                  (unsafe-fxand (unsafe-fxnot (fxarithmetic-shift mask start)) n))))

(define (fxreverse k n)
  (do ((m (if (fxnegative? n) (unsafe-fxnot n) n) (unsafe-fxrshift m 1))
       (k (unsafe-fx+ -1 k) (unsafe-fx+ -1 k))
       (rvs 0 (unsafe-fxior (unsafe-fxlshift rvs 1) (unsafe-fxand 1 m))))
      ((fxnegative? k) (if (fxnegative? n) (unsafe-fxnot rvs) rvs))))

(define (fxbit-field-reverse n start end)
  (define width (unsafe-fx- end start))
  (let ((mask (unsafe-fxnot (fxarithmetic-shift-left -1 width))))
    (define zn (unsafe-fxand mask (fxarithmetic-shift-right n start)))
    (unsafe-fxior (fxarithmetic-shift-left (fxreverse width zn) start)
                  (unsafe-fxand (unsafe-fxnot (fxarithmetic-shift-left mask start)) n))))


(module+ test
  (test-group "fixnum"
              (test-group "fixnum/arithmetic"
                          (test #t (fixnum? 32767))
                          (test #f (fixnum? 1.1))

                          (test #t (fx=? 1 1 1))
                          (test #f (fx=? 1 2 2))
                          (test #f (fx=? 1 1 2))
                          (test #f (fx=? 1 2 3))

                          (test #t (fx<? 1 2 3))
                          (test #f (fx<? 1 1 2))
                          (test #t (fx>? 3 2 1))
                          (test #f (fx>? 2 1 1))
                          (test #t (fx<=? 1 1 2))
                          (test #f (fx<=? 1 2 1))
                          (test #t (fx>=? 2 1 1))
                          (test #f (fx>=? 1 2 1))
                          (test '(#t #f) (list (fx<=? 1 1 2) (fx<=? 2 1 3)))

                          (test #t (fxzero? 0))
                          (test #f (fxzero? 1))

                          (test #f (fxpositive? 0))
                          (test #t (fxpositive? 1))
                          (test #f (fxpositive? -1))

                          (test #f (fxnegative? 0))
                          (test #f (fxnegative? 1))
                          (test #t (fxnegative? -1))

                          (test #f (fxodd? 0))
                          (test #t (fxodd? 1))
                          (test #t (fxodd? -1))
                          (test #f (fxodd? 102))

                          (test #t (fxeven? 0))
                          (test #f (fxeven? 1))
                          (test #t (fxeven? -2))
                          (test #t (fxeven? 102))

                          (test 4 (fxmax 3 4))
                          (test 5 (fxmax 3 5 4))
                          (test 3 (fxmin 3 4))
                          (test 3 (fxmin 3 5 4))

                          (test 7 (fx+ 3 4))
                          (test 12 (fx* 4 3))

                          (test -1 (fx- 3 4))
                          (test -3 (fxneg 3))

                          (test 7 (fxabs -7))
                          (test 7 (fxabs 7))

                          (test 1764 (fxsquare 42))
                          (test 4 (fxsquare 2))

                          (test 2 (fxquotient 5 2))
                          (test -2 (fxquotient -5 2))
                          (test -2 (fxquotient 5 -2))
                          (test 2 (fxquotient -5 -2))

                          (test 1 (fxremainder 13 4))
                          (test -1 (fxremainder -13 4))
                          (test 1 (fxremainder 13 -4))
                          (test -1 (fxremainder -13 -4))

                          (let*-values (((root rem) (fxsqrt 32)))
                            (test 35 (* root rem)))
                          )

              (test-group "fixnum/bitwise"
                          (test "test-1" -1 (fxnot 0))
                          (test "test-2" 0 (fxand #b0 #b1))
                          (test "test-115" 6 (fxand 14 6))
                          (test "test-117" 14 (fxior 10 12))
                          (test "test-119" 6 (fxxor 10 12))
                          (test "test-122" 0 (fxnot -1))
                          (test "test-125" 9 (fxif 3 1 8))
                          (test "test-126" 0 (fxif 3 8 1))
                          (test "test-135" 2 (fxbit-count 12))
                          (test "test-137" 0 (fxlength 0))
                          (test "test-138" 8 (fxlength 128))
                          (test "test-139" 8 (fxlength 255))
                          (test "test-140" 9 (fxlength 256))
                          ;(test "test-141" -1 (fxfirst-set-bit 0))
                          (test "test-142" 0 (fxfirst-set-bit 1))
                          (test "test-143" 0 (fxfirst-set-bit 3))
                          (test "test-144" 2 (fxfirst-set-bit 4))
                          (test "test-145" 1 (fxfirst-set-bit 6))
                          (test "test-146" 0 (fxfirst-set-bit -1))
                          (test "test-147" 1 (fxfirst-set-bit -2))
                          (test "test-148" 0 (fxfirst-set-bit -3))
                          (test "test-149" 2 (fxfirst-set-bit -4))
                          (test "test-160" #t (fxbit-set? 0 1))
                          (test "test-161" #f (fxbit-set? 1 1))
                          (test "test-162" #f (fxbit-set? 1 8))
                          ;(test "test-163" #t (fxbit-set? 10000 -1))
                          ;(test "test-167" #t (fxbit-set? 1000 -1))
                          (test "test-168" 0 (fxcopy-bit 0 0 #f))
                          (test "test-174" -1 (fxcopy-bit 0 -1 #t))
                          (test "test-180" 1 (fxcopy-bit 0 0 #t))
                          (test "test-181" #x106 (fxcopy-bit 8 6 #t))
                          (test "test-182" 6 (fxcopy-bit 8 6 #f))
                          (test "test-183" -2 (fxcopy-bit 0 -1 #f))
                          (test "test-189" 0 (fxbit-field 6 0 1))
                          (test "test-190" 3 (fxbit-field 6 1 3))
                          (test "test-196" 2 (fxarithmetic-shift 1 1))
                          (test "test-197" 0 (fxarithmetic-shift 1 -1))
                          (test "test-200" #b110  (fxbit-field-rotate #b110 1 1 2))
                          (test "test-201" #b1010 (fxbit-field-rotate #b110 1 2 4))
                          (test "test-202" #b1011 (fxbit-field-rotate #b0111 -1 1 4))
                          (test "test-208" #b110 (fxbit-field-rotate #b110 0 0 10))
                          (test "test-211" 6 (fxbit-field-reverse 6 1 3))
                          (test "test-212" 12 (fxbit-field-reverse 6 1 4))
                          (test "test-248" -11 (fxnot 10))
                          (test "test-249" 36 (fxnot -37))
                          (test "test-250" 11 (fxior 3  10))
                          (test "test-251" 10 (fxand 11 26))
                          (test "test-252" 9 (fxxor 3 10))
                          (test "test-254" 4 (fxand 37 12))
                          (test "test-255" 32 (fxarithmetic-shift 8 2))
                          (test "test-256" 4 (fxarithmetic-shift 4 0))
                          (test "test-257" 4 (fxarithmetic-shift 8 -1))
                          (test "test-263" 0 (fxlength  0))
                          (test "test-264" 1 (fxlength  1))
                          (test "test-265" 0 (fxlength -1))
                          (test "test-266" 3 (fxlength  7))
                          (test "test-267" 3 (fxlength -7))
                          (test "test-268" 4 (fxlength  8))
                          (test "test-269" 3 (fxlength -8))
                          (test "test-272" #t (fxbit-set? 3 10))
                          (test "test-273" #t (fxbit-set? 2 6))
                          (test "test-274" #f (fxbit-set? 0 6))
                          (test "test-276" #b100 (fxcopy-bit 2 0 #t))
                          (test "test-277" #b1011 (fxcopy-bit 2 #b1111 #f))
                          (test "test-280" 1 (fxfirst-set-bit 2))
                          (test "test-282" 3 (fxfirst-set-bit 40))
                          (test "test-283" 2 (fxfirst-set-bit -28))
                          (test "test-288" 1 (fxand #b1 #b1))
                          (test "test-289" 0 (fxand #b1 #b10))
                          (test "test-290" #b10 (fxand #b11 #b10))
                          (test "test-291" #b101 (fxand #b101 #b111))
                          (test "test-292" #b111 (fxand -1 #b111))
                          (test "test-293" #b110 (fxand -2 #b111))
                          (test "test-331" 1 (fxarithmetic-shift 1 0))
                          (test "test-333" 4 (fxarithmetic-shift 1 2))
                          (test "test-334" 8 (fxarithmetic-shift 1 3))
                          (test "test-335" 16 (fxarithmetic-shift 1 4))
                          (test "test-346" -1 (fxarithmetic-shift -1 0))
                          (test "test-347" -2 (fxarithmetic-shift -1 1))
                          (test "test-348" -4 (fxarithmetic-shift -1 2))
                          (test "test-349" -8 (fxarithmetic-shift -1 3))
                          (test "test-350" -16 (fxarithmetic-shift -1 4))
                          (test "test-363" #b1010 (fxbit-field #b1101101010 0 4))
                          (test "test-364" #b101101 (fxbit-field #b1101101010 3 9))
                          (test "test-365" #b10110 (fxbit-field #b1101101010 4 9))
                          (test "test-366" #b110110 (fxbit-field #b1101101010 4 10))
                          (test "test-373" 3 (fxif 1 1 2))
                          (test "test-378" #b00110011 (fxif #b00111100 #b11110000 #b00001111))
                          (test "test-379" #b1 (fxcopy-bit 0 0 #t))
                          )))
