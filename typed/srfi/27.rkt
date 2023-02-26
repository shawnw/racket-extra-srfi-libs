; MODULE DEFINITION FOR SRFI-27
; =============================
;
; based on Sebastian.Egner@philips.com, Mar-2002, in PLT 204
;  plus the C code from that SRFI, which is built into
;  MzScheme's random routines.
; Contributors include David Van Horn and Chongkai Zhu.

; GENERIC PART OF MRG32k3a-GENERATOR FOR SRFI-27
; ==============================================
;
; Sebastian.Egner@philips.com, 2002.
;
; This is the generic R5RS-part of the implementation of the MRG32k3a
; generator to be used in SRFI-27. It is based on a separate implementation
; of the core generator (presumably in native code) and on code to
; provide essential functionality not available in R5RS (see below).
;
; compliance:
;   Scheme R5RS with integer covering at least {-2^53..2^53-1}.
;   In addition,
;     SRFI-23: error
;
; history of this file:
;   SE, 22-Mar-2002: refactored from earlier versions
;   SE, 25-Mar-2002: pack/unpack need not allocate
;   SE, 27-Mar-2002: changed interface to core generator
;   SE, 10-Apr-2002: updated spec of mrg32k3a-random-integer

; Generator
; =========
;
; Pierre L'Ecuyer's MRG32k3a generator is a Combined Multiple Recursive
; Generator. It produces the sequence {(x[1,n] - x[2,n]) mod m1 : n}
; defined by the two recursive generators
;
;   x[1,n] = (               a12 x[1,n-2] + a13 x[1,n-3]) mod m1,
;   x[2,n] = (a21 x[2,n-1] +                a23 x[2,n-3]) mod m2,
;
; where the constants are
;   m1       = 4294967087 = 2^32 - 209    modulus of 1st component
;   m2       = 4294944443 = 2^32 - 22853  modulus of 2nd component
;   a12      =  1403580                   recursion coefficients
;   a13      =  -810728
;   a21      =   527612
;   a23      = -1370589
;
; The generator passes all tests of G. Marsaglia's Diehard testsuite.
; Its period is (m1^3 - 1)(m2^3 - 1)/2 which is nearly 2^191.
; L'Ecuyer reports: "This generator is well-behaved in all dimensions
; up to at least 45: ..." [with respect to the spectral test, SE].
;
; The period is maximal for all values of the seed as long as the
; state of both recursive generators is not entirely zero.
;
; As the successor state is a linear combination of previous
; states, it is possible to advance the generator by more than one
; iteration by applying a linear transformation. The following
; publication provides detailed information on how to do that:
;
;    [1] P. L'Ecuyer, R. Simard, E. J. Chen, W. D. Kelton:
;        An Object-Oriented Random-Number Package With Many Long
;        Streams and Substreams. 2001.
;        To appear in Operations Research.
;
; Arithmetics
; ===========
;
; The MRG32k3a generator produces values in {0..2^32-209-1}. All
; subexpressions of the actual generator fit into {-2^53..2^53-1}.
; The code below assumes that Scheme's "integer" covers this range.
; In addition, it is assumed that floating point literals can be
; read and there is some arithmetics with inexact numbers.
;
; However, for advancing the state of the generator by more than
; one step at a time, the full range {0..2^32-209-1} is needed.


; Required: Backbone Generator
; ============================
;
; At this point in the code, the following procedures are assumed
; to be defined to execute the core generator:
;
;   (mrg32k3a-pack-state unpacked-state) -> packed-state
;   (mrg32k3a-unpack-state packed-state) -> unpacked-state
;      pack/unpack a state of the generator. The core generator works
;      on packed states, passed as an explicit argument, only. This
;      allows native code implementations to store their state in a
;      suitable form. Unpacked states are #(x10 x11 x12 x20 x21 x22)
;      with integer x_ij. Pack/unpack need not allocate new objects
;      in case packed and unpacked states are identical.
;
;   (mrg32k3a-random-range) -> m-max
;   (mrg32k3a-random-integer packed-state range) -> x in {0..range-1}
;      advance the state of the generator and return the next random
;      range-limited integer.
;        Note that the state is not necessarily advanced by just one
;      step because we use the rejection method to avoid any problems
;      with distribution anomalies.
;        The range argument must be an exact integer in {1..m-max}.
;      It can be assumed that range is a fixnum if the Scheme system
;      has such a number representation.
;
;   (mrg32k3a-random-real packed-state) -> x in (0,1)
;      advance the state of the generator and return the next random
;      real number between zero and one (both excluded). The type of
;      the result should be a flonum if possible.

#lang typed/racket/base

(require racket/math racket/flonum)

; Share the same default source with untyped srfi/27
(require/typed srfi/27
  [default-random-source Pseudo-Random-Generator])

(provide random-real
         default-random-source
         make-random-source
         random-source?
         random-source-state-ref
         random-source-state-set!
         random-source-randomize!
         random-source-pseudo-randomize!

         random-integer
         random-source-make-integers
         random-source-make-reals

         Random-State

         )

(define-type Random-State
  (U (Immutable-Vector
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer)
     (Mutable-Vector
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer
      Positive-Integer)))

(: random-source? : Any -> Boolean)
(define (random-source? v)
  (pseudo-random-generator? v))

#;(provide/contract (random-integer
                   (-> exact-positive-integer? any))
                  (random-source-make-integers
                   (-> random-source? (-> exact-positive-integer? any)))
                  (random-source-make-reals
                   (case->
                    (-> random-source? any)
                    (-> random-source? (and/c (>/c 0) (</c 1)) any))))

(: mrg32k3a-initial-state Random-State)
(define mrg32k3a-initial-state ; 0 3 6 9 12 15 of A^16, see below
  '#( 1062452522
      2961816100
       342112271
      2854655037
      3321940838
      3542344109))

(: make-random-source (-> Pseudo-Random-Generator))
(define (make-random-source)
  (let ([new (make-pseudo-random-generator)])
    (vector->pseudo-random-generator! new mrg32k3a-initial-state)
    new))

(: random-source-state-ref : Pseudo-Random-Generator -> Random-State)
(define (random-source-state-ref s)
  (pseudo-random-generator->vector s))

(: random-source-state-set! : Pseudo-Random-Generator Random-State -> Void)
(define (random-source-state-set! s state)
  (vector->pseudo-random-generator! s state))

(: random-source-randomize! : Pseudo-Random-Generator -> Void)
(define (random-source-randomize! s)
  (let ([new (make-pseudo-random-generator)])
    (random-source-state-set! s (random-source-state-ref new))))

(: random-source-pseudo-randomize! : Pseudo-Random-Generator Nonnegative-Integer Nonnegative-Integer -> Void)
(define (random-source-pseudo-randomize! s i j)
  (let ([vec (mrg32k3a-pseudo-randomize-state i j)])
    (vector->pseudo-random-generator! s vec)))

(: my-random-integer : Integer Pseudo-Random-Generator -> Nonnegative-Integer)
(define (my-random-integer n s)
  (if (<= n 4294967087)
      (random n s)
      (mrg32k3a-random-large s n)))

(: random-source-make-integers : Pseudo-Random-Generator -> (Positive-Integer -> Nonnegative-Integer))
(define (random-source-make-integers s)
  (lambda (n)
    (my-random-integer n s)))

(: random-source-make-reals
   (case->
    (Pseudo-Random-Generator -> (-> Flonum))
    (Pseudo-Random-Generator Flonum -> (-> Flonum))))
(define random-source-make-reals
  (case-lambda
   [(s) (lambda () (random s))]
   [(s unit)
    (let ((n (fl->exact-integer (flfloor (fl/ unit)))))
      (lambda ()
        (fl* (->fl (+ (my-random-integer n s) 1)) unit)))]))

(define random-integer
  (random-source-make-integers default-random-source))

(define random-real
  (random-source-make-reals default-random-source))

; Large Integers
; ==============
;
; To produce large integer random deviates, for n > m-max, we first
; construct large random numbers in the range {0..m-max^k-1} for some
; k such that m-max^k >= n and then use the rejection method to choose
; uniformly from the range {0..n-1}.

(define mrg32k3a-m-max 4294967087)

(: mrg32k3a-random-integer : Pseudo-Random-Generator Integer -> Nonnegative-Integer)
(define (mrg32k3a-random-integer s n) (random n s))

(: mrg32k3a-random-power : Pseudo-Random-Generator Integer -> Nonnegative-Integer)
(define (mrg32k3a-random-power state k) ; n = m-max^k, k >= 1
  (if (= k 1)
      (mrg32k3a-random-integer state mrg32k3a-m-max)
      (+ (* (mrg32k3a-random-power state (- k 1)) mrg32k3a-m-max)
         (mrg32k3a-random-integer state mrg32k3a-m-max))))

(: mrg32k3a-random-large : Pseudo-Random-Generator Integer -> Nonnegative-Integer)
(define (mrg32k3a-random-large state n) ; n > m-max
  (do ((k 2 (+ k 1))
       (mk (* mrg32k3a-m-max mrg32k3a-m-max) (* mk mrg32k3a-m-max)))
      ((>= mk n)
       (let* ((mk-by-n (quotient mk n))
              (a (* mk-by-n n)))
         (do ((x (mrg32k3a-random-power state k)
                 (mrg32k3a-random-power state k)))
             ((< x a) (cast (quotient x mk-by-n) Nonnegative-Integer)))))))


; Pseudo-Randomization
; ====================
;
; Reference [1] above shows how to obtain many long streams and
; substream from the backbone generator.
;
; The idea is that the generator is a linear operation on the state.
; Hence, we can express this operation as a 3x3-matrix acting on the
; three most recent states. Raising the matrix to the k-th power, we
; obtain the operation to advance the state by k steps at once. The
; virtual streams and substreams are now simply parts of the entire
; periodic sequence (which has period around 2^191).
;
; For the implementation it is necessary to compute with matrices in
; the ring (Z/(m1*m1)*Z)^(3x3). By the Chinese-Remainder Theorem, this
; is isomorphic to ((Z/m1*Z) x (Z/m2*Z))^(3x3). We represent such a pair
; of matrices
;   [ [[x00 x01 x02],
;      [x10 x11 x12],
;      [x20 x21 x22]], mod m1
;     [[y00 y01 y02],
;      [y10 y11 y12],
;      [y20 y21 y22]]  mod m2]
; as a vector of length 18 of the integers as written above:
;   #(x00 x01 x02 x10 x11 x12 x20 x21 x22
;     y00 y01 y02 y10 y11 y12 y20 y21 y22)
;
; As the implementation should only use the range {-2^53..2^53-1}, the
; fundamental operation (x*y) mod m, where x, y, m are nearly 2^32,
; is computed by breaking up x and y as x = x1*w + x0 and y = y1*w + y0
; where w = 2^16. In this case, all operations fit the range because
; w^2 mod m is a small number. If proper multiprecision integers are
; available this is not necessary, but pseudo-randomize! is an expected
; to be called only occasionally so we do not provide this implementation.

(define mrg32k3a-m1 4294967087) ; modulus of component 1
(define mrg32k3a-m2 4294944443) ; modulus of component 2

(: mrg32k3a-generators (Listof (Vectorof Integer)))
(define mrg32k3a-generators '()) ; computed when needed

(: mrg32k3a-pseudo-randomize-state : Integer Integer -> Random-State)
(define (mrg32k3a-pseudo-randomize-state i j)

  (: product : (Vectorof Integer) (Vectorof Integer) -> (Vectorof Integer))
  (define (product A B) ; A*B in ((Z/m1*Z) x (Z/m2*Z))^(3x3)

    (define w      65536) ; wordsize to split {0..2^32-1}
    (define w-sqr1 209)   ; w^2 mod m1
    (define w-sqr2 22853) ; w^2 mod m2

    (: lc : Integer Integer Integer Integer Integer Integer Integer Integer -> Integer)
    (define (lc i0 i1 i2 j0 j1 j2 m w-sqr) ; linear combination
      (let ((a0h (quotient (vector-ref A i0) w))
            (a0l (modulo   (vector-ref A i0) w))
            (a1h (quotient (vector-ref A i1) w))
            (a1l (modulo   (vector-ref A i1) w))
            (a2h (quotient (vector-ref A i2) w))
            (a2l (modulo   (vector-ref A i2) w))
            (b0h (quotient (vector-ref B j0) w))
            (b0l (modulo   (vector-ref B j0) w))
            (b1h (quotient (vector-ref B j1) w))
            (b1l (modulo   (vector-ref B j1) w))
            (b2h (quotient (vector-ref B j2) w))
            (b2l (modulo   (vector-ref B j2) w)))
        (modulo
         (+ (* (+ (* a0h b0h)
                  (* a1h b1h)
                  (* a2h b2h))
               w-sqr)
            (* (+ (* a0h b0l)
                  (* a0l b0h)
                  (* a1h b1l)
                  (* a1l b1h)
                  (* a2h b2l)
                  (* a2l b2h))
               w)
            (* a0l b0l)
            (* a1l b1l)
            (* a2l b2l))
         m)))

    (vector
     (lc  0  1  2   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_00 mod m1
     (lc  0  1  2   1  4  7  mrg32k3a-m1 w-sqr1) ; (A*B)_01
     (lc  0  1  2   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  3  4  5   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_10
     (lc  3  4  5   1  4  7  mrg32k3a-m1 w-sqr1)
     (lc  3  4  5   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   0  3  6  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   1  4  7  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  9 10 11   9 12 15  mrg32k3a-m2 w-sqr2) ; (A*B)_00 mod m2
     (lc  9 10 11  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc  9 10 11  11 14 17  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14   9 12 15  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14  11 14 17  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17   9 12 15  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17  11 14 17  mrg32k3a-m2 w-sqr2)))

  (: power : (Vectorof Integer) Integer -> (Vectorof Integer))
  (define (power A e) ; A^e
    (cond
     ((zero? e)
      '#(1 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 1))
     ((= e 1)
      A)
     ((even? e)
      (power (product A A) (quotient e 2)))
     (else
      (product (power A (- e 1)) A))))

  (: power-power : (Vectorof Integer) Integer -> (Vectorof Integer))
  (define (power-power A b) ; A^(2^b)
    (if (zero? b)
        A
        (power-power (product A A) (- b 1))))

  (define A                        ; the MRG32k3a recursion
    '#(     0 1403580 4294156359
            1       0          0
            0       1          0
       527612       0 4293573854
            1       0          0
            0       1          0))

  ; check arguments
  #;(when (not (and (exact-integer? i)
                  (exact-integer? j)))
      (error "'random-source-pseudo-randomize!: arguments must be exact integers:" i j))

  ; precompute A^(2^127) and A^(2^76) only once

  (when (null? mrg32k3a-generators)
      (set! mrg32k3a-generators
            (list (power-power A 127)
                  (power-power A  76)
                  (power A 16))))

  ; compute M = A^(16 + i*2^127 + j*2^76)
  (let ((M (product
            (list-ref mrg32k3a-generators 2)
            (product
             (power (list-ref mrg32k3a-generators 0)
                    (modulo i (expt 2 28)))
             (power (list-ref mrg32k3a-generators 1)
                    (modulo j (expt 2 28)))))))
     (cast (vector
            (vector-ref M 0)
            (vector-ref M 3)
            (vector-ref M 6)
            (vector-ref M 9)
            (vector-ref M 12)
            (vector-ref M 15))
           Random-State)))
