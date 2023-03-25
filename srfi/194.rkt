#lang typed/racket/base

(require "../typed/srfi/27.rkt" (only-in srfi/43 vector-fold)
         (only-in racket/vector vector-map vector-append vector-take vector-drop)
         (only-in racket/math infinite? pi)
         math/flonum)

(provide
 gsampling
 current-random-source with-random-source
 make-random-source-generator make-random-integer-generator
 make-random-u1-generator
 make-random-u8-generator make-random-s8-generator
 make-random-u16-generator make-random-s16-generator
 make-random-u32-generator make-random-s32-generator
 make-random-u64-generator make-random-s64-generator
 clamp-real-number
 make-random-real-generator
 make-random-rectangular-generator make-random-polar-generator
 make-random-boolean-generator
 make-random-char-generator make-random-string-generator
 make-bernoulli-generator
 make-categorical-generator
 make-normal-generator
 make-exponential-generator
 make-geometric-generator
 make-poisson-generator
 make-binomial-generator
 make-zipf-generator
 make-sphere-generator flmake-sphere-generator
 make-ellipsoid-generator flmake-ellipsoid-generator
 make-ball-generator flmake-ball-generator
 )

(: flsqr : Flonum -> Flonum)
(define (flsqr x) (fl* x x))

;;
;; Parameters
;;
(: current-random-source (Parameterof Pseudo-Random-Generator))
(define current-random-source (make-parameter default-random-source))

(: with-random-source (All (a) Pseudo-Random-Generator (-> a) -> a))
(define (with-random-source random-source thunk)
  (parameterize ((current-random-source random-source))
                (thunk)))

;;
;; Carefully return consecutive substreams of the s'th
;; SRFI 27 stream of random numbers.  See Sections 1.2 and
;; 1.3 of "An object-oriented random-number package with many
;; long streams and substreams", by Pierre L'Ecuyer, Richard
;; Simard, E. Jack Chen, and W. David Kelton, Operations Research,
;; vol. 50 (2002), pages 1073-1075.
;; https://doi.org/10.1287/opre.50.6.1073.358
;;

(: make-random-source-generator
   : Nonnegative-Integer -> (-> Pseudo-Random-Generator))
(define (make-random-source-generator s)
  (let ((substream : Nonnegative-Integer 0))
    (lambda ()
      (let ((new-source (make-random-source))) ;; deterministic
        (random-source-pseudo-randomize! new-source s substream)
        (set! substream (+ substream 1))
        new-source))))
;;
;; Primitive randoms
;;

(: make-random-integer-generator : Integer Integer -> (-> Integer))
(define (make-random-integer-generator low-bound up-bound)
  (unless (< low-bound up-bound)
    (raise-arguments-error 'make-random-integer-generator "Upper bound should be greater than lower bound"
                           "low-bound" low-bound "up-bound" up-bound))
  (let ((rand-int-proc (random-source-make-integers (current-random-source)))
        (range (cast (- up-bound low-bound) Positive-Integer)))
    (lambda ()
      (+ low-bound (rand-int-proc range)))))

(: make-random-u1-generator : (-> (-> Integer)))
(define (make-random-u1-generator)
  (make-random-integer-generator 0 2))
(: make-random-u8-generator : (-> (-> Integer)))
(define (make-random-u8-generator)
  (make-random-integer-generator 0 256))
(: make-random-s8-generator : (-> (-> Integer)))
(define (make-random-s8-generator)
  (make-random-integer-generator -128 128))
(: make-random-u16-generator : (-> (-> Integer)))
(define (make-random-u16-generator)
  (make-random-integer-generator 0 65536))
(: make-random-s16-generator : (-> (-> Integer)))
(define (make-random-s16-generator)
  (make-random-integer-generator -32768 32768))
(: make-random-u32-generator : (-> (-> Integer)))
(define (make-random-u32-generator)
  (make-random-integer-generator 0 (expt 2 32)))
(: make-random-s32-generator : (-> (-> Integer)))
(define (make-random-s32-generator)
  (make-random-integer-generator (- (expt 2 31)) (expt 2 31)))
(: make-random-u64-generator : (-> (-> Integer)))
(define (make-random-u64-generator)
  (make-random-integer-generator 0 (expt 2 64)))
(: make-random-s64-generator : (-> (-> Integer)))
(define (make-random-s64-generator)
  (make-random-integer-generator (- (expt 2 63)) (expt 2 63)))

(: clamp-real-number : Real Real Real -> Real)
(define (clamp-real-number lower-bound upper-bound value)
  (cond
    ((not (<= lower-bound upper-bound))
     (raise-arguments-error 'clamp-real-number "lower bound must be <= upper bound" "lower-bound"
                            lower-bound "upper-bound" upper-bound))
    ((< value lower-bound) lower-bound)
    ((> value upper-bound) upper-bound)
    (else value)))

(: finite? : Real -> Boolean)
(define (finite? x)
  (not (infinite? x)))

(: make-random-real-generator : Real Real -> (-> Flonum))
(define (make-random-real-generator low-bound up-bound)
  (unless (and ;(real? low-bound)
               (finite? low-bound))
    (raise-argument-error 'make-random-real-generator "(and/c real? finite?)" low-bound))
  (unless (and ;(real? up-bound)
               (finite? up-bound))
    (raise-argument-error 'make-random-real-generator "(and/c real? finite?)" up-bound))
  (unless (< low-bound up-bound)
     (raise-arguments-error 'make-random-real-generator "lower bound must be < upper bound"
                            "low-bound" low-bound "up-bound" up-bound))
  (let ([rand-real-proc (random-source-make-reals (current-random-source))]
        [low-bound (fl low-bound)]
        [up-bound (fl up-bound)])
    (lambda ()
      (define t (rand-real-proc))
      ;; alternative way of doing lowbound + t * (up-bound - low-bound)
      ;; is susceptible to rounding errors and would require clamping to be safe
      ;; (which in turn requires 144 for adjacent float function)
      (fl+ (fl* t low-bound)
           (fl* (fl- 1.0 t) up-bound)))))

(: make-random-rectangular-generator : Real Real Real Real -> (-> Float-Complex))
(define (make-random-rectangular-generator
          real-lower-bound real-upper-bound
          imag-lower-bound imag-upper-bound)
  (let ((real-gen (make-random-real-generator real-lower-bound real-upper-bound))
        (imag-gen (make-random-real-generator imag-lower-bound imag-upper-bound)))
    (lambda ()
      (make-flrectangular (real-gen) (imag-gen)))))

(: make-random-polar-generator
   (case->
    (-> Real Real (-> Complex))
    (-> Complex Real Real (-> Complex))
    (-> Real Real Real Real (-> Complex))
    (-> Complex Real Real Real Real (-> Complex))))
(define make-random-polar-generator
  (case-lambda
    ((magnitude-lower-bound magnitude-upper-bound)
     (make-random-polar-generator 0+0i magnitude-lower-bound magnitude-upper-bound 0 (* 2.0 PI)))
    ((origin magnitude-lower-bound magnitude-upper-bound)
     (make-random-polar-generator origin magnitude-lower-bound magnitude-upper-bound 0 (* 2.0 PI)))
    ((magnitude-lower-bound magnitude-upper-bound angle-lower-bound angle-upper-bound)
     (make-random-polar-generator 0+0i magnitude-lower-bound magnitude-upper-bound angle-lower-bound angle-upper-bound))
     ((origin magnitude-lower-bound magnitude-upper-bound angle-lower-bound angle-upper-bound)
     (unless (< magnitude-lower-bound magnitude-upper-bound)
       (raise-arguments-error 'make-random-polar-generator "magnitude lower bound should be less than upper bound"
                              "magnitude-lower-bound" magnitude-lower-bound "magnitude-upper-bound" magnitude-upper-bound))
     (when (= angle-lower-bound angle-upper-bound)
       (raise-arguments-error 'make-random-polar-generator "angle bounds shouldn't be equal"
                              "angle-lower-bound" angle-lower-bound "angle-upper-bound" angle-upper-bound))
     (let* ((b (flsqr (fl magnitude-lower-bound)))
            (m (fl- (flsqr (fl magnitude-upper-bound)) b))
            (t-gen (make-random-real-generator 0.0 1.0))
            (phi-gen (make-random-real-generator angle-lower-bound angle-upper-bound)))
       (lambda ()
         (let* ((t (t-gen))
                (phi (phi-gen))
                (r (flsqrt (fl+ (fl* m t) b))))
          (+ origin (make-polar r phi))))))))

(: make-random-boolean-generator (-> (-> Boolean)))
(define (make-random-boolean-generator)
  (define u1 (make-random-u1-generator))
  (lambda ()
    (zero? (u1))))

(: make-random-char-generator : String -> (-> Char))
(define (make-random-char-generator str)
  (unless (> (string-length str) 0)
    (raise-argument-error 'make-random-char-generator "(not/c string-empty?)" str))
  (let* ([str (string->immutable-string str)]
         [int-gen (make-random-integer-generator 0 (string-length str))])
   (lambda ()
     (string-ref str (int-gen)))))

(: make-random-string-generator : Nonnegative-Integer String -> (-> String))
(define (make-random-string-generator k str)
  (unless (> (string-length str) 0)
    (raise-argument-error 'make-random-string-generator "(not/c string-empty?)" str))
  (let ((char-gen (make-random-char-generator str))
        (int-gen (make-random-integer-generator 0 k)))
    (lambda ()
      (build-string (int-gen) (lambda (i) (char-gen))))))

;;
;; Non-uniform distributions
;;

(define PI pi)

(: make-bernoulli-generator : Flonum -> (-> (U Zero One)))
(define (make-bernoulli-generator p)
  (unless (<= 0.0 p 1.0)
    (raise-argument-error 'make-bernoulli-generator "(between/c 0 1)" p))
  (let ((rand-real-proc (random-source-make-reals (current-random-source))))
   (lambda ()
     (if (<= (rand-real-proc) p)
         1
         0))))

(: make-categorical-generator : (Vectorof Integer) -> (-> Nonnegative-Integer))
(define (make-categorical-generator weights-vec)
  (define weight-sum
    (for/sum : Integer ([elem : Integer (in-vector weights-vec)]) elem))
  (define length (vector-length weights-vec))
  (let ((real-gen (make-random-real-generator 0 weight-sum)))
   (lambda ()
     (define roll (real-gen))
     (let it ((sum : Integer 0)
              (i : Nonnegative-Integer 0))
       (define newsum (+ sum (vector-ref weights-vec i)))
       (if (or (< roll newsum)
               ;; in case of rounding errors and no matches, return last element
               (= i (- length 1)))
           i
           (it newsum
               (+ i 1)))))))

;; Normal distribution (continuous - generates real numbers)
;; Box-Muller algorithm
;; NB: We tested Ziggurat method, too,
;; only to find out Box-Muller is faster about 12% - presumably
;; the overhead of each ops is larger in Gauche than C/C++, and
;; so the difference of cost of log or sin from the primitive
;; addition/multiplication are negligible.

;; NOTE: this implementation is not thread safe
(: make-normal-generator (->* () (Real Real) (-> Flonum)))
(define make-normal-generator
  (case-lambda
    (()
     (make-normal-generator 0.0 1.0))
    ((mean)
     (make-normal-generator mean 1.0))
    ((mean deviation)
     (unless (and ;(real? mean)
              (finite? mean))
       (raise-argument-error 'make-normal-generator "(and/c real? finite?)" mean))
     (unless (and ;(real? deviation)
              (finite? deviation)
              (> deviation 0))
       (raise-argument-error 'make-normal-generator "(and/c real? finite? positive?)" deviation))
     (let ([rand-real-proc (random-source-make-reals (current-random-source))]
           [mean (fl mean)]
           [deviation (fl deviation)]
           [state : (U Flonum False) #f])
       (lambda ()
         (if state
             (let ((result state))
               (set! state #f)
               (if result result +nan.0))
             (let ((r (flsqrt (fl* -2.0 (fllog (rand-real-proc)))))
                   (theta (* 2.0 pi (rand-real-proc))))
               (set! state (fl+ mean (* deviation r (flcos theta))))
               (fl+ mean (* deviation r (flsin theta))))))))))

(: make-exponential-generator : Real -> (-> Flonum))
(define (make-exponential-generator mean)
  (unless (and ;(real? mean)
               (finite? mean)
               (positive? mean))
    (raise-argument-error 'make-exponential-generator "(and/c real? finite? positive?)" mean))
  (let ((rand-real-proc (random-source-make-reals (current-random-source)))
        [mean (fl mean)])
    (lambda ()
      (fl- 0.0 (fl* mean (fllog (rand-real-proc)))))))

(: make-geometric-generator : Flonum -> (-> Integer))
(define (make-geometric-generator p)
  (unless (and ;(real? p)
           (> p 0.0)
           (<= p 1.0))
    (raise-argument-error 'make-geometric-generator "(and/c (>/c 0) (<=/c 1))" p))
  (if (fl= (fl- p 1.0) 0.0)
      ;; p is indistinguishable from 1.
      (lambda () 1)
      (let ((c (/ (fllog1p (- p))))
            (rand-real-proc (random-source-make-reals (current-random-source))))
        (lambda ()
          (fl->exact-integer (flceiling (fl* c (fllog (rand-real-proc)))))))))

;; Draw from poisson distribution with mean L, variance L.
;; For small L, we use Knuth's method.  For larger L, we use rejection
;; method by Atkinson, The Computer Generation of Poisson Random Variables,
;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
;; (http://www.johndcook.com/stand_alone_code.html )

;; NOTE: this implementation calculates and stores a table of log(n!) on first invocation of L >= 36
;; and therefore is not entirely thread safe (should still produce correct result, but with performance hit if table
;; is recalculated multiple times)
(: make-poisson-generator : Real -> (-> Integer))
(define (make-poisson-generator L)
  (unless (and ;(real? L)
               (finite? L)
               (> L 0))
    (raise-argument-error 'make-poisson-generator "(and/c real? finite? positive?)" L))
  (if (< L 30)
      (make-poisson/small (random-source-make-reals (current-random-source))
                          (fl L))
      (make-poisson/large (random-source-make-reals (current-random-source))
                          (fl L))))

;; private
(: make-poisson/small ((-> Flonum) Flonum -> (-> Integer)))
(define (make-poisson/small rand-real-proc L)
  (lambda ()
    (do : Integer ((exp-L (flexp (- L)))
                   (k : Integer 0 (+ k 1))
                   (p 1.0 (fl* p (rand-real-proc))))
        ((fl<= p exp-L) (- k 1)))))

;; private
(: make-poisson/large ((-> Flonum) Flonum -> (-> Integer)))
(define (make-poisson/large rand-real-proc L)
  (let* ((c (fl- 0.767 (fl/ 3.36 L)))
         (beta (fl/ pi (flsqrt (fl* 3.0 L))))
         (alpha (fl* beta L))
         (k (- (fllog c) L (fllog beta))))
    (define (loop) : Integer
      (let* ((u (rand-real-proc))
             (x (fl/ (fl- alpha (fllog (fl/ (fl- 1.0 u) u))) beta))
             (n (fl->exact-integer (flfloor (fl+ x 0.5)))))
        (if (< n 0)
            (loop)
            (let* ((v (rand-real-proc))
                   (y (fl- alpha (fl* beta x)))
                   (t (fl+ 1.0 (flexp y)))
                   (lhs (fl+ y (fllog (fl/ v (flsqr t)))))
                   (rhs (+ k (fl* (->fl n) (fllog L)) (- (fllog-factorial (->fl n))))))
              (if (fl<= lhs rhs)
                  n
                  (loop))))))
    loop))


(: gsampling (All (a) (-> (-> a) * (-> (U a EOF)))))
(define (gsampling . generators-lst)
  (let ((gen-vec (list->vector generators-lst))
        (rand-int-proc (random-source-make-integers (current-random-source))))

    ;remove exhausted generator at index
    (: remove-gen : Integer -> Void)
    (define (remove-gen index)
      (let ([before (if (> index 0) (vector-take gen-vec index) #())]
            [after (vector-drop gen-vec (+ index 1))])
        (set! gen-vec (vector-append before after))))

    ;randomly pick generator. If it's exhausted remove it, and pick again
    ;returns value (or eof, if all generators are exhausted)
    (define (pick) : (U a EOF)
      (let* ((index (rand-int-proc (cast (vector-length gen-vec) Positive-Integer)))
             (gen (vector-ref gen-vec index))
             (value (gen)))
        (if (eof-object? value)
            (begin
              (remove-gen index)
              (if (= (vector-length gen-vec) 0)
                  eof
                  (pick)))
            value)))

    (lambda ()
      (if (= 0 (vector-length gen-vec))
          eof
          (pick)))))

(: make-binomial-generator : Positive-Integer Real -> (-> Integer))
(define (make-binomial-generator n p)
  (unless (and ;(real? p)
           (<= 0.0 p 1.0)
           ;(exact-integer? n)
           #;(positive? n))
    (error "make-binomial-generator: Bad parameters: " n p))
  (cond ((< 0.5 p)
         (let ((complement (make-binomial-generator n (- 1 p))))
           (lambda ()
             (- n (complement)))))
        ((zero? p)
         (lambda () 0))
        ((< (* n p) 10)
         (binomial-geometric n (fl p)))
        (else
         (binomial-rejection n (fl p)))))

;;; Code for binomial random variable generation.

;;; binomial-geometric is somewhat classical, the
;;; "First waiting time algorithm" from page 525 of
;;; Devroye, L. (1986), Non-Uniform Random Variate
;;; Generation, Springer-Verlag, New York.

;;; binomial-rejection is algorithm BTRS from
;;; Hormann, W. (1993), The generation of binomial
;;; random variates, Journal of Statistical Computation
;;; and Simulation, 46:1-2, 101-110,
;;; DOI: https://doi.org/10.1080/00949659308811496
;;; stirling-tail is also from that paper.

;;; Another implementation of the same algorithm is at
;;; https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/kernels/random_binomial_op.cc
;;; That implementation pointed out at least two bugs in the
;;; BTRS paper.

(define small-k-table
  ;; Computed using computable reals package
  ;; Matches values in paper, which are given
  ;; for 0\leq k < 10
  (flvector
   .08106146679532726
   .0413406959554093
   .02767792568499834
   .020790672103765093
   .016644691189821193
   .013876128823070748
   .01189670994589177
   .010411265261972096
   .009255462182712733
   .00833056343336287
   .007573675487951841
   .00694284010720953
   .006408994188004207
   .0059513701127588475
   .005554733551962801
   .0052076559196096404
   .004901395948434738
   .004629153749334028
   .004385560249232324
   .004166319691996922))

(: stirling-tail (Integer -> Flonum))
(define (stirling-tail k)
  ;; Computes
  ;;
  ;; \log(k!)-[\log(\sqrt{2\pi})+(k+\frac12)\log(k+1)-(k+1)]
  ;;
  (if (< k 20)
      (flvector-ref small-k-table k)
      ;; the correction term (+ (/ (* 12 (+ k 1))) ...)
      ;; in Stirling's approximation to log(k!)
      (let* ((inexact-k+1 (->fl (+ k 1)))
             (inexact-k+1^2 (fl* inexact-k+1 inexact-k+1)))
        (fl/ (fl- #i1/12
                  (fl/ (fl- #i1/360
                            (fl/ #i1/1260 inexact-k+1^2))
                       inexact-k+1^2))
             inexact-k+1))))

(: binomial-geometric (Integer Flonum -> (-> Integer)))
(define (binomial-geometric n p)
  (let ((geom (make-geometric-generator p)))
    (lambda ()
      (let loop ((X -1)
                 (sum 0))
        (if (< n sum)
            X
            (loop (+ X 1)
                  (+ sum (geom))))))))

(: binomial-rejection (Integer Flonum -> (-> Integer)))
(define (binomial-rejection n p)
  ;; call when p <= 1/2 and np >= 10
  ;; Use notation from the paper
  (let* ((spq
          (flsqrt (* (->fl n) p (fl- 1.0 p))))
         (b
          (fl+ 1.15 (fl* 2.53 spq)))
         (a
          (+ -0.0873
             (fl* 0.0248 b)
             (fl* 0.01 p)))
         (c
          (fl+ (fl* (->fl n) p) 0.5))
         (v_r
          (fl- 0.92
             (fl/ 4.2 b)))
         (alpha
          ;; The formula in BTRS has 1.5 instead of 5.1;
          ;; The formula for alpha in algorithm BTRD and Table 1
          ;; and the tensorflow code uses 5.1, so we use 5.1
          (fl* (fl+ 2.83 (fl/ 5.1 b)) spq))
         (lpq
          (fllog (fl/ p (fl- 1.0 p))))
         (m
          (fl->exact-integer (flfloor (fl* (fl+ 1.0 (->fl n)) p))))
         (rand-real-proc
          (random-source-make-reals (current-random-source))))
   (lambda ()
      (let loop ((u (rand-real-proc))
                 (v (rand-real-proc)))
        (let* ((u
                (fl- u 0.5))
               (us
                (fl- 0.5 (flabs u)))
               (k
                (fl->exact-integer (flfloor
                  (fl+ (fl* (fl+ (fl* 2. (fl/ a us)) b) u) c)))))
          (cond ((or (< k 0)
                     (< n k))
                 (loop (rand-real-proc)
                       (rand-real-proc)))
                ((and (fl<= 0.07 us)
                      (fl<= v v_r))
                 k)
                (else
                 (let ((v
                        ;; The tensorflow code notes that BTRS doesn't have
                        ;; this logarithm; BTRS is incorrect (see BTRD, step 3.2)
                        (fllog (fl* v (fl/ alpha
                                     (fl+ (fl/ a (fl* us us)) b))))))
                   (if (fl<=  v
                              (+ (fl* (fl+ (->fl m) 0.5)
                                      (fllog (fl/ (fl+ (->fl m) 1.)
                                                  (fl- (fl- (->fl n) (->fl m)) -1.))))
                                 (fl* (fl+ (->fl n) 1.)
                                      (fllog (fl/ (fl- (fl- (->fl n) (->fl m)) -1.)
                                                  (fl- (fl- (->fl n) (->fl k)) -1.))))
                                 (fl* (fl+ (->fl k) 0.5)
                                      (fllog (fl/ (fl- (fl- (->fl n) (->fl k)) -1.)
                                                  (fl+ (->fl k) 1.))))
                                 (fl* (fl- (->fl k) (->fl m)) lpq)
                                 (fl- (fl+ (stirling-tail m)
                                           (stirling-tail (- n m)))
                                      (fl+ (stirling-tail k)
                                           (stirling-tail (- n k))))))
                       k
                       (loop (rand-real-proc)
                             (rand-real-proc)))))))))))


;------------------------------------------------------------------
;
; (make-zipf-generator n [s [q]])
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
; If `q` is not specified, 0 is assumed.
; If `s` is not specified, 1 is assumed.
;
; Valid for real -10 < s < 100 (otherwise overflows likely)
; Valid for real -0.5 < q < 2e8 (otherwise overflows likely)
; Valid for integer 1 <= k < int-max
;
; Example usage:
;    (define zgen (make-zipf-generator 50 1.01 0))
;    (generator->list zgen 10)
;
(: make-zipf-generator (->* (Integer) (Real Real) (-> Integer)))
(define (make-zipf-generator n [s 1.0] [q 0.0])
  (if (fl< #i1e-5 (flabs (- 1.0 (fl s))))
      (make-zipf-generator/zri n (fl s) (fl q))
      (make-zipf-generator/one n (fl s) (fl q))))

; zipf-zri.scm
; Create a Zipf random distribution.
;
; Created by Linas Vepstas 10 July 2020
; Nominated for inclusion in srfi-194
;
; Not optimized for speed!
;
; Implementation from ZRI algorithm presented in the paper:
; "Rejection-inversion to generate variates from monotone discrete
; distributions", Wolfgang Hörmann and Gerhard Derflinger
; ACM TOMACS 6.3 (1996): 169-184
;
; Hörmann and Derflinger use "q" everywhere, when they really mean "s".
; Thier "q" is not the standard q-series deformation. Its just "s".
; The notation in the code below differs from the article to reflect
; conventional usage.
;
;------------------------------------------------------------------
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
;
; The exponent `s` must be a real number not equal to 1.
; Accuracy is diminished for |1-s|< 1e-6. The accuracy is roughly
; equal to 1e-15 / |1-s| where 1e-15 == 64-bit double-precision ULP.
;
(: make-zipf-generator/zri (Integer Flonum Flonum -> (-> Integer)))
(define (make-zipf-generator/zri n s q)

  ; The hat function h(x) = 1 / (x+q)^s
  (: hat (Flonum -> Flonum))
  (define (hat x)
    (flexpt (fl+ x q) (fl- 0.0 s)))

  (define _1-s (fl- 1.0 s))
  (define oms (fl/ 1.0 _1-s))

  ; The integral of hat(x)
  ; H(x) = (x+q)^{1-s} / (1-s)
  ; Note that H(x) is always negative.
  (: big-h (Flonum -> Flonum))
  (define (big-h x)
    (fl/ (flexpt (fl+ q x) _1-s) _1-s))

  ; The inverse function of H(x)
  ; H^{-1}(y) = -q + (y(1-s))^{1/(1-s)}
  (: big-h-inv (Flonum -> Flonum))
  (define (big-h-inv y)
    (fl- (flexpt (fl* y _1-s) oms) q))

  ; Lower and upper bounds for the uniform random generator.
  (define big-h-half (fl- (big-h 1.5) (hat 1.0)))
  (define big-h-n (big-h (fl+ (->fl n) 0.5)))

  ; Rejection cut
  (define cut (fl- 1.0 (big-h-inv (fl- (big-h 1.5) (hat 1.0)))))

  ; Uniform distribution
  (define dist (make-random-real-generator big-h-half big-h-n))

  ; Attempt to hit the dartboard. Return #f if we fail,
  ; otherwise return an integer between 1 and n.
  (: try (-> (Option Integer)))
  (define (try)
    (define u (dist))
    (define x (big-h-inv u))
    (define kflt (flfloor (fl+ x 0.5)))
    (define k (fl->exact-integer kflt))
    (if (and (< 0 k)
             (or
               (fl<= (fl- kflt x) cut)
               (fl>= u (fl- (big-h (fl+ kflt 0.5)) (hat kflt))))) k #f))

  ; Did we hit the dartboard? If not, try again.
  (: loop-until (-> Integer))
  (define (loop-until)
    (define k (try))
    (if k k (loop-until)))

  ; Return the generator.
  loop-until)

;------------------------------------------------------------------
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
;
; The exponent `s` must be a real number close to 1.
; Accuracy is diminished for |1-s|> 2e-4. The accuracy is roughly
; equal to 0.05 * |1-s|^4 due to exp(1-s) being expanded to 4 terms.
;
; This handles the special case of s==1 perfectly.
(: make-zipf-generator/one (Integer Flonum Flonum -> (-> Integer)))
(define (make-zipf-generator/one n s q)

  (define _1-s (fl- 1.0 s))

  ; The hat function h(x) = 1 / (x+q)^s
  ; Written for s->1 i.e. 1/(x+q)(x+q)^{s-1}
  (: hat (Flonum -> Flonum))
  (define (hat x)
    (define xpq (fl+ x q))
    (fl/ (flexpt xpq _1-s) xpq))

  ; Expansion of exn(y) = [exp(y(1-s))-1]/(1-s) for s->1
  ; Expanded to 4th order.
  ; Should equal this:
  ;;; (define (exn lg) (/ (- (exp (* _1-s lg)) 1) _1-s))
  ; but more accurate for s near 1.0
  (: exn (Flonum -> Flonum))
  (define (exn lg)
    (: trm (Flonum Flonum Flonum -> Flonum))
    (define (trm n u lg) (fl* lg (fl+ 1.0 (fl/ (fl* _1-s u) n))))
    (trm 2.0 (trm 3.0 (trm 4.0 1.0 lg) lg) lg))

  ; Expansion of lg(y) = [log(1 + y(1-s))] / (1-s) for s->1
  ; Expanded to 4th order.
  ; Should equal this:
  ;;; (define (lg y) (/ (log (+ 1 (* y _1-s))) _1-s))
  ; but more accurate for s near 1.0
  (: lg (Flonum -> Flonum))
  (define (lg y)
    (define yms (fl* y _1-s))
    (: trm (Flonum Flonum Flonum -> Flonum))
    (define (trm n u r) (fl- (fl/ 1.0 n) (fl* u r)))
    (* y (trm 1.0 yms (trm 2.0 yms (trm 3.0 yms (trm 4.0 yms 0.0))))))

  ; The integral of hat(x) defined at s==1
  ; H(x) = [exp{(1-s) log(x+q)} - 1]/(1-s)
  ; Should equal this:
  ;;;  (define (big-h x) (/ (- (exp (* _1-s (log (+ q x)))) 1)  _1-s))
  ; but expanded so that it's more accurate for s near 1.0
  (: big-h (Flonum -> Flonum))
  (define (big-h x)
    (exn (fllog (fl+ q x))))

  ; The inverse function of H(x)
  ; H^{-1}(y) = -q + (1 + y(1-s))^{1/(1-s)}
  ; Should equal this:
  ;;; (define (big-h-inv y) (- (expt (+ 1 (* y _1-s)) (/ 1 _1-s)) q ))
  ; but expanded so that it's more accurate for s near 1.0
  (: big-h-inv (Flonum -> Flonum))
  (define (big-h-inv y)
    (fl- (flexp (lg y)) q))

  ; Lower and upper bounds for the uniform random generator.
  (define big-h-half (fl- (big-h 1.5) (hat 1.0)))

  (define big-h-n (big-h (fl+ (->fl n) 0.5)))

  ; Rejection cut
  (define cut (fl- 1.0 (big-h-inv (fl- (big-h 1.5) (fl/ 1.0 (fl+ 1.0 q))))))

  ; Uniform distribution
  (define dist (make-random-real-generator big-h-half big-h-n))

  ; Attempt to hit the dartboard. Return #f if we fail,
  ; otherwise return an integer between 1 and n.
  (: try (-> (Option Integer)))
  (define (try)
    (define u (dist))
    (define x (big-h-inv u))
    (define kflt (flfloor (fl+ x 0.5)))
    (define k (fl->exact-integer kflt))
    (if (and (< 0 k)
             (or
               (fl<= (fl- kflt x) cut)
               (fl>= u (fl- (big-h (fl+ kflt 0.5)) (hat kflt))))) k #f))

  ; Did we hit the dartboard? If not, try again.
  (: loop-until (-> Integer))
  (define (loop-until)
    (define k (try))
    (if k k (loop-until)))

  ; Return the generator.
  loop-until)

; sphere.scm
; Uniform distributions on a sphere, and a ball.
; Submitted for inclusion in srfi-194
;
; Algorithm based on BoxMeuller as described in
; http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
;

; make-sphere-generator N - return a generator of points uniformly
; distributed on an N-dimensional sphere.
; This implements the BoxMeuller algorithm, that is, of normalizing
; N+1 Gaussian random variables.
(: make-sphere-generator : Integer -> (-> (Vectorof Flonum)))
(define (make-sphere-generator arg)
  (let ([gen (flmake-ellipsoid-generator (make-flvector (+ arg 1) 1.0))])
    (lambda () (flvector->vector (gen)))))

(: flmake-sphere-generator : Integer -> (-> FlVector))
(define (flmake-sphere-generator arg)
  (flmake-ellipsoid-generator (make-flvector (+ arg 1) 1.0)))

(: make-ellipsoid-generator : (U FlVector (Vectorof Real)) -> (-> (Vectorof Flonum)))
(define (make-ellipsoid-generator arg)
  (let ([gen (flmake-ellipsoid-generator (if (flvector? arg) arg (vector->flvector arg)))])
    (lambda () (flvector->vector (gen)))))

; -----------------------------------------------
; Generator of points uniformly distributed on an N-dimensional ellipsoid.
;
; The `axes` should be a vector of floats, specifying the axes of the
; ellipsoid. The algorithm used is an accept/reject sampling algo,
; wherein the acceptance rate is proportional to the measure of a
; surface element on the ellipsoid. The measure is straight-forward to
; arrive at, and the 3D case is described by `mercio` in detail at
; https://math.stackexchange.com/questions/973101/how-to-generate-points-uniformly-distributed-on-the-surface-of-an-ellipsoid
;
; Note that sampling means that performance goes as
; O(B/A x C/A x D/A x ...) where `A` is the shorest axis,
; and `B`, `C`, `D`, ... are the other axes. Maximum performance
; achieved on spheres.
;

; Banach l2-norm of a vector
(: l2-norm (FlVector -> Flonum))
(define (l2-norm VEC)
  (flsqrt (flvector-sum (flvector-sqr VEC))))

(: flmake-ellipsoid-generator : (U FlVector (Vectorof Real)) -> (-> FlVector))
(define (flmake-ellipsoid-generator axes)
  (cond
    ((flvector? axes) (%flmake-ellipsoid-generator axes))
    ((vector? axes) (%flmake-ellipsoid-generator (vector->flvector axes)))))

(: %flmake-ellipsoid-generator : FlVector -> (-> FlVector))
(define (%flmake-ellipsoid-generator axes)
  ; A vector of normal gaussian generators
  (define gaussg-vec
    (build-vector (flvector-length axes)
                  (lambda (i) (make-normal-generator 0.0 1.0))))

  ; Generate one point on a sphere
  (: sph (-> FlVector))
  (define (sph)
    ; Sample a point
    (define point
      (build-flvector (vector-length gaussg-vec) (lambda (i) ((vector-ref gaussg-vec i)))))
    ; Project it to the unit sphere (make it unit length)
    (define norm (fl/ 1.0 (l2-norm point)))
    (flvector-map (lambda (x) (fl* x norm)) point))

  ; Distance from origin to the surface of the
  ; ellipsoid along direction RAY.
  (: ellipsoid-dist (FlVector -> Flonum))
  (define (ellipsoid-dist RAY)
    (flsqrt (flvector-sum (flvector/ (flvector-sqr RAY) (flvector-sqr axes)))))

  ; Find the shortest axis.
  (define minor
    (for/fold : Flonum ([mino 1e308])
              ([elem (in-flvector axes)])
      (flmin mino elem)))

  ; Uniform generator [0,1)
  (define uni (make-random-real-generator 0.0 1.0))

  ; Return #t if the POINT can be kept; else must resample.
  (: keep (FlVector -> Boolean))
  (define (keep POINT)
    (fl< (uni) (fl* minor (ellipsoid-dist POINT))))

  ; Sample until a good point is found. The returned sample is a
  ; vector of unit length (we already normed up above).
  (: sample (-> FlVector))
  (define (sample)
    (define vect (sph))
    (if (keep vect) vect (sample)))

  (lambda ()
    ; Find a good point, and rescale to ellipsoid.
    (flvector* (sample) axes))
)


(: realvec->flvec : (U (Vectorof Real)) -> FlVector)
(define (realvec->flvec rv)
  (inline-build-flvector (+ (vector-length rv) 2)
                         (lambda ([i : Index])
                           (if (< i (vector-length rv))
                               (fl (cast (vector-ref rv i) Real))
                               1.0))))

(: grow-flvec : FlVector -> FlVector)
(define (grow-flvec fv)
  (let ([new-fv (make-flvector (+ (flvector-length fv) 2) 1.0)])
    (flvector-copy! new-fv 0 fv)
    new-fv))

; -----------------------------------------------
; make-ball-generator N - return a generator of points uniformly
; distributed inside an N-dimensional ball.
; This implements the Harman-Lacko-Voelker Dropped Coordinate method.
(: make-ball-generator : (U Integer (Vectorof Real) FlVector) -> (-> (Vectorof Flonum)))
(define (make-ball-generator arg)
  (: dim-sizes FlVector)
  (define dim-sizes
    (cond
      ((integer? arg) (make-flvector (+ 2 arg) 1.0))
      ((vector? arg) (realvec->flvec arg))
      ((flvector? arg) (grow-flvec arg))))
  (define N (- (flvector-length dim-sizes) 2))
  (define sphereg (flmake-sphere-generator (+ N 2)))
  ; Create a vector of N+2 values, and drop the last two.
  ; (The sphere already added one, so we only add one more)
  (lambda ()
    (for/vector : (Vectorof Flonum) ([el : Flonum (in-flvector (sphereg))]
                                     [dim-size : Flonum (in-flvector dim-sizes)]
                                     [_ (in-range N)])
      (fl* el dim-size))))

(: flmake-ball-generator : (U Integer (Vectorof Real) FlVector) -> (-> FlVector))
(define (flmake-ball-generator arg)
  (: dim-sizes FlVector)
  (define dim-sizes
    (cond
      ((integer? arg) (make-flvector (+ 2 arg) 1.0))
      ((vector? arg) (realvec->flvec arg))
      ((flvector? arg) (grow-flvec arg))))
  (define N (- (flvector-length dim-sizes) 2))
  (define sphereg (flmake-sphere-generator (+ N 2)))
  ; Create a vector of N+2 values, and drop the last two.
  ; (The sphere already added one, so we only add one more)
  (lambda ()
    (let ([els (sphereg)])
      (inline-build-flvector N
                             (lambda ([i : Index])
                               (fl* (flvector-ref els i) (flvector-ref dim-sizes i)))))))
