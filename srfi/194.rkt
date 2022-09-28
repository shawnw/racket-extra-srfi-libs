#lang racket/base

(require srfi/27 (only-in srfi/43 vector-fold)
         (only-in racket/vector vector-map vector-append)
         (only-in racket/math infinite? sqr pi exact-ceiling exact-floor)
         racket/contract racket/require
         (for-syntax racket/base (only-in racket/string string-prefix?))
         "private/194-kernel.rkt")
(require (filtered-in
          (lambda (name)
            (and (string-prefix? name "unsafe-fl")
                 (substring name 7)))
          racket/unsafe/ops)
         (only-in racket/unsafe/ops unsafe-make-flrectangular)
         (only-in math/flonum ->fl fl flvector? flvector make-flvector inline-flvector-map
                  vector->flvector flvector->vector flvector-copy!))
(module+ test (require rackunit))

(provide
 gsampling
 (contract-out
  [current-random-source (parameter/c random-source?)]
  [with-random-source (-> random-source? (-> any) any)]
  [make-random-source-generator (-> exact-nonnegative-integer? random-source?)]
  [make-random-integer-generator (-> integer? integer? (-> integer?))]
  [make-random-u1-generator (-> (-> (integer-in 0 1)))]
  [make-random-u8-generator (-> (-> byte?))]
  [make-random-s8-generator (-> (-> fixnum?))]
  [make-random-u16-generator (-> (-> exact-nonnegative-integer?))]
  [make-random-s16-generator (-> (-> fixnum?))]
  [make-random-u32-generator (-> (-> exact-nonnegative-integer?))]
  [make-random-s32-generator (-> (-> exact-integer?))]
  [make-random-u64-generator (-> (-> exact-nonnegative-integer?))]
  [make-random-s64-generator (-> (-> exact-integer?))]
  [clamp-real-number (-> real? real? real? real?)]
  [make-random-real-generator (-> real? real? (-> flonum?))]
  [make-random-rectangular-generator (-> real? real? real? real? (-> complex?))]
  [make-random-polar-generator (case->
                                (-> positive? positive? (-> complex?))
                                (-> complex? positive? positive? (-> complex?))
                                (-> positive? positive? real? real? (-> complex?))
                                (-> complex? positive? positive? real? real? (-> complex?)))]
  [make-random-boolean-generator (-> (-> boolean?))]
  [make-random-char-generator (-> string? (-> char?))]
  [make-random-string-generator (-> exact-nonnegative-integer? string? (-> string?))]
  [make-bernoulli-generator (-> (between/c 0 1) (-> (integer-in 0 1)))]
  [make-categorical-generator (-> (vectorof positive?) (-> exact-nonnegative-integer?))]
  [make-normal-generator (->* () (real? real?) (-> flonum?))]
  [make-exponential-generator (-> positive? (-> flonum?))]
  [make-geometric-generator (-> (and/c flonum? (>/c 0.0) (<=/c 1.0)) (-> integer?))]
  [make-poisson-generator (-> positive? (-> integer?))]
  [make-binomial-generator (-> exact-positive-integer? (between/c 0 1) (-> integer?))]
  [make-zipf-generator (->* (integer?) (real? real?) (-> integer?))]
  [make-sphere-generator (->* (exact-positive-integer?) (boolean?) (or/c (-> (vectorof flonum?)) (-> flvector?)))]
  [make-ellipsoid-generator (-> (or/c (vectorof real?) flvector?) (or/c (-> (vectorof flonum?)) (-> flvector?)))]
  [make-ball-generator (-> (or/c integer? (vectorof real?) flvector?) (or/c (-> (vectorof flonum?)) (-> flvector?)))]))

(define (finite? x) (not (infinite? x)))
(define (flsqr x) (fl* x x))

;;
;; Parameters
;;
(define current-random-source (make-parameter default-random-source))

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

(define (make-random-source-generator s)
  (let ((substream 0))
    (lambda ()
      (let ((new-source (make-random-source))) ;; deterministic
        (random-source-pseudo-randomize! new-source s substream)
        (set! substream (+ substream 1))
        new-source))))
;;
;; Primitive randoms
;;

(define (make-random-integer-generator low-bound up-bound)
  (unless (< low-bound up-bound)
    (raise-arguments-error 'make-random-integer-generator "Upper bound should be greater than lower bound"
                           "low-bound" low-bound "up-bound" up-bound))
  (let ((rand-int-proc (random-source-make-integers (current-random-source)))
        (range (- up-bound low-bound)))
    (lambda ()
      (+ low-bound (rand-int-proc range)))))

(define (make-random-u1-generator)
  (make-random-integer-generator 0 2))
(define (make-random-u8-generator)
  (make-random-integer-generator 0 256))
(define (make-random-s8-generator)
  (make-random-integer-generator -128 128))
(define (make-random-u16-generator)
  (make-random-integer-generator 0 65536))
(define (make-random-s16-generator)
  (make-random-integer-generator -32768 32768))
(define (make-random-u32-generator)
  (make-random-integer-generator 0 (expt 2 32)))
(define (make-random-s32-generator)
  (make-random-integer-generator (- (expt 2 31)) (expt 2 31)))
(define (make-random-u64-generator)
  (make-random-integer-generator 0 (expt 2 64)))
(define (make-random-s64-generator)
  (make-random-integer-generator (- (expt 2 63)) (expt 2 63)))

(define (clamp-real-number lower-bound upper-bound value)
  (cond
    ((not (<= lower-bound upper-bound))
     (raise-arguments-error 'clamp-real-number "lower bound must be <= upper bound" "lower-bound"
                            lower-bound "upper-bound" upper-bound))
    ((< value lower-bound) lower-bound)
    ((> value upper-bound) upper-bound)
    (else value)))

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
  (%make-random-real-generator (current-random-source) (fl low-bound) (fl up-bound)))

(define (make-random-rectangular-generator
          real-lower-bound real-upper-bound
          imag-lower-bound imag-upper-bound)
  (let ((real-gen (make-random-real-generator real-lower-bound real-upper-bound))
        (imag-gen (make-random-real-generator imag-lower-bound imag-upper-bound)))
    (lambda ()
      (unsafe-make-flrectangular (real-gen) (imag-gen)))))

(define make-random-polar-generator
  (case-lambda
    ((magnitude-lower-bound magnitude-upper-bound)
     (make-random-polar-generator 0+0i magnitude-lower-bound magnitude-upper-bound 0 (fl* 2.0 PI)))
    ((origin magnitude-lower-bound magnitude-upper-bound)
     (make-random-polar-generator origin magnitude-lower-bound magnitude-upper-bound 0 (fl* 2.0 PI)))
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
            (t-gen (make-random-real-generator 0. 1.))
            (phi-gen (make-random-real-generator angle-lower-bound angle-upper-bound)))
       (lambda ()
         (let* ((t (t-gen))
                (phi (phi-gen))
                (r (flsqrt (fl+ (fl* m t) b))))
          (+ origin (make-polar r phi))))))))

(define (make-random-boolean-generator)
  (define u1 (make-random-u1-generator))
  (lambda ()
    (zero? (u1))))

(define (make-random-char-generator str)
  (unless (> (string-length str) 0)
    (raise-argument-error 'make-random-char-generator "(not/c string-empty?)" str))
  (let* ([str (string->immutable-string str)]
         [int-gen (make-random-integer-generator 0 (string-length str))])
   (lambda ()
     (string-ref str (int-gen)))))

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

(define (make-bernoulli-generator p)
  #;(unless (<= 0 p 1)
    (raise-argument-error 'make-bernoulli-generator "(between/c 0 1)" p))
  (let ((rand-real-proc (random-source-make-reals (current-random-source))))
   (lambda ()
     (if (<= (rand-real-proc) p)
         1
         0))))

(define (make-categorical-generator weights-vec)
  (define weight-sum
    (vector-fold
      (lambda (i sum p)
        (+ sum p))
      0
      weights-vec))
  (define length (vector-length weights-vec))
  (let ((real-gen (make-random-real-generator 0 weight-sum)))
   (lambda ()
     (define roll (real-gen))
     (let it ((sum 0)
              (i 0))
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
     (%make-normal-generator (current-random-source) (fl mean) (fl deviation)))))

(define (make-exponential-generator mean)
  (unless (and ;(real? mean)
               (finite? mean)
               #;(positive? mean))
    (raise-argument-error 'make-exponential-generator "(and/c real? finite? positive?)" mean))
  (%make-exponential-generator (current-random-source) (fl mean)))

(define (make-geometric-generator p)
  #;(unless (and ;(real? p)
               (> p 0)
               (<= p 1))
      (raise-argument-error 'make-geometric-generator "(and/c (>/c 0) (<=/c 1))" p))
  (%make-geometric-generator (current-random-source) (fl p)))

;; Draw from poisson distribution with mean L, variance L.
;; For small L, we use Knuth's method.  For larger L, we use rejection
;; method by Atkinson, The Computer Generation of Poisson Random Variables,
;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
;; (http://www.johndcook.com/stand_alone_code.html )

;; NOTE: this implementation calculates and stores a table of log(n!) on first invocation of L >= 36
;; and therefore is not entirely thread safe (should still produce correct result, but with performance hit if table
;; is recalculated multiple times)
(define (make-poisson-generator L)
  (unless (and ;(real? L)
               (finite? L)
               #;(> L 0))
    (raise-argument-error 'make-poisson-generator "(and/c real? finite? positive?)" L))
  (let ((rand-real-proc (random-source-make-reals (current-random-source))))
   (if (< L 30)
       (make-poisson/small rand-real-proc (fl L))
       (make-poisson/large rand-real-proc (fl L)))))

(define (gsampling . generators-lst)
  (let ((gen-vec (list->vector generators-lst))
        (rand-int-proc (random-source-make-integers (current-random-source))))

    ;remove exhausted generator at index
    (define (remove-gen index)
      (define new-vec (make-vector (- (vector-length gen-vec) 1)))
      ;when removing anything but first, copy all elements before index
      (when (> index 0)
        (vector-copy! new-vec 0 gen-vec 0 index))
      ;when removing anything but last, copy all elements after index
      (when (< index (- (vector-length gen-vec) 1))
        (vector-copy! new-vec index gen-vec (+ 1 index)))
      (set! gen-vec new-vec))

    ;randomly pick generator. If it's exhausted remove it, and pick again
    ;returns value (or eof, if all generators are exhausted)
    (define (pick)
      (let* ((index (rand-int-proc (vector-length gen-vec)))
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

(define (make-binomial-generator n p)
#|  (if (not (and ;(real? p)
                (<= 0 p 1)
                ;(exact-integer? n)
                #;(positive? n)))
       (error "make-binomial-generator: Bad parameters: " n p) |#
  (cond ((< 1/2 p)
         (let ((complement (make-binomial-generator n (- 1 p))))
           (lambda ()
             (- n (complement)))))
        ((zero? p)
         (lambda () 0))
        ((< (* n p) 10)
         (binomial-geometric (current-random-source) n (fl p)))
        (else
         (binomial-rejection (current-random-source) n (fl p)))))

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
(define (make-zipf-generator n [s 1.0] [q 0.0])
  (if (< 1e-5 (abs (- 1 s)))
      (make-zipf-generator/zri (current-random-source) n (fl s) (fl q))
      (make-zipf-generator/one (current-random-source) n (fl s) (fl q))))

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
(define (make-sphere-generator arg [use-flvec? #f])
  (let ([gen
         (make-ellipsoid-generator* (current-random-source) (if use-flvec? (make-flvector (+ arg 1) 1.0) (make-vector (+ 1 arg) 1.0)))])
    (if use-flvec?
        gen
        (lambda () (flvector->vector (gen))))))

(define (make-ellipsoid-generator arg)
  (let ([gen
         (make-ellipsoid-generator* (current-random-source) (if (flvector? arg) arg (vector->flvector arg)))])
    (if (flvector? arg)
        gen
        (lambda () (flvector->vector (gen))))))

; -----------------------------------------------
; make-ball-generator N - return a generator of points uniformly
; distributed inside an N-dimensional ball.
; This implements the Harman-Lacko-Voelker Dropped Coordinate method.
(define (make-ball-generator arg)
  (define dim-sizes
    (cond
      ((integer? arg) (make-vector (+ 2 arg) 1.0))
      ((vector? arg) (vector-append arg (vector-immutable 1.0 1.0)))
      ((flvector? arg)
       (let ([dim (make-flvector (+ (flvector-length arg) 2) 1.0)])
             (flvector-copy! dim 0 arg)
             dim))
      (else (raise-argument-error 'make-ball-generator "(or/c integer? (vectorof real?) flvector?)" arg))))
  (define N (- (vector-length dim-sizes) 2))
  (define sphereg (make-sphere-generator (+ N 2) (flvector? arg)))
  ; Create a vector of N+2 values, and drop the last two.
  ; (The sphere already added one, so we only add one more)
  (if (flvector? arg)
      (lambda () (inline-flvector-map
                  (lambda (el dim-size _) (* el dim-size))
                  (sphereg)
                  dim-sizes
                  (make-flvector N 0.0)))
      (lambda ()
        (vector-map
         (lambda (el dim-size _) (* el dim-size))
         (sphereg)
         dim-sizes
         (make-vector N #f)))))


(module+ test
  ;; NOTE: for zipf tests data can be exported, this can be enabled by uncommenting appropriate lines.

  (require "158.rkt" srfi/1 srfi/43)
  (define-syntax-rule (test-group name tests ...)
    (begin tests ...))
  (define-syntax-rule (test-assert expr)
    (check-not-false expr))
  (define-syntax-rule (test-equal expected expr)
    (check-equal? expr expected))
  (define-syntax-rule (test-approximate target value max-delta)
    (check-= target value max-delta))

  ;; syntax just we can plop it at top and still allow internal `define`s
  (define-syntax reset-source!
    (syntax-rules ()
      ((_)
       (define _ (random-source-pseudo-randomize! (current-random-source) 0 0)))))

  (define (reset-source!*)
    (random-source-pseudo-randomize! (current-random-source) 0 0))

  (define (assert-number-generator/all-in-range gen from to)
    (test-assert
     (generator-every
      (lambda (num)
        (and (>= num from)
             (< num to)))
      (gtake gen 1000))))

  (define (assert-number-generator gen from to)
    (define range (- to from))
    (define lower-quarter (+ from (* 0.25 range)))
    (define upper-quarter (- to (* 0.25 range)))
    (assert-number-generator/all-in-range gen from to)

    (test-assert
     (generator-any
      (lambda (num)
        (and (>= num from)
             (< num lower-quarter)))
      (gtake gen 1000)))

    (test-assert
     (generator-any
      (lambda (num)
        (and (>= num lower-quarter)
             (< num upper-quarter)))
      (gtake gen 1000)))

    (test-assert
     (generator-any
      (lambda (num)
        (and (>= num upper-quarter)
             (< num to)))
      (gtake gen 1000))))

  (define (assert-int-generator gen byte-size signed?)
    (define from (if signed?
                     (- (expt 2 (- byte-size 1)))
                     0))
    (define to (if signed?
                   (expt 2 (- byte-size 1))
                   (expt 2 byte-size)))
    (assert-number-generator gen from to))

  (test-group "Test clamp real number"
              (reset-source!)
              (test-equal 10.0 (clamp-real-number 5.0 10.0 11))
              (test-equal 5.0 (clamp-real-number 5.0 10.0 2.0))
              (test-equal 7.5 (clamp-real-number 5.0 10.0 7.5)))

  (test-group "Test with-random-source basic syntax"
              (reset-source!)
              (void (with-random-source default-random-source
                      (lambda () (make-random-integer-generator 0 10)))))

  (test-group "Test make-random-source-generator"
              (reset-source!)
              (define (make-numbers src-gen)
                (define gen1 (with-random-source (src-gen) (lambda () (make-random-integer-generator 0 100))))
                (define gen2 (with-random-source (src-gen) (lambda () (make-random-real-generator 0. 100.))))
                (generator->list
                 (gappend
                  (gtake gen1 10)
                  (gtake gen2 10))))

              (test-equal
               (make-numbers (make-random-source-generator 0))
               (make-numbers (make-random-source-generator 0)))
              (test-assert
               (not (equal? (make-numbers (make-random-source-generator 0))
                            (make-numbers (make-random-source-generator 1))))))
  (test-group "Test random int"
              (reset-source!)
              (assert-number-generator
               (make-random-integer-generator 1 100)
               1 100)

              (for-each
               (lambda (testcase)
                 (define make-gen (car testcase))
                 (define byte-size (cadr testcase))
                 (define signed? (caddr testcase))
                 (assert-int-generator (make-gen) byte-size signed?))
               (list
                (list make-random-u8-generator 8 #f)
                (list make-random-s8-generator 8 #t)
                (list make-random-u16-generator 16 #f)
                (list make-random-s16-generator 16 #t)
                (list make-random-u32-generator 32 #f)
                (list make-random-s32-generator 32 #t)
                (list make-random-u64-generator 64 #f)
                (list make-random-s64-generator 64 #t)))

              ;;test u1 separately, since it will fail quarter checks due to small range
              (assert-number-generator/all-in-range (make-random-u1-generator) 0 2)
              (test-assert
               (generator-any
                (lambda (v) (= v 0))
                (gtake (make-random-u1-generator) 100)))
              (test-assert
               (generator-any
                (lambda (v) (= v 1))
                (gtake (make-random-u1-generator) 100))))

  (test-group "Test random real"
              (reset-source!)
              (assert-number-generator
               (make-random-real-generator 1.0 5.0)
               1.0 5.0)

              (test-assert
               (generator-any
                (lambda (v)
                  (not (= v (floor v))))
                (make-random-real-generator 1.0 5.0))))

  (test-group "Test complex rectangular"
              (reset-source!)

              (assert-number-generator
               (gmap
                real-part
                (make-random-rectangular-generator -10.0 10.0 -100.0 100.0))
               -10 10)

              (assert-number-generator
               (gmap
                imag-part
                (make-random-rectangular-generator -100.0 100.0 -10.0 10.0))
               -10 10)

              (test-assert
               (generator-any
                (lambda (num)
                  (and (not (= 0 (real-part num)))
                       (not (= 0 (imag-part num)))))
                (make-random-rectangular-generator -10.0 10.0 -10.0 10.0))))
  (test-group "Test complex polar"
              (reset-source!)
              (define PI (* 4 (atan 1.0)))

              (define (test-polar g origin mag-from mag-to angle-from angle-to test-converge-origin)
                (assert-number-generator
                 (gmap
                  (lambda (num)
                    (angle (- num origin)))
                  g)
                 angle-from angle-to)

                (assert-number-generator
                 (gmap
                  (lambda (num)
                    (magnitude (- num origin)))
                  g)
                 mag-from mag-to)

                ;; split generated area through circle at 0.5*(mag-from + mag-to)
                ;; and validate generated points in them proportional to their area
                (let* ((outter-count 0)
                       (inner-count 0)
                       (donut-area (lambda (r1 r2) (- (* r1 r1) (* r2 r2))))
                       (mag-mid (/ (+ mag-from mag-to) 2.))
                       (expected-fraction (/ (donut-area mag-to mag-mid)
                                             (donut-area mag-mid mag-from))))
                  (generator-for-each
                   (lambda (point)
                     (if (< (magnitude (- point origin)) mag-mid)
                         (set! inner-count (+ 1 inner-count))
                         (set! outter-count (+ 1 outter-count))))
                   (gtake g 10000))
                  (test-approximate expected-fraction (/ outter-count inner-count) 0.2))

                ;; test points converge to center
                (when test-converge-origin
                  (let ((sum 0+0i))
                    (generator-for-each
                     (lambda (point) (set! sum (+ point sum)))
                     (gtake g 1000))
                    (test-approximate (real-part origin) (real-part (/ sum 1000.)) 0.1)
                    (test-approximate (imag-part origin) (imag-part (/ sum 1000.)) 0.1))))


              (test-polar (make-random-polar-generator 0. 1.)
                          0+0i 0. 1. (- PI) PI #t)

              (test-polar (make-random-polar-generator 2+5i 1. 2.)
                          2+5i 1. 2. (- PI) PI #t)

              (test-polar (make-random-polar-generator 1. 2. -1. 1.)
                          0+0i 1. 2. -1. 1. #f)

              (test-polar (make-random-polar-generator -1+3i 0. 2. (- PI) PI)
                          -1+3i 0. 2. (- PI) PI #t))

  (test-group "Test random bool"
              (reset-source!)
              (test-assert
               (generator-every
                (lambda (v)
                  (or (eq? v #t)
                      (eq? v #f)))
                (gtake (make-random-boolean-generator) 10000)))

              (test-assert
               (generator-any
                (lambda (v)
                  (eq? #t v))
                (make-random-boolean-generator)))

              (test-assert
               (generator-any
                (lambda (v)
                  (eq? #f v))
                (make-random-boolean-generator))))

  (test-group "Test random char"
              (reset-source!)
              (test-assert
               (generator-every
                (lambda (v)
                  (or (equal? v #\a)
                      (equal? v #\b)))
                (gtake (make-random-char-generator "ab")
                       10000)))

              (test-assert
               (generator-any
                (lambda (v)
                  (equal? v #\a))
                (make-random-char-generator "ab")))

              (test-assert
               (generator-any
                (lambda (v)
                  (equal? v #\b))
                (make-random-char-generator "ab"))))

  (test-group "Test random string"
              (reset-source!)
              (test-assert
               (generator-every
                (lambda (str)
                  (and (< (string-length str) 5)
                       (every (lambda (c)
                                (or (equal? c #\a)
                                    (equal? c #\b)))
                              (string->list str))))
                (gtake (make-random-string-generator 5 "ab")
                       10000)))

              (test-assert
               (generator-any
                (lambda (str)
                  (equal? "abb" str))
                (make-random-string-generator 4 "ab"))))

  (test-group "Test Bernoulli"
              (reset-source!)
              (define g (make-bernoulli-generator 0.7))
              (define expect 7000)
              (define actual (generator-count
                              (lambda (i) (= i 1))
                              (gtake g 10000)))
              (define ratio (exact->inexact (/ actual expect)))
              (test-assert (> ratio 0.9))
              (test-assert (< ratio 1.1)))

  (test-group "Test categorical"
              (reset-source!)
              (define result-vec (vector 0 0 0))
              (define expect-vec (vector 2000 5000 3000))
              (define wvec (vector 20 50 30))
              (define g2 (make-categorical-generator wvec))
              (generator-for-each
               (lambda (i)
                 (vector-set! result-vec i (+ 1 (vector-ref result-vec i))))
               (gtake g2 10000))
              (vector-for-each
               (lambda (i result expect)
                 (define ratio (exact->inexact (/ result expect)))
                 (test-approximate 1.0 ratio 0.1))
               result-vec
               expect-vec))

  (test-group "Test poisson"
              (reset-source!)
              ;;TODO import from somewhere?
              (define (fact1 k)
                (cond
                  ((<= k 1) 1)
                  (else (* k (fact1 (- k 1))))))
              (define (expected-fraction L k)
                (/ (* (inexact->exact (expt L k)) (inexact->exact (exp (- L))))
                   (fact1 k)))

              (define (test-poisson L poisson-gen test-points)
                (void (generator-every
                       (lambda (k)
                         (define expect (expected-fraction L k))
                         (define actual (/ (generator-count
                                            (lambda (i) (= i k))
                                            (gtake poisson-gen 10000))
                                           10000))
                         (define ratio (/ actual expect))
                         (test-assert (> ratio 8/10))
                         (test-assert (< ratio 12/10))
                         #t)
                       (list->generator test-points))))

              (test-poisson 2 (make-poisson-generator 2) '(1 2 3))
              (test-poisson 40 (make-poisson-generator 40) '(30 40 50))
              (test-poisson 280 (make-poisson-generator 280) '(260 280 300)))

  (test-group "Test normal"
              (reset-source!)
              (define frac-at-1dev 0.34134)
              (define frac-at-2dev 0.47725)
              (define frac-at-3dev 0.49865)

              (define (test-normal-at-point gen count-from count-to expected-fraction)
                (define actual (/ (generator-count
                                   (lambda (n)
                                     (and (>= n count-from)
                                          (< n count-to)))
                                   (gtake gen 10000))
                                  10000.0))
                (test-assert (and (> actual (* 0.9 expected-fraction))
                                  (< actual (* 1.1 expected-fraction)))))

              (define (test-normal gen mean deviation)
                (test-normal-at-point gen mean (+ mean deviation) frac-at-1dev)
                (test-normal-at-point gen mean (+ mean (* 2 deviation)) frac-at-2dev)
                (test-normal-at-point gen mean (+ mean (* 3 deviation)) frac-at-3dev))

              (test-normal (make-normal-generator) 0.0 1.0)
              (test-normal (make-normal-generator 1.0) 1.0 1.0)
              (test-normal (make-normal-generator 1.0 2.0) 1.0 2.0))

  (test-group "Test exponential"
              (reset-source!)
              (define (expected-fraction2 mean x)
                (- 1 (exp (* (- (/ 1.0 mean)) x))))

              (define (test-exp-at-point gen count-to expected)
                (define actual (/ (generator-count
                                   (lambda (n)
                                     (< n count-to))
                                   (gtake gen 10000))
                                  10000.0))
                (test-assert (> actual (* 0.9 expected)))
                (test-assert (< actual (* 1.1 expected))))

              (define (test-exp gen mean)
                (test-exp-at-point gen 1 (expected-fraction2 mean 1))
                (test-exp-at-point gen 2 (expected-fraction2 mean 2))
                (test-exp-at-point gen 3 (expected-fraction2 mean 3)))

              (test-exp (make-exponential-generator 1) 1)
              (test-exp (make-exponential-generator 1.5) 1.5))

  (test-group "Test geometric"
              (reset-source!)
              (define (expected-fraction3 p x)
                (* (expt (- 1 p) (- x 1)) p))

              (define (test-geom-at-point gen p x)
                (define expected (expected-fraction3 p x))
                (define actual (/ (generator-count
                                   (lambda (n)
                                     (= n x))
                                   (gtake gen 100000))
                                  100000))
                (define ratio (/ actual expected))
                (test-assert (> ratio 0.9))
                (test-assert (< ratio 1.1)))

              (define (test-geom gen p)
                (test-geom-at-point gen p 1)
                (test-geom-at-point gen p 3)
                (test-geom-at-point gen p 5))

              (test-geom (make-geometric-generator 0.5) 0.5))

  (test-group "Test uniform sampling"
              (reset-source!)
              (test-equal
               '()
               (generator->list (gsampling)))
              (test-equal
               '()
               (generator->list (gsampling (generator) (generator))))
              (test-equal
               '(1 1 1)
               (generator->list (gsampling (generator) (generator 1 1 1))))
              (test-assert
               (generator-any
                (lambda (el)
                  (= el 1))
                (gsampling (circular-generator 1) (circular-generator 2))))
              (test-assert
               (generator-any
                (lambda (el)
                  (= el 2))
                (gsampling (circular-generator 1) (circular-generator 2)))))

  #;(test-group "Test sphere"
              (reset-source!*)
              (test-sphere (make-sphere-generator 1) (vector 1.0 1.0) 200 #t)
              (test-sphere (make-sphere-generator 2) (vector 1.0 1.0 1.0) 200 #t)
              (test-sphere (make-sphere-generator 3 #t) (flvector 1.0 1.0 1.0 1.0) 200 #t)

              (reset-source!*)
              (test-sphere (make-ellipsoid-generator (vector 1.0 1.0)) (vector 1.0 1.0) 200 #t)
              (test-sphere (make-ellipsoid-generator (vector 1.0 1.0 1.0)) (vector 1.0 1.0 1.0) 200 #t)
              (test-sphere (make-ellipsoid-generator (vector 1.0 1.0 1.0 1.0)) (vector 1.0 1.0 1.0 1.0) 200 #t)

              (reset-source!*)
              (test-sphere (make-ellipsoid-generator (vector 1.0 3.0)) (vector 1.0 3.0) 200 #f)
              (test-sphere (make-ellipsoid-generator (vector 1.0 3.0 5.0)) (vector 1.0 3.0 5.0) 200 #f)
              (test-sphere (make-ellipsoid-generator (flvector 1.0 3.0 5.0 7.0)) (flvector 1.0 3.0 5.0 7.0) 200 #f)

              (reset-source!*)
              (test-ball (make-ball-generator 2) (vector 1.0 1.0))
              (test-ball (make-ball-generator 3) (vector 1.0 1.0 1.0))
              (test-ball (make-ball-generator (vector 1.0 3.0)) (vector 1.0 3.0))
              (test-ball (make-ball-generator (flvector 1.0 3.0 5.0)) (flvector 1.0 3.0 5.0)))

  (test-group "Test binomial"
              (reset-source!)
              (define (factorial n)
                (if (<= n 1)
                    1
                    (* n (factorial (- n 1)))))
              (define (C n k)
                (/ (factorial n)
                   (* (factorial k) (factorial (- n k)))))
              (define (expected-frac n p k)
                (* (C n k) (expt p k) (expt (- 1 p) (- n k))))

              (define (test-binomial n p count)
                (define g (make-binomial-generator n p))
                (define counts (make-vector (+ n 1) 0))
                (generator-for-each
                 (lambda (x)
                   (vector-set! counts x (+ 1 (vector-ref counts x))))
                 (gtake g count))
                (for-each
                 (lambda (k)
                   (define expected (* count (expected-frac n p k) ))
                   (define actual (vector-ref counts k))
                   (cond
                     ((= expected 0)
                      (test-equal 0 actual))
                     ;;hacky.. testing values with very low probability fails
                     ((> expected (* 1/1000 count))
                      (test-approximate 1.0 (/ actual expected) 0.2))))
                 (iota (+ n 1))))

              (test-binomial 1 0 100)
              (test-binomial 1 1 100)
              (test-binomial 1 0. 100)
              (test-binomial 1 1. 100)
              (test-binomial 10 0 100)
              (test-binomial 10 1 100)
              (test-binomial 10 0. 100)
              (test-binomial 10 1. 100)
              (test-binomial 10 0.25 100000)
              (test-binomial 40 0.375 1000000))


  ;
  ; zipf-test.scm
  ; Unit tests for the Zipf (zeta) distribution.
  ;
  ; Created by Linas Vepstas 10 July 2020
  ; Nominated for inclusion in srfi-194

  ; ------------------------------------------------------------------
  ; Debug utility for gnuplot graphing.
  ; You can use this to dump a vector to a tab-delimited file.
  (define (vector-to-file vec filename)
    (define (write-vec)
      (for-each
       (lambda (i)
         (define index (+ i 1))
         (define val (vector-ref vec i))
         (display index)
         (display "  ")
         (display val)
         (newline))
       (iota (vector-length vec))))
    (with-output-to-file filename write-vec))

  ; ------------------------------------------------------------------
  ; Simple test harness for exploring paramter space.
  ;
  ; Take REPS samples from the zeta distribution (ZGEN NVOCAB ESS QUE)
  ; Accumulate them into NVOCAB bins.
  ; Normalize to unit probability (i.e. divide by NVOCAB)
  ;
  ; The resulting distribution should uniformly converge to C/(k+q)^s
  ; for 1 <= k <= NVOCAB where C is a normalization constant.
  ;
  ; This compares the actual distribution to the expected convergent
  ; and reports an error if it is not within TOL of the convergent.
  ; i.e. it computes the Banach l_0 norm of (distribution-convergent)
  ; TOL is to be given in units of standard deviations. So, for example,
  ; setting TOL to 6 gives a six-sigma bandpass, allowsing the tests to
  ; usually pass.
  ;
  (define (test-zipf ZGEN NVOCAB ESS QUE REPS TOL)

    ; Bin-counter containing accumulated histogram.
    (define bin-counts
      (let ((bin-counts (make-vector NVOCAB 0)))
        ; Accumulate samples into the histogram.
        (generator-for-each
         (lambda (SAMP)
           (define offset (- SAMP 1))
           (vector-set! bin-counts offset (+ 1 (vector-ref bin-counts offset))))
         (gtake (ZGEN NVOCAB ESS QUE) REPS))
        bin-counts))


    ; Verify the distribution is within tolerance.
    ; This is written out long-hand for easier debuggability.

    ; Frequency is normalized to be 0.0 to 1.0
    (define frequency (vector-map (lambda (n) (/ n REPS)) bin-counts))
    (define probility (vector-map (lambda (n) (exact->inexact n)) frequency))

    ; Sequence 1..NVOCAB
    (define seq
      (vector-unfold (lambda (i x) (values x (+ x 1))) NVOCAB 1))

    ; Sequence  1/(k+QUE)^ESS
    (define inv-pow (vector-map
                     (lambda (k) (expt (+ k QUE) (- (exact->inexact ESS)))) seq))

    ; Hurwicz harmonic number sum_1..NVOCAB 1/(k+QUE)^ESS
    (define hnorm
      (vector-fold
       (lambda (i sum cnt) (+ sum cnt)) 0 inv-pow))

    ; The expected distribution
    (define expect
      (vector-map (lambda (x) (/ x hnorm)) inv-pow))

    ; Convert to floating point.
    (define prexpect (vector-map (lambda (x) (exact->inexact x)) expect))

    ; The difference
    (define diff (vector-map (lambda (x y) (- x y)) probility prexpect))

    ; Re-weight the tail by k^{s/2}. This seems give a normal error
    ; distribution. ... at least, for small q. Problems for large q
    ; and with undersampling; so we hack around that.
    (define err-dist
      (if (< 10 QUE) diff
          (vector-map (lambda (i x) (* x (expt (+ i 1) (* 0.5 ESS))))
                      (list->vector (iota (vector-length diff)))
                      diff)))

    ; Normalize to unit root-mean-square.
    (define rms (/ 1 (sqrt (* 2 3.141592653 REPS))))
    (define norm-dist (vector-map (lambda (x) (/ x rms)) err-dist))

    ; Maximum deviation from expected distribution (l_0 norm)
    (define l0-norm
      (vector-fold
       (lambda (i sum x) (if (< sum (abs x)) (abs x) sum)) 0 norm-dist))

    ; The mean.
    (define mean (/
                  (vector-fold (lambda (i sum x) (+ sum x)) 0 norm-dist)
                  NVOCAB))

    (define root-mean-square (sqrt (/
                                    (vector-fold (lambda (sum x) (+ sum (* x x))) 0 norm-dist)
                                    NVOCAB)))

    ; The total counts in the bins should be equal to REPS
    (test-assert
     (equal? REPS
             (vector-fold
              (lambda (i sum cnt) (+ sum cnt)) 0 bin-counts)))

    ; Test for uniform convergence.
    (test-assert (<= l0-norm TOL))

    ; Should not random walk too far away.
    ; Could tighten this with a proper theory of the error distribution.
    (test-assert (< (abs mean) 3))
    ; I don't understand the error distribution ....
    ; (test-assert (and (< 0.4 root-mean-square) (< root-mean-square 1.5)))

    (test-assert (<= l0-norm TOL))

    ; Utility debug printing
    ;(vector-to-file probility "probility.dat")
    ;(vector-to-file prexpect "prexpect.dat")
    ;(vector-to-file diff "diff.dat")
    #f)

  ; Explore the parameter space.
  (define (zipf-test-group)

    ; The unit test is computes something that is "almost" a standard
    ; deviation for the error distribution. Except, maybe not quite,
    ; I don't fully understand the theory. So most tests seem to come
    ; in fine in well-under a six-sigma deviation, but some of the wilder
    ; paramter choices misbehave, so six-sigma doesn't always work.
    ; Also, when the number of bins is large, its easy to under-sample,
    ; some bins end up empty and the std-dev is thrown off as a result.
    ; Thus, the tolerance bounds below are hand-adjusted.
    (define six-sigma 6.0)

    (define hack-que 3.0)

    ; Zoom into s->1
    (test-zipf make-zipf-generator 30 1.1     0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 1.01    0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 1.001   0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 1.0001  0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 1.00001 0 1000 six-sigma)

    (test-zipf make-zipf-generator 30 (+ 1 1e-6)  0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 (+ 1 1e-8)  0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 (+ 1 1e-10) 0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 (+ 1 1e-12) 0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 (+ 1 1e-14) 0 1000 six-sigma)
    (test-zipf make-zipf-generator 30 1           0 1000 six-sigma)

    ; Verify improving uniform convergence
    (test-zipf make-zipf-generator 30 1  0 10000   six-sigma)
    (test-zipf make-zipf-generator 30 1  0 100000  six-sigma)

    ; Larger vocabulary
    (test-zipf make-zipf-generator 300 1.1     0 10000 six-sigma)
    (test-zipf make-zipf-generator 300 1.01    0 10000 six-sigma)
    (test-zipf make-zipf-generator 300 1.001   0 10000 six-sigma)
    (test-zipf make-zipf-generator 300 1.0001  0 10000 six-sigma)
    (test-zipf make-zipf-generator 300 1.00001 0 10000 six-sigma)

    ; Larger vocabulary. Take more samples....
    (test-zipf make-zipf-generator 3701 1.1     0 40000 six-sigma)
    (test-zipf make-zipf-generator 3701 1.01    0 40000 six-sigma)
    (test-zipf make-zipf-generator 3701 1.001   0 40000 six-sigma)
    (test-zipf make-zipf-generator 3701 1.0001  0 40000 six-sigma)
    (test-zipf make-zipf-generator 3701 1.00001 0 40000 six-sigma)

    ; Huge vocabulary; few samples. Many bins will be empty,
    ; causing the std-dev to get large.
    (test-zipf make-zipf-generator 43701 (+ 1 1e-6)  0 60000 9.5)
    (test-zipf make-zipf-generator 43701 (+ 1 1e-7)  0 60000 9.5)
    (test-zipf make-zipf-generator 43701 (+ 1 1e-9)  0 60000 9.5)
    (test-zipf make-zipf-generator 43701 (+ 1 1e-12) 0 60000 9.5)
    (test-zipf make-zipf-generator 43701 1           0 60000 9.5)

    ; Large s, small range
    (test-zipf make-zipf-generator 5 1.1     0 1000 six-sigma)
    (test-zipf make-zipf-generator 5 2.01    0 1000 six-sigma)
    (test-zipf make-zipf-generator 5 4.731   0 1000 six-sigma)
    (test-zipf make-zipf-generator 5 9.09001 0 1000 six-sigma)
    (test-zipf make-zipf-generator 5 13.45   0 1000 8.0)

    ; Large s, larger range. Most histogram bins will be empty
    ; so allow much larger error margins.
    (test-zipf make-zipf-generator 130 1.5     0 30000 six-sigma)
    (test-zipf make-zipf-generator 130 2.03    0 30000 9.0)
    (test-zipf make-zipf-generator 130 4.5     0 30000 16.0)
    (test-zipf make-zipf-generator 130 6.66    0 30000 24.0)

    ; Verify that accuracy improves with more samples.
    (test-zipf make-zipf-generator 129 1.1     0 10000 six-sigma)
    (test-zipf make-zipf-generator 129 1.01    0 10000 six-sigma)
    (test-zipf make-zipf-generator 129 1.001   0 10000 six-sigma)
    (test-zipf make-zipf-generator 129 1.0001  0 10000 six-sigma)
    (test-zipf make-zipf-generator 129 1.00001 0 10000 six-sigma)

    ; Non-zero Hurwicz parameter
    (test-zipf make-zipf-generator 131 1.1     0.3    10000 six-sigma)
    (test-zipf make-zipf-generator 131 1.1     1.3    10000 six-sigma)
    (test-zipf make-zipf-generator 131 1.1     6.3    10000 six-sigma)
    (test-zipf make-zipf-generator 131 1.1     20.23  10000 six-sigma)

    ; Negative Hurwicz parameter. Must be greater than branch point at -0.5.
    (test-zipf make-zipf-generator 81 1.1     -0.1   1000 six-sigma)
    (test-zipf make-zipf-generator 81 1.1     -0.3   1000 six-sigma)
    (test-zipf make-zipf-generator 81 1.1     -0.4   1000 six-sigma)
    (test-zipf make-zipf-generator 81 1.1     -0.499 1000 six-sigma)

    ; A walk into a stranger corner of the parameter space.
    (test-zipf make-zipf-generator 131 1.1     41.483 10000 hack-que)
    (test-zipf make-zipf-generator 131 2.1     41.483 10000 hack-que)
    (test-zipf make-zipf-generator 131 6.1     41.483 10000 hack-que)
    (test-zipf make-zipf-generator 131 16.1    41.483 10000 hack-que)
    (test-zipf make-zipf-generator 131 46.1    41.483 10000 hack-que)
    (test-zipf make-zipf-generator 131 96.1    41.483 10000 hack-que)

    ; A still wilder corner of the parameter space.
    (test-zipf make-zipf-generator 131 1.1     1841.4 10000 hack-que)
    (test-zipf make-zipf-generator 131 1.1     1.75e6 10000 hack-que)
    (test-zipf make-zipf-generator 131 2.1     1.75e6 10000 hack-que)
    (test-zipf make-zipf-generator 131 12.1    1.75e6 10000 hack-que)
    (test-zipf make-zipf-generator 131 42.1    1.75e6 10000 hack-que)

    ; Lets try s less than 1
    (test-zipf make-zipf-generator 35 0.9     0 1000 six-sigma)
    (test-zipf make-zipf-generator 35 0.99    0 1000 six-sigma)
    (test-zipf make-zipf-generator 35 0.999   0 1000 six-sigma)
    (test-zipf make-zipf-generator 35 0.9999  0 1000 six-sigma)
    (test-zipf make-zipf-generator 35 0.99999 0 1000 six-sigma)

    ; Attempt to force an overflow
    (test-zipf make-zipf-generator 437 (- 1 1e-6)  0 1000 six-sigma)
    (test-zipf make-zipf-generator 437 (- 1 1e-7)  0 1000 six-sigma)
    (test-zipf make-zipf-generator 437 (- 1 1e-9)  0 1000 six-sigma)
    (test-zipf make-zipf-generator 437 (- 1 1e-12) 0 1000 six-sigma)

    ; Almost flat distribution
    (test-zipf make-zipf-generator 36 0.8     0 1000 six-sigma)
    (test-zipf make-zipf-generator 36 0.5     0 1000 six-sigma)
    (test-zipf make-zipf-generator 36 0.1     0 1000 six-sigma)

    ; A visit to crazy-town -- increasing, not decreasing exponent
    (test-zipf make-zipf-generator 36 0.0     0 1000 six-sigma)
    (test-zipf make-zipf-generator 36 -0.1    0 1000 six-sigma)
    (test-zipf make-zipf-generator 36 -1.0    0 1000 six-sigma)
    (test-zipf make-zipf-generator 36 -3.0    0 1000 six-sigma)

    ; More crazy with some Hurwicz on top.
    (test-zipf make-zipf-generator 16 0.0     0.5 1000 six-sigma)
    (test-zipf make-zipf-generator 16 -0.2    2.5 1000 six-sigma)
    (test-zipf make-zipf-generator 16 -1.3    10  1000 six-sigma)
    (test-zipf make-zipf-generator 16 -2.9    100 1000 six-sigma)

    ; (test-end "srfi-194-zipf")
    )
  )
