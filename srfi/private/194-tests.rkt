#lang racket/base

(module+ test
  ;; NOTE: for zipf tests data can be exported, this can be enabled by uncommenting appropriate lines.

  (require "../194.rkt" "../158.rkt" rackunit math/flonum (only-in racket/list argmin)
           srfi/1 srfi/27
           (only-in srfi/43 vector-for-each vector-fold vector-unfold))

  (define (vector-map func . vecs)
    (build-vector (vector-length (argmin vector-length vecs))
                  (lambda (i)
                    (apply func (map (lambda (v) (vector-ref v i)) vecs)))))

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

; Take REPS samples from unit sphere, verify random distribution.
;
; This test checks that:
; * Every sample has unit length, within numerical tolerance.
; * The REPS samples are uniformly distributed.
; * Rotations of the REPS samples are uniformly distributed.
(define (test-sphere sphereg dim-sizes REPS rotate?)
  (define random-int (random-source-make-integers (current-random-source)))
  (define random-real (random-source-make-reals (current-random-source)))
  (define N (- (vector-length dim-sizes) 1))

  ; Fix list of samples
  (define samples
    (generator->list (gtake sphereg REPS)))

  (define (l2-norm VEC)
    (sqrt (vector-fold
            (lambda (i sum x l) (+ sum (/ (* x x)
                                        (* l l))))
            0
            VEC
            dim-sizes)))

  ; Rotate the j'th amnd k'th coordinates of a vector VEC
  ; by cosine co and sine si
  (define (pair-rot VEC j k co si)
    (define oj (vector-ref VEC j))
    (define ok (vector-ref VEC k))
    (define nj (+ (* co oj) (* si ok)))
    (define nk (+ (* (- si) oj) (* co ok)))
    (list->vector
      (map (lambda (index)
             (cond
               ((= index j) nj)
               ((= index k) nk)
               (else (vector-ref VEC index))))
           (iota (vector-length VEC)))))

  ; Apply a random rotation to a collection of vectors
  (define how-many-rots
    (if (< 10 N) 10 N))

  (define (arb-rot VEC-LIST)
    (define j (random-int N))
    (define k (+ j 1 (random-int (- N j))))
    (define theta (* 3.14 (random-real)))
    (define co (cos theta))
    (define si (sin theta))
    (define rvl
      (map (lambda (vec)
             (pair-rot vec j k co si))
           VEC-LIST))
    (if (not (= 0 (random-int how-many-rots)))
        (arb-rot rvl)
        rvl))

  ; Expect a vector approaching zero. That is, each individual
  ; coordinate should be uniformly randomly distributed in the
  ; interval [-1,1]. The sum of REPS samples of these should
  ; converge to zero, within pi/2 sqrt(REPS).
  (define (converge-to-zero samples)
    (fold (lambda (acc sample) (vector-map + sample acc))
          (make-vector REPS 0.0)
          samples))

  (define (should-be-zero samples)
    (l2-norm (converge-to-zero samples)))

  (define (norm-should-be-zero samples)
    (/ (should-be-zero samples) (* 1.57 (sqrt REPS))))

  (define (check-zero samples)
    (define zz (norm-should-be-zero samples))
    (test-assert (< zz 1)))

  ; maximum allowed tolerance for radius deviation
  (define EPS (* 2e-15 (sqrt N)))

  ; Each individual sphere radius should be 1.0 to within float
  ; tolerance.
  (for-each
    (lambda (SAMP)
      (test-approximate 1.0 (l2-norm SAMP) EPS))
    samples)

  ; The distribution should be zero
  (check-zero samples)

  ; Rotate wildly. Should still be uniform.
  (when rotate?
    (for-each
      (lambda (junk) (check-zero (arb-rot samples)))
      (make-list 12))))

(define (test-flsphere sphereg dim-sizes REPS rotate?)
  (define random-int (random-source-make-integers (current-random-source)))
  (define random-real (random-source-make-reals (current-random-source)))
  (define N (- (flvector-length dim-sizes) 1))

  ; Fix list of samples
  (define samples
    (generator->list (gtake sphereg REPS)))

  (define (l2-norm VEC)
    (sqrt (for/sum ([x (in-flvector VEC)]
                    [l (in-flvector dim-sizes)])
            (/ (* x x) (* l l)))))

  ; Rotate the j'th amnd k'th coordinates of a vector VEC
  ; by cosine co and sine si
  (define (pair-rot VEC j k co si)
    (define oj (flvector-ref VEC j))
    (define ok (flvector-ref VEC k))
    (define nj (+ (* co oj) (* si ok)))
    (define nk (+ (* (- si) oj) (* co ok)))
    (list->flvector
      (map (lambda (index)
             (cond
               ((= index j) nj)
               ((= index k) nk)
               (else (flvector-ref VEC index))))
           (iota (flvector-length VEC)))))

  ; Apply a random rotation to a collection of vectors
  (define how-many-rots
    (if (< 10 N) 10 N))

  (define (arb-rot VEC-LIST)
    (define j (random-int N))
    (define k (+ j 1 (random-int (- N j))))
    (define theta (* 3.14 (random-real)))
    (define co (cos theta))
    (define si (sin theta))
    (define rvl
      (map (lambda (vec)
             (pair-rot vec j k co si))
           VEC-LIST))
    (if (not (= 0 (random-int how-many-rots)))
        (arb-rot rvl)
        rvl))

  ; Expect a vector approaching zero. That is, each individual
  ; coordinate should be uniformly randomly distributed in the
  ; interval [-1,1]. The sum of REPS samples of these should
  ; converge to zero, within pi/2 sqrt(REPS).
  (define (converge-to-zero samples)
    (fold (lambda (acc sample)
            (for/flvector ([a (in-flvector acc)]
                           [b (in-flvector sample)])
              (fl+ a b))

            #;(flvector-map + sample acc))
          (make-flvector REPS 0.0)
          samples))

  (define (should-be-zero samples)
    (l2-norm (converge-to-zero samples)))

  (define (norm-should-be-zero samples)
    (/ (should-be-zero samples) (* 1.57 (sqrt REPS))))

  (define (check-zero samples)
    (define zz (norm-should-be-zero samples))
    (test-assert (< zz 1)))

  ; maximum allowed tolerance for radius deviation
  (define EPS (* 2e-15 (sqrt N)))

  ; Each individual sphere radius should be 1.0 to within float
  ; tolerance.
  (for-each
    (lambda (SAMP)
      (test-approximate 1.0 (l2-norm SAMP) EPS))
    samples)

  ; The distribution should be zero
  (check-zero samples)

  ; Rotate wildly. Should still be uniform.
  (when rotate?
    (for-each
      (lambda (junk) (check-zero (arb-rot samples)))
      (make-list 12))))


(define (test-ball ballg dim-sizes)
  (define (l2-norm VEC)
    (sqrt
     (for/sum ([x (in-vector VEC)]
               [l (in-vector dim-sizes)])
       (/ (* x x) (* l l)))))

  (define (test-ball-generates-on-radius radius err)
    (test-assert
      (generator-any
        (lambda (vec)
          (define n (l2-norm vec))
          (and (> n (- radius err))
               (< n (+ radius err))))
        (gtake ballg 10000))))

  (define (test-ball-avg-zero N)
    (define vec-sum
      (generator-fold
        (lambda (vec acc)
          (vector-map + vec acc))
        (make-vector (vector-length dim-sizes) 0.0)
        (gtake ballg N)))
    (define avg-vec
      (vector-map
        (lambda (e)
          (/ e N))
        vec-sum))
    (define n (l2-norm avg-vec))
    (test-assert (< n 1)))

  (test-ball-generates-on-radius 0.0 0.1)
  (test-ball-generates-on-radius 0.5 0.1)
  (test-ball-generates-on-radius 1.0 0.1)

  (test-ball-avg-zero 5000))

(define (test-flball ballg dim-sizes)
  (define (l2-norm VEC)
    (sqrt (for/sum ([x (in-flvector VEC)]
                    [l (in-flvector dim-sizes)])
            (/ (* x x) (* l l)))))

  (define (test-ball-generates-on-radius radius err)
    (test-assert
      (generator-any
        (lambda (vec)
          (define n (l2-norm vec))
          (and (> n (- radius err))
               (< n (+ radius err))))
        (gtake ballg 10000))))

  (define (test-ball-avg-zero N)
    (define vec-sum
      (generator-fold
        (lambda (vec acc)
          (flvector-map + vec acc))
        (make-flvector (flvector-length dim-sizes) 0.0)
        (gtake ballg N)))
    (define avg-vec
      (flvector-map
        (lambda (e)
          (/ e N))
        vec-sum))
    (define n (l2-norm avg-vec))
    (test-assert (< n 1)))

  (test-ball-generates-on-radius 0.0 0.1)
  (test-ball-generates-on-radius 0.5 0.1)
  (test-ball-generates-on-radius 1.0 0.1)

  (test-ball-avg-zero 5000))

  (test-group "Test sphere"
              (reset-source!*)
              (test-sphere (make-sphere-generator 1) (vector 1.0 1.0) 200 #t)
              (test-sphere (make-sphere-generator 2) (vector 1.0 1.0 1.0) 200 #t)
              (test-flsphere (flmake-sphere-generator 3) (flvector 1.0 1.0 1.0 1.0) 200 #t)

              (reset-source!*)
              (test-sphere (make-ellipsoid-generator (vector 1.0 1.0)) (vector 1.0 1.0) 200 #t)
              (test-sphere (make-ellipsoid-generator (vector 1.0 1.0 1.0)) (vector 1.0 1.0 1.0) 200 #t)
              (test-flsphere (flmake-ellipsoid-generator (flvector 1.0 1.0 1.0 1.0)) (flvector 1.0 1.0 1.0 1.0) 200 #t)

              (reset-source!*)
              (test-sphere (make-ellipsoid-generator (vector 1.0 3.0)) (vector 1.0 3.0) 200 #f)
              (test-sphere (make-ellipsoid-generator (vector 1.0 3.0 5.0)) (vector 1.0 3.0 5.0) 200 #f)
              (test-flsphere (flmake-ellipsoid-generator (flvector 1.0 3.0 5.0 7.0)) (flvector 1.0 3.0 5.0 7.0) 200 #f)
              (reset-source!*)
              (test-ball (make-ball-generator 2) (vector 1.0 1.0))
              (test-ball (make-ball-generator 3) (vector 1.0 1.0 1.0))
              (test-ball (make-ball-generator (vector 1.0 3.0)) (vector 1.0 3.0))
              (test-ball (make-ball-generator (flvector 1.0 3.0)) (vector 1.0 3.0))
              (test-flball (flmake-ball-generator (flvector 1.0 3.0 5.0)) (flvector 1.0 3.0 5.0))
              (test-flball (flmake-ball-generator (vector 1.0 3.0 5.0)) (flvector 1.0 3.0 5.0)))

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
                                    (vector-fold (lambda (i sum x) (+ sum (* x x))) 0 norm-dist)
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
