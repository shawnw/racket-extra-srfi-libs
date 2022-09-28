#lang typed/racket/base

;;; Math heavy parts of SRFI-194 generators, in Typed Racket for efficiency.

(require math/flonum racket/math)

(require/typed srfi/27
  [#:opaque RandomSource random-source?]
  [random-source-make-reals (-> RandomSource (-> Flonum))])

(provide %make-random-real-generator
         %make-normal-generator
         %make-exponential-generator
         %make-geometric-generator
         make-poisson/small make-poisson/large
         binomial-geometric binomial-rejection
         make-zipf-generator/zri make-zipf-generator/one
         make-ellipsoid-generator*)

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (* x x))

(: %make-random-real-generator (RandomSource Flonum Flonum -> (-> Flonum)))
(define (%make-random-real-generator source low-bound up-bound)
  (let ([rand-real-proc (random-source-make-reals source)])
    (lambda ()
     (define t (rand-real-proc))
      ;; alternative way of doing lowbound + t * (up-bound - low-bound)
      ;; is susceptible to rounding errors and would require clamping to be safe
      ;; (which in turn requires 144 for adjacent float function)
      (fl+ (fl* t low-bound)
           (fl* (fl- 1.0 t) up-bound)))))

(: %make-normal-generator (RandomSource Flonum Flonum -> (-> Flonum)))
(define (%make-normal-generator source mean deviation)
  (let ([rand-real-proc (random-source-make-reals source)]
        [state : (U Flonum False) #f])
    (lambda ()
      (if state
          (let ((result state))
            (set! state #f)
            (if result result +nan.0))
          (let ((r (flsqrt (* -2.0 (fllog (rand-real-proc)))))
                (theta (* 2.0 pi (rand-real-proc))))
            (set! state (fl+ mean (* deviation r (flcos theta))))
            (fl+ mean (* deviation r (flsin theta))))))))

(: %make-exponential-generator (RandomSource Flonum -> (-> Flonum)))
(define (%make-exponential-generator source mean)
  (let ((rand-real-proc (random-source-make-reals source)))
    (lambda ()
      (- (fl* mean (fllog (rand-real-proc)))))))

(: %make-geometric-generator (RandomSource Flonum -> (-> Integer)))
(define (%make-geometric-generator source p)
  (if (fl= (fl- p 1.) 0.0)
      ;; p is indistinguishable from 1.
      (lambda () 1)
      (let ((c (/ (fllog1p (- p))))
            (rand-real-proc (random-source-make-reals source)))
        (lambda ()
          (fl->exact-integer (flceiling (fl* c (fllog (rand-real-proc)))))))))

;; private
(: make-poisson/small ((-> Flonum) Flonum -> (-> Integer)))
(define (make-poisson/small rand-real-proc L)
  (lambda ()
    (do : Integer ((exp-L (flexp (- L)))
                   (k 0 (+ k 1))
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

(: binomial-geometric (RandomSource Integer Flonum -> (-> Integer)))
(define (binomial-geometric source n p)
  (let ((geom (%make-geometric-generator source p)))
    (lambda ()
      (let loop ((X -1)
                 (sum 0))
        (if (< n sum)
            X
            (loop (+ X 1)
                  (+ sum (geom))))))))

(: binomial-rejection (RandomSource Integer Flonum -> (-> Integer)))
(define (binomial-rejection source n p)
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
          (fl->exact-integer (flfloor (fl* (->fl (+ n 1)) p))))
         (rand-real-proc
          (random-source-make-reals source)))
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
(: make-zipf-generator/zri (RandomSource Integer Flonum Flonum -> (-> Integer)))
(define (make-zipf-generator/zri source n s q)

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
  (define dist (%make-random-real-generator source big-h-half big-h-n))

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
(: make-zipf-generator/one (RandomSource Integer Flonum Flonum -> (-> Integer)))
(define (make-zipf-generator/one source n s q)

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
    (define yms (* y _1-s))
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
  (define dist (%make-random-real-generator source big-h-half big-h-n))

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

(: make-ellipsoid-generator* (RandomSource FlVector -> (-> FlVector)))
(define (make-ellipsoid-generator* source axes)
  ; A vector of normal gaussian generators
  (define gaussg-vec
    (build-vector (flvector-length axes) (lambda (i) (%make-normal-generator source 0.0 1.0))))

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
  (define uni (%make-random-real-generator source 0.0 1.0))

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
