#lang racket/base

(require racket/contract "141.rkt")
(module+ test (require rackunit srfi/9))

(provide
 (contract-out
  [bisect-left (-> any/c any/c (-> any/c integer? any/c) (-> any/c any/c any/c) integer? integer? integer?)]
  [bisect-right (-> any/c any/c (-> any/c integer? any/c) (-> any/c any/c any/c) integer? integer? integer?)]
  [bisection (case-> (-> (-> any/c integer? any/c) (-> any/c (values integer? integer?))
                         (values (case->
                                  (-> any/c any/c (-> any/c any/c any/c) integer?)
                                  (-> any/c any/c (-> any/c any/c any/c) integer? integer? integer?))
                                 (case->
                                  (-> any/c any/c (-> any/c any/c any/c) integer?)
                                  (-> any/c any/c (-> any/c any/c any/c) integer? integer? integer?))))
                     (-> (-> any/c integer? any/c)
                         (values (case->
                                  (-> any/c any/c (-> any/c any/c any/c) integer?)
                                  (-> any/c any/c (-> any/c any/c any/c) integer? integer? integer?))
                                 (case->
                                  (-> any/c any/c (-> any/c any/c any/c) integer?)
                                  (-> any/c any/c (-> any/c any/c any/c) integer? integer? integer?)))))]
  [vector-bisect-left (case-> (-> vector? any/c (-> any/c any/c any/c) integer?)
                              (-> vector? any/c (-> any/c any/c any/c) integer? integer? integer?))]
  [vector-bisect-right (case-> (-> vector? any/c (-> any/c any/c any/c) integer?)
                               (-> vector? any/c (-> any/c any/c any/c) integer? integer? integer?))]))

(define (bisect-left a val ref less? lo hi)
  (if (>= lo hi) lo
      (let ((mid (floor-quotient (+ lo hi) 2)))
        (if (less? (ref a mid) val)
            (bisect-left a val ref less? (+ mid 1) hi)
            (bisect-left a val ref less? lo mid)))))

(define (bisect-right a val ref less? lo hi)
  (if (>= lo hi) lo
      (let ((mid (floor-quotient (+ lo hi) 2)))
        (if (less? val (ref a mid))
            (bisect-right a val ref less? lo mid)
            (bisect-right a val ref less? (+ mid 1) hi)))))

(define bisection
  (case-lambda
    ((ref lo-hi-proc)
     (values
      (case-lambda
       ((a val less?)
        (let-values (((lo hi) (lo-hi-proc a)))
          (bisect-left a val ref less? lo hi)))
       ((a val less? lo hi)
        (bisect-left a val ref less? lo hi)))
      (case-lambda
       ((a val less?)
        (let-values (((lo hi) (lo-hi-proc a)))
          (bisect-right a val ref less? lo hi)))
       ((a val less? lo hi)
        (bisect-right a val ref less? lo hi)))))
    ((ref)
     (bisection ref
                (lambda (a) (error "both lo and hi arguments must be given to this procedure"))))))

(define-values (vector-bisect-left vector-bisect-right)
  (bisection vector-ref (lambda (v) (values 0 (vector-length v)))))

(module+ test
  (define-syntax-rule (test name expected funccall)
    (test-equal? name funccall expected))
  (define-syntax-rule (test-group name tests ...)
    (begin tests ...))

  ;; OEIS A003056
  (define (nth-inverted-triangular n)
    ;; The OEIS formula yields the sequence 0, 1, 2, 2, 3, 3, 3, ...
    ;; instead of 0, 1, 1, 2, 2, 2, 3, 3, 3, 3 ..., so this is
    ;; implemented in terms of A002024, which makes it correct.
    (- (floor (+ 1/2 (inexact->exact (sqrt (* 2 (+ n 1)))))) 1))
  (define (inverted-triangular-ref _ n) (nth-inverted-triangular n))

  (test-group "Generalized bisection procedures"
              (test "left bisection for n for the nth inverted triangular number ≥ x."
                    3 (bisect-left #f 2 inverted-triangular-ref < 0 100))
              (test "right bisection for n for the nth inverted triangular number < x."
                    6 (bisect-right #f 2 inverted-triangular-ref < 0 100))

              ;; test for integer overflow
              (test "left bisection for a massive sequence size"
                    68719439628
                    (bisect-left #f 370727 inverted-triangular-ref < 0 (expt 2 48)))
              (test "right bisection for a massive sequence size"
                    68719810356
                    (bisect-right #f 370727 inverted-triangular-ref < 0 (expt 2 48))))

  (test-group "Bisections over vectors"
              (test "left bisection over a vector"
                    1 (vector-bisect-left #(1 2 2 3 5) 2 <))
              (test "right bisection over a vector"
                    3 (vector-bisect-right #(1 2 2 3 5) 2 <)))

  (test-group "Bisections over vectors of things other than numbers"
              (test "basic left bisection over a vector of characters"
                    1 (vector-bisect-left #(#\A #\Z #\a #\z) #\B char<?))
              (test "basic right bisection over a vector of characters"
                    2 (vector-bisect-right #(#\A #\Z #\a #\z) #\Z char<?)))

  ;; These tests, meant to ensure that everything works properly on
  ;; larger vectors, may not work in Schemes which don’t provide full
  ;; Unicode support.
  ;;
  ;; The list of BMP blocks ends before the surrogate pairs because some
  ;; Schemes (arguably correctly) don’t allow unpaired surrogates as
  ;; character objects.
  (define low-bmp-block-starts
    #(#\null #\u80 #\u100 #\u180 #\u250 #\u2B0 #\u300 #\u370 #\u400
      #\u500 #\u530 #\u590 #\u600 #\u700 #\u750 #\u780 #\u7C0 #\u800
      #\u840 #\u860 #\u8A0 #\u900 #\u980 #\uA00 #\uA80 #\uB00 #\uB80
      #\uC00 #\uC80 #\uD00 #\uD80 #\uE00 #\uE80 #\uF00 #\u1000 #\u10A0
      #\u1100 #\u1200 #\u1380 #\u13A0 #\u1400 #\u1680 #\u16A0 #\u1700
      #\u1720 #\u1740 #\u1760 #\u1780 #\u1800 #\u18B0 #\u1900 #\u1950
      #\u1980 #\u19E0 #\u1A00 #\u1A20 #\u1AB0 #\u1B00 #\u1B80 #\u1BC0
      #\u1C00 #\u1C50 #\u1C80 #\u1C90 #\u1CC0 #\u1CD0 #\u1D00 #\u1D80
      #\u1DC0 #\u1E00 #\u1F00 #\u2000 #\u2070 #\u20A0 #\u20D0 #\u2100
      #\u2150 #\u2190 #\u2200 #\u2300 #\u2400 #\u2440 #\u2460 #\u2500
      #\u2580 #\u25A0 #\u2600 #\u2700 #\u27C0 #\u27F0 #\u2800 #\u2900
      #\u2980 #\u2A00 #\u2B00 #\u2C00 #\u2C60 #\u2C80 #\u2D00 #\u2D30
      #\u2D80 #\u2DE0 #\u2E00 #\u2E80 #\u2F00 #\u2FF0 #\u3000 #\u3040
      #\u30A0 #\u3100 #\u3130 #\u3190 #\u31A0 #\u31C0 #\u31F0 #\u3200
      #\u3300 #\u3400 #\u4DC0 #\u4E00 #\uA000 #\uA490 #\uA4D0 #\uA500
      #\uA640 #\uA6A0 #\uA700 #\uA720 #\uA800 #\uA830 #\uA840 #\uA880
      #\uA8E0 #\uA900 #\uA930 #\uA960 #\uA980 #\uA9E0 #\uAA00 #\uAA60
      #\uAA80 #\uAAE0 #\uAB00 #\uAB30 #\uAB70 #\uABC0 #\uAC00 #\uD7B0))

  (test-group "Bisections over larger vectors of non-numbers"
              (test "find the number of the Unicode block for schwa"
                    4 (- (vector-bisect-right low-bmp-block-starts #\u0259 char<?) 1))
              (test "find the number of the Unicode block for Devanagari long a"
                    21 (- (vector-bisect-right low-bmp-block-starts #\u0906 char<?) 1))
              (test "find the number of the Unicode block for em dash"
                    71 (- (vector-bisect-right low-bmp-block-starts #\u2014 char<?) 1))
              (test "find the number of the Unicode block for Cherokee small he"
                    144 (- (vector-bisect-right low-bmp-block-starts #\uAB7E char<?) 1)))

  ;; Example of a vector with negative indexes: the mvector accessor for
  ;; a vector guarantees that index 0 will always refer to the median
  ;; value in the vector, provided the vector is sorted.
  (define-record-type <mvector>
    (vector->mvector vector)
    mvector?
    (vector mvector-vector mvector-vector-set!))

  (define (mvector . vals)
    (vector->mvector (apply vector vals)))

  (define (mvector-size mvector) (vector-length (mvector-vector mvector)))
  (define (mvector-first-idx mvector)
    (- (floor-quotient (mvector-size mvector) 2)))
  (define (mvector-last-idx mvector)
    (- (ceiling-quotient (mvector-size mvector) 2)
       1))
  (define (mvector-ref mvector idx)
    (vector-ref (mvector-vector mvector)
                (+ (abs (mvector-first-idx mvector)) idx)))

  (define-values (mvector-bisect-left mvector-bisect-right)
    (bisection mvector-ref
               (lambda (mv)
                 (values (mvector-first-idx mv)
                         (+ 1 (mvector-last-idx mv))))))

  (define mvtest-even (mvector 1 1 2 3 5 8 13 21))
  (define mvtest-odd (mvector 1 1 2 3 5 8 13 21 34))

  (test-group "Implementation of example sequence type with negative indexes"
              (test "first-idx of even-sized mvector" -4 (mvector-first-idx mvtest-even))
              (test "first-idx of odd-sized mvector" -4 (mvector-first-idx mvtest-odd))

              (test "last-idx of even-size mvector" 3 (mvector-last-idx mvtest-even))
              (test "last-idx of odd-size mvector" 4 (mvector-last-idx mvtest-odd))

              (test "min value of even-sized mvector"
                    1 (mvector-ref mvtest-even (mvector-first-idx mvtest-even)))
              (test "min value of odd-sized mvector"
                    1 (mvector-ref mvtest-odd (mvector-first-idx mvtest-odd)))

              (test "max value of even-sized mvector"
                    21 (mvector-ref mvtest-even (mvector-last-idx mvtest-even)))
              (test "max value of odd-sized mvector"
                    34 (mvector-ref mvtest-odd (mvector-last-idx mvtest-odd)))

              (test "median value of odd-sized mvector"
                    5 (mvector-ref mvtest-odd 0))
              (test "median value of even-sized mvector"
                    5 (mvector-ref mvtest-even 0)))

  (test-group "Bisection of sequence type with negative indexes"
              (test "left bisection returning negative result"
                    -2 (mvector-bisect-left mvtest-odd 2 <))
              (test "right bisection returning negative result"
                    -1 (mvector-bisect-right mvtest-odd 2 <))

              (test "left bisection returning positive result"
                    1 (mvector-bisect-left mvtest-odd 8 <))
              (test "right bisection returning positive result"
                    2 (mvector-bisect-right mvtest-odd 9 <))))
