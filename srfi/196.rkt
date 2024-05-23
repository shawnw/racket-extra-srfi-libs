#lang racket/base

(require racket/contract
         (only-in srfi/1 reduce unfold xcons every concatenate)
         "133.rkt" "145.rkt")
(module+ test (require rackunit))

(provide
 (contract-out
  [range? predicate/c]
  [range (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? any/c) range?)]
  [numeric-range (->* (real? real?) (real?) range?)]
  [iota-range (->* (exact-nonnegative-integer?) (real? real?) range?)]
  [vector-range (-> vector? range?)]
  [string-range (-> string? range?)]
  [range-append (-> range? ... range?)]
  [range-reverse (-> range? range?)]
  [range=? (-> (-> any/c any/c any/c) range? range? ... boolean?)]
  [range-length (-> range? exact-nonnegative-integer?)]
  [range-ref (-> range? exact-nonnegative-integer? any/c)]
  [range-first (-> (and/c range? (not/c range-empty?)) any/c)]
  [range-last (-> (and/c range? (not/c range-empty?)) any/c)]
  [range-split-at (-> range? exact-nonnegative-integer? (values range? range?))]
  [subrange (-> range? exact-nonnegative-integer? exact-nonnegative-integer? range?)]
  [range-segment (-> range? exact-positive-integer? (listof range?))]
  [range-take (-> range? exact-nonnegative-integer? range?)]
  [range-take-right (-> range? exact-nonnegative-integer? range?)]
  [range-drop (-> range? exact-nonnegative-integer? range?)]
  [range-drop-right (-> range? exact-nonnegative-integer? range?)]
  [range-count (-> procedure? range? range? ... exact-nonnegative-integer?)]
  [range-any (-> procedure? range? range? ... boolean?)]
  [range-every (-> procedure? range? range? ... boolean?)]
  [range-map (-> procedure? range? range? ... range?)]
  [range-map->list (-> procedure? range? range? ... list?)]
  [range-map->vector (-> procedure? range? range? ... vector?)]
  [range-for-each (-> procedure? range? range? ... void?)]
  [range-filter-map (-> procedure? range? range? ... range?)]
  [range-filter-map->list (-> procedure? range? range? ... list?)]
  [range-filter (-> (-> any/c any/c) range? range?)]
  [range-filter->list (-> (-> any/c any/c) range? list?)]
  [range-remove (-> (-> any/c any/c) range? range?)]
  [range-remove->list (-> (-> any/c any/c) range? list?)]
  [range-fold (-> procedure? any/c range? range? ... any/c)]
  [range-fold-right (-> procedure? any/c range? range? ... any/c)]
  [range-index (-> procedure? range? range? ... (or/c exact-nonnegative-integer? #f))]
  [range-index-right (-> procedure? range? range? ... (or/c exact-nonnegative-integer? #f))]
  [range-take-while (-> (-> any/c any/c) range? range?)]
  [range-take-while-right (-> (-> any/c any/c) range? range?)]
  [range-drop-while (-> (-> any/c any/c) range? range?)]
  [range-drop-while-right (-> (-> any/c any/c) range? range?)]
  [vector->range (-> vector? range?)]
  [range->string (-> range? string?)]
  [range->list (-> range? list?)]
  [range->generator (-> range? (-> any/c))]
  [range->vector (-> range? vector?)]
  ;;; Extra functions
  [range-empty? (-> range? boolean?)]
  [in-range-object (-> range? sequence?)]
  ))

(define (range-empty? r) (= (range-length r) 0))

(define (in-range-object r)
  (make-do-sequence
   (lambda ()
     (values
      (lambda (i) (range-ref r i))
      add1
      0
      (lambda (i) (< i (range-length r)))
      #f
      #f))))

;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Find the least element of a list non-empty of naturals. If an element
;; is zero, returns it immediately.
(define (short-minimum ns)
  (call-with-escape-continuation
   (lambda (return)
     (reduce (lambda (n s)
               (if (zero? n) (return n) (min n s)))
             0
             ns))))

(define (sum ns) (reduce + 0 ns))

#;(define (string->vector s)
  (for/vector #:length (string-length s) ([ch (in-string s)]) ch))

(struct range (start-index length indexer complexity)
  #:sealed
  #:name <range>
  #:constructor-name raw-range
  #:property prop:sequence
  (lambda (r) (in-range-object r)))

;; Maximum number of indexers to compose with range-reverse and
;; range-append before a range is expanded with vector-range.
;; This may need adjustment.
(define %range-maximum-complexity 16)

;; Returns an empty range which is otherwise identical to r.
(define (%empty-range-from r)
  (struct-copy <range> r [length 0]))

(define (threshold? k)
  (> k %range-maximum-complexity))

(define (%range-valid-index name r index)
  (when (or (< index 0) (>= index (range-length r)))
    (raise-range-error name "range" "" index r 0 (- (range-length r) 1))))

;; As the previous check, but bound is assumed to be exclusive.
(define (%range-valid-bound name r bound)
  (when (or (< bound 0) (> bound (range-length r)))
    (raise-range-error name "range" "" bound r 0 (range-length r))))

;;;; Constructors

;; The primary range constructor does some extra consistency checking.
(define (range length indexer)
  ;(assume (exact-nonnegative-integer? length))
  ;(assume (procedure? indexer))
  (raw-range 0 length indexer 0))

(define numeric-range
  (case-lambda
    ((start end) (numeric-range start end 1))
    ((start end step)
     ;(assume (real? start))
     ;(assume (real? end))
     (assume (not (zero? step)) "numeric-range: zero-valued step")
     (let ((len (inexact->exact (ceiling (max 0 (/ (- end start) step))))))
       ;; Try to ensure that we can compute a correct range from the
       ;; given parameters, i.e. one not plagued by roundoff errors.
       (assume (cond ((and (positive? step) (< start end))
                      (and (> (+ start step) start)
                           (< (+ start (* (- len 1) step)) end)))
                     ((and (negative? step) (> start end))
                      (and (< (+ start step) start)
                           (> (+ start (* (- len 1) step)) end)))
                     (else #t))
               "numeric-range: invalid parameters")
       (raw-range 0 len (lambda (n) (+ start (* n step))) 0)))))

;; TODO: Consider possible round-off bugs.
(define iota-range
  (case-lambda
    ((len) (iota-range len 0 1))
    ((len start) (iota-range len start 1))
    ((len start step)
     ;(assume (exact-nonnegative-integer? len))
     ;(assume (real? start))
     ;(assume (real? step))
     (raw-range 0
                len
                (cond ((and (zero? start) (= step 1)) (lambda (i) i))
                      ((= step 1) (lambda (i) (+ start i)))
                      ((zero? start) (lambda (i) (* step i)))
                      (else (lambda (i) (+ start (* step i)))))
                0))))

(define (vector-range vec)
  ;(assume (vector? vec))
  (raw-range 0 (vector-length vec) (lambda (i) (vector-ref vec i)) 0))

;; This implementation assumes that string-ref is O(n), as would be
;; the case with UTF-8.  If an implementation has an O(1) string-ref,
;; the following version is preferable:
;;
;; (raw-range 0 (string-length s) (lambda (i) (string-ref s i))))
;;
(define (string-range s)
  ;(assume (string? s))
  (raw-range 0 (string-length s) (lambda (i) (string-ref s i)) 0)
  #;(vector-range (string->vector s)))

(define (%range-maybe-vectorize r)
  (if (threshold? (range-complexity r))
      (vector-range (range->vector r))
      r))

;;;; Accessors

(define (range-ref r index)
  ;(assume (range? r))
  (%range-valid-index 'range-ref r index)
  ((range-indexer r) (+ index (range-start-index r))))

;; A portable implementation can't rely on inlining, but it
;; can rely on macros.
(define-syntax %range-ref-no-check
  (syntax-rules ()
    ((_ r index)
     ((range-indexer r) (+ index (range-start-index r))))))

(define (range-first r) (%range-ref-no-check r (range-start-index r)))

(define (range-last r) (%range-ref-no-check r (- (range-length r) 1)))

;;;; Predicates

(define range=?
  (case-lambda
    ((equal ra rb)                      ; two-range fast path
     ;(assume (procedure? equal))
     ;(assume (range? ra))
     (%range=?-2 equal ra rb))
    ((equal . rs)                       ; variadic path
     ;(assume (procedure? equal))
     ;(assume (pair? rs))
     (let ((ra (car rs)))
       ;(assume (range? ra))
       (every (lambda (rb) (%range=?-2 equal ra rb)) (cdr rs))))))

(define (%range=?-2 equal ra rb)
  ;(assume (range? rb))
  (or (eqv? ra rb)                      ; quick check
      (let ((la (range-length ra)))
        (and (= la (range-length rb))
             (if (zero? la)
                 #t                     ; all empty ranges are equal
                 (let lp ((i 0))
                   (cond ((= i la) #t)
                         ((not (equal (%range-ref-no-check ra i)
                                      (%range-ref-no-check rb i)))
                          #f)
                         (else (lp (+ i 1))))))))))

;;;; Iteration

(define (range-split-at r index)
  ;(assume (range? r))
  (%range-valid-bound 'range-split-at r index)
  (cond ((= index 0) (values (%empty-range-from r) r))
        ((= index (range-length r)) (values r (%empty-range-from r)))
        (else
         (let ((indexer (range-indexer r)) (k (range-complexity r)))
           (values (raw-range (range-start-index r) index indexer k)
                   (raw-range index (- (range-length r) index) indexer k))))))

(define (subrange r start end)
  ;(assume (range? r))
  (%range-valid-index 'subrange r start)
  (%range-valid-bound 'subrange  r end)
  (assume (not (negative? (- end start))) "subrange: invalid subrange")
  (if (and (zero? start) (= end (range-length r)))
      r
      (raw-range (+ (range-start-index r) start)
                 (- end start)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-segment r k)
  ;(assume (range? r))
  ;(assume (and (exact-integer? k) (positive? k)))
  (let ((len (range-length r))
        (%subrange-no-check
         (lambda (s e)
           (raw-range (+ (range-start-index r) s)
                      (- e s)
                      (range-indexer r)
                      (range-complexity r)))))
    (unfold (lambda (i) (>= i len))
            (lambda (i) (%subrange-no-check i (min len (+ i k))))
            (lambda (i) (+ i k))
            0)))

(define (range-take r count)
  ;(assume (range? r))
  (%range-valid-bound 'range-take r count)
  (cond ((zero? count) (%empty-range-from r))
        ((= count (range-length r)) r)
        (else (raw-range (range-start-index r)
                         count
                         (range-indexer r)
                         (range-complexity r)))))

(define (range-take-right r count)
  ;(assume (range? r))
  (%range-valid-bound 'range-take-right r count)
  (cond ((zero? count) (%empty-range-from r))
        ((= count (range-length r)) r)
        (else
         (raw-range (+ (range-start-index r) (- (range-length r) count))
                    count
                    (range-indexer r)
                    (range-complexity r)))))

(define (range-drop r count)
  ;(assume (range? r))
  (%range-valid-bound 'range-drop r count)
  (if (zero? count)
      r
      (raw-range (+ (range-start-index r) count)
                 (- (range-length r) count)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-drop-right r count)
  ;(assume (range? r))
  (%range-valid-bound 'range-drop-right r count)
  (if (zero? count)
      r
      (raw-range (range-start-index r)
                 (- (range-length r) count)
                 (range-indexer r)
                 (range-complexity r))))

(define (range-count pred r . rs)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-1 (lambda (c x) (if (pred x) (+ c 1) c)) 0 r)
      (apply range-fold                 ; variadic path
             (lambda (c . xs)
               (if (apply pred xs) (+ c 1) c))
             0
             r
             rs)))

(define (range-any pred r . rs)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (call-with-escape-continuation
   (lambda (return)
     (if (null? rs)                        ; one-range fast path
         (%range-fold-1 (lambda (_last x)
                          (cond ((pred x) => return)
                                (else #f)))
                        #f
                        r)
         (apply range-fold                 ; variadic path
                (lambda (_last . xs)
                  (cond ((apply pred xs) => return)
                        (else #f)))
                #f
                r
                rs)))))

(define (range-every pred r . rs)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (call-with-escape-continuation
   (lambda (return)
     (if (null? rs)                     ; one-range fast path
         (%range-fold-1 (lambda (_ x) (or (pred x) (return #f))) #t r)
         (apply range-fold              ; variadic path
                (lambda (_ . xs) (or (apply pred xs) (return #f)))
                #t
                r
                rs)))))

(define (range-map proc . rs)
  ;(assume (pair? rs))
  (vector-range (apply range-map->vector proc rs)))

(define (range-filter-map proc . rs)
  ;(assume (pair? rs))
  (vector-range (list->vector (apply range-filter-map->list proc rs))))

(define (range-map->list proc r . rs)
  ;(assume (procedure? proc))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-right-1 (lambda (res x) (cons (proc x) res)) '() r)
      (apply range-fold-right           ; variadic path
             (lambda (res . xs) (cons (apply proc xs) res))
             '()
             r
             rs)))

(define (range-filter-map->list proc r . rs)
  (if (null? rs)                        ; one-range fast path
      (%range-fold-right-1 (lambda (res x)
                             (cond ((proc x) =>
                                             (lambda (elt) (cons elt res)))
                                   (else res)))
                           '()
                           r)
      (apply range-fold-right           ; variadic path
             (lambda (res . xs)
               (cond ((apply proc xs) => (lambda (elt) (cons elt res)))
                     (else res)))
             '()
             r
             rs)))

(define (range-map->vector proc r . rs)
  ;(assume (procedure? proc))
  ;(assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (vector-unfold (lambda (i) (proc (%range-ref-no-check r i)))
                     (range-length r))
      (let ((rs* (cons r rs)))          ; variadic path
        (vector-unfold (lambda (i)
                         (apply proc (map (lambda (r)
                                            (%range-ref-no-check r i))
                                          rs*)))
                       (short-minimum (map range-length rs*))))))

(define (range-for-each proc r . rs)
  ;(assume (procedure? proc))
  ;(assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let ((len (range-length r)))
        (let lp ((i 0))
          (cond ((= i len) (void))
                (else (proc (%range-ref-no-check r i))
                      (lp (+ i 1))))))
      (let* ((rs* (cons r rs))          ; variadic path
             (len (short-minimum (map range-length rs*))))
        (let lp ((i 0))
          (cond ((= i len) (void))
                (else
                 (apply proc (map (lambda (r)
                                    (%range-ref-no-check r i))
                                  rs*))
                 (lp (+ i 1))))))))

(define (%range-fold-1 proc nil r)
  ;(assume (procedure? proc))
  ;(assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0) (acc nil))
      (if (= i len)
          acc
          (lp (+ i 1) (proc acc (%range-ref-no-check r i)))))))

(define range-fold
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     ;(assume (procedure? proc))
     ;(assume (pair? rs))
     (let ((len (short-minimum (map range-length rs))))
       (let lp ((i 0) (acc nil))
         (if (= i len)
             acc
             (lp (+ i 1)
                 (apply proc acc (map (lambda (r)
                                        (%range-ref-no-check r i))
                                      rs)))))))))

(define (%range-fold-right-1 proc nil r)
  ;(assume (procedure? proc))
  ;(assume (range? r))
  (let ((len (range-length r)))
    (let rec ((i 0))
      (if (= i len)
          nil
          (proc (rec (+ i 1)) (%range-ref-no-check r i))))))

(define range-fold-right
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-right-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     ;(assume (procedure? proc))
     ;(assume (pair? rs))
     (let ((len (short-minimum (map range-length rs))))
       (let rec ((i 0))
         (if (= i len)
             nil
             (apply proc
                    (rec (+ i 1))
                    (map (lambda (r) (%range-ref-no-check r i)) rs))))))))

(define (range-filter pred r)
  (vector-range (list->vector (range-filter->list pred r))))

(define (range-filter->list pred r)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) (cons x xs) xs))
                    '()
                    r))

(define (range-remove pred r)
  (vector-range (list->vector (range-remove->list pred r))))

(define (range-remove->list pred r)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) xs (cons x xs)))
                    '()
                    r))

(define (range-reverse r)
  ;(assume (range? r))
  (%range-maybe-vectorize
   (raw-range (range-start-index r)
              (range-length r)
              (lambda (n)
                ((range-indexer r) (- (range-length r) 1 n)))
              (+ 1 (range-complexity r)))))

(define range-append
  (case-lambda
    (() (raw-range 0 0 #f 0))
    ((r) r)                             ; one-range fast path
    ((ra rb)                            ; two-range fast path
     (let ((la (range-length ra))
           (lb (range-length rb)))
       (%range-maybe-vectorize          ; FIXME: should be lazy.
        (raw-range 0
                   (+ la lb)
                   (lambda (i)
                     (if (< i la)
                         (%range-ref-no-check ra i)
                         (%range-ref-no-check rb (- i la))))
                   (+ 2 (range-complexity ra) (range-complexity rb))))))
    (rs                                 ; variadic path
     (let ((lens (map range-length rs)))
       (%range-maybe-vectorize          ; FIXME: should be lazy.
        (raw-range 0
                   (sum lens)
                   (lambda (i)
                     (let lp ((i i) (rs rs) (lens lens))
                       (if (< i (car lens))
                           (%range-ref-no-check (car rs) i)
                           (lp (- i (car lens)) (cdr rs) (cdr lens)))))
                   (+ (length rs) (sum (map range-complexity rs)))))))))

;;;; Searching

(define (range-index pred r . rs)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let ((len (range-length r)))
        (let lp ((i 0))
          (cond ((= i len) #f)
                ((pred (%range-ref-no-check r i)) i)
                (else (lp (+ i 1))))))
      (let* ((rs* (cons r rs))          ; variadic path
             (len (short-minimum (map range-length rs*))))
        (let lp ((i 0))
          (cond ((= i len) #f)
                ((apply pred
                        (map (lambda (s) (%range-ref-no-check s i))
                             rs*))
                 i)
                (else (lp (+ i 1))))))))

(define (range-index-right pred r . rs)
  ;(assume (procedure? pred))
  ;(assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (let lp ((i (- (range-length r) 1)))
        (cond ((< i 0) #f)
              ((pred (%range-ref-no-check r i)) i)
              (else (lp (- i 1)))))
      (let ((len (range-length r))      ; variadic path
            (rs* (cons r rs)))
        (assume (every (lambda (s) (= len (range-length s))) rs)
                "range-index-right: ranges must be of the same length")
        (let lp ((i (- len 1)))
          (cond ((< i 0) #f)
                ((apply pred
                        (map (lambda (s) (%range-ref-no-check s i))
                             rs*))
                 i)
                (else (lp (- i 1))))))))

(define (range-take-while pred r)
  (cond ((range-index (lambda (x) (not (pred x))) r) =>
                                                     (lambda (i) (range-take r i)))
        (else r)))

(define (range-take-while-right pred r)
  (cond ((range-index-right (lambda (x) (not (pred x))) r) =>
                                                           (lambda (i) (range-take-right r (- (range-length r) 1 i))))
        (else r)))

(define (range-drop-while pred r)
  (cond ((range-index (lambda (x) (not (pred x))) r) =>
                                                     (lambda (i) (range-drop r i)))
        (else (%empty-range-from r))))

(define (range-drop-while-right pred r)
  (cond ((range-index-right (lambda (x) (not (pred x))) r) =>
                                                           (lambda (i) (range-drop-right r (- (range-length r) 1 i))))
        (else (%empty-range-from r))))

;;;; Conversion

(define (range->list r)
  (range-fold-right xcons '() r))

(define (range->vector r)
  ;(assume (range? r))
  (vector-unfold (lambda (i) (%range-ref-no-check r i))
                 (range-length r)))

(define (range->string r)
  ;(assume (range? r))
  (let ((res (make-string (range-length r))))
    (range-fold (lambda (i c) (string-set! res i c) (+ i 1)) 0 r)
    res))

(define (vector->range vec)
  ;(assume (vector? vec))
  (vector-range (vector-copy vec)))

(define (range->generator r)
  ;(assume (range? r))
  (let ((i 0) (len (range-length r)))
    (lambda ()
      (if (>= i len)
          eof
          (begin
            (let ((v (%range-ref-no-check r i)))
              (set! i (+ i 1))
              v))))))

(module+ test
  (require (only-in srfi/1 iota take drop fold count filter-map fold-right list-index take-while drop-while remove)
           (only-in "158.rkt" generator->list))

  (define-syntax check
    (syntax-rules (=>)
      ((_ expr => expected)
       (check-equal? expr expected))))

  (define (print-header msg) (void))

  ;;;; Utility

  (define (square n) (* n n))

  (define (identity x) x)

  (define-syntax constantly
    (syntax-rules ()
      ((_ obj) (lambda _ obj))))

  (define always (constantly #t))
  (define never (constantly #f))

  (define (range=/eqv? ra rb)
    (range=? eqv? ra rb))

  (define (%range-empty? r) (zero? (range-length r)))

  ;;;; Test ranges

  (define test-num-range (numeric-range 10 30))

  (define test-num-seq (iota 20 10))

  (define test-empty-range (numeric-range 0 0))

  ;; Produces the range {#f, #t}.
  (define test-bool-range
    (range 2 (lambda (n) (not (zero? n)))))

  ;;;; Conversion

  (define (check-conversion)
    (print-header "Running conversion tests...")

    (check (range->list test-empty-range) => '())
    (check (range->list test-bool-range)  => '(#f #t))
    (check (range->list test-num-range)   => test-num-seq)

    (check (generator->list (range->generator test-num-range))
           => test-num-seq)

    (check (vector->list (range->vector test-num-range)) => test-num-seq)

    (check (range->string test-empty-range) => "")
    (let ((s "0123456789"))
      (check (range->string (string-range s)) => s))

    (let* ((vec (vector 1 3 5 7 9))
           (vrange (vector->range vec)))
      (check (range-length vrange)  => (vector-length vec))
      (check (range-first vrange)   => (vector-ref vec 0))
      (check (range-last vrange)    => (vector-ref vec 4))
      (check (range->vector vrange) => vec)
      (check (range->list (begin (vector-set! vec 0 0) vrange))
             => '(1 3 5 7 9)))
    )

  (define (check-constructors)
    (print-header "Running constructor tests...")

    (check (%range-empty? (numeric-range 1 1))      => #t)
    (check (range->list (numeric-range -5 -1))      => (iota 4 -5))
    (check (range->list (numeric-range 1 -5 -1))    => (iota 6 1 -1))
    (check (range->list (numeric-range 4/3 16/3))   => (iota 4 4/3))
    (check (range->list (numeric-range 0 9 4))      => (iota 3 0 4))
    (check (%range-empty? (numeric-range 0 10 -1))  => #t)
    (check (%range-empty? (numeric-range 0 -10))    => #t)
    (check (range->list (numeric-range 5 1 -1))     => (iota 4 5 -1))
    (check (range->list (numeric-range -2 2))       => (iota 4 -2))
    (check (range->list (numeric-range 2 -2 -1))    => (iota 4 2 -1))
    (check (range->list (numeric-range -4 -8 -1))   => (iota 4 -4 -1))
    (check (range->list (numeric-range -1 -4 -2/3)) => (iota 5 -1 -2/3))

    (check (range=/eqv? (iota-range 10 0 0)
                        (range 10 (lambda (_) 0)))
           => #t)
    (check (%range-empty? (iota-range 0))     => #t)
    (check (range->list (iota-range 10))      => (iota 10))
    (check (range->list (iota-range 10 0))    => (iota 10))
    (check (range->list (iota-range 10 0 1))  => (iota 10))
    (check (range->list (iota-range 10 10 2)) => (iota 10 10 2))
    (check (range->list (iota-range 10 0 -1)) => (iota 10 0 -1))
    (check (range->list (iota-range 10 5 -2)) => (iota 10 5 -2))
    (check (range->list (iota-range 10 1/2))  => (iota 10 1/2))

    (let ((vec (vector 1 3 5 7 9)))
      (check (range-length (vector-range vec))  => (vector-length vec))
      (check (range-first (vector-range vec))   => (vector-ref vec 0))
      (check (range-last (vector-range vec))    => (vector-ref vec 4))
      (check (range->vector (vector-range vec)) => vec))

    (let* ((s "0123456789") (srange (string-range s)))
      (check (range-length srange) => (string-length s))
      (check (range-first srange)  => (string-ref s 0))
      (check (range-last srange)   => (string-ref s 9))
      (check (range->list srange)  => (string->list s)))
    )

  ;;;; Predicates

  (define (check-predicates)
    (print-header "Running predicate tests...")

    (check (range=? eqv? (numeric-range 0 0) (numeric-range 5 5))  => #t)
    (check (range=? eqv? (numeric-range 0 0) test-num-range)       => #f)
    (check (range=? eqv? test-num-range test-num-range)            => #t)
    (check (range=? eqv? test-num-range (numeric-range 10 30))     => #t)
    (check (range=? eqv? test-num-range (numeric-range 10 20))     => #f)
    (check (range=? eqv? test-bool-range (vector-range #(#f #t))) => #t)
    (check (range=? eqv? test-bool-range (vector-range #(#t #f))) => #f)
    (check (range=? eqv?
                    test-num-range
                    (numeric-range 10 30)
                    (subrange (numeric-range 0 50) 10 30))
           => #t)
    (check (range=? eqv?
                    test-bool-range
                    (numeric-range 10 30)
                    (subrange (numeric-range 0 50) 10 30))
           => #f)
    (check (range=? eqv?
                    test-num-range
                    (numeric-range 11 31)
                    (subrange (numeric-range 0 50) 10 30))
           => #f)
    )

  ;;;; Accessors

  (define (check-accessors)
    (print-header "Running accessor tests...")

    (check (range-ref test-num-range 0)  => 10)
    (check (range-ref test-bool-range 1) => #t)
    )

  ;;;; Iteration

  (define (check-iteration)
    (print-header "Running iteration tests...")

    ;; Check lengths of ranges returned by range-split-at.
    (let ((n 10))
      (check (let-values (((ra rb) (range-split-at test-num-range n)))
               (list (range-length ra) (range-length rb)))
             => (list n (- (range-length test-num-range) n))))

    ;; Joining the two ranges returned by range-split-at gives the
    ;; original range.
    (check (let-values (((ra rb) (range-split-at test-bool-range 1)))
             (range=/eqv? (range-append ra rb) test-bool-range))
           => #t)

    (check (range=/eqv?
            (subrange test-bool-range 0 (range-length test-bool-range))
            test-bool-range)
           => #t)
    (let ((a 5) (b 10))
      (check (= (range-length (subrange test-num-range a b)) (- b a))
             => #t)
      (check (range=/eqv? (subrange test-num-range a b)
                          (range-take (range-drop test-num-range a) (- b a)))
             => #t)
      (check (range=/eqv? (subrange test-num-range 0 b)
                          (range-take test-num-range b))
             => #t)
      (check (range=/eqv?
              (subrange test-num-range a (range-length test-num-range))
              (range-drop test-num-range a))
             => #t))

    ;; range-take r n returns a range of length n.
    (check (range-length (range-take test-num-range 10)) => 10)
    (check (range-length
            (range-take test-num-range (range-length test-num-range)))
           => (range-length test-num-range))
    (check (range->list (range-take test-num-range 5))
           => (take test-num-seq 5))

    ;; range-take-right r n returns a range of length n.
    (check (range-length (range-take-right test-num-range 10)) => 10)
    (check (range-length
            (range-take-right test-num-range (range-length test-num-range)))
           => (range-length test-num-range))
    (check (range->list (range-take-right test-num-range 5))
           => (drop test-num-seq 15))

    ;; range-drop r n returns a range of length (range-length r) - n.
    (check (range-length (range-drop test-num-range 10))
           => (- (range-length test-num-range) 10))
    (check (range-length
            (range-drop test-num-range (range-length test-num-range)))
           => 0)
    (check (range->list (range-drop test-num-range 15))
           => (drop test-num-seq 15))

    ;; range-drop-right r n returns a range of length (range-length r) - n.
    (check (range-length (range-drop-right test-num-range 10))
           => (- (range-length test-num-range) 10))
    (check (range-length
            (range-drop-right test-num-range (range-length test-num-range)))
           => 0)
    (check (range->list (range-drop-right test-num-range 15))
           => (take test-num-seq 5))

    (check (range=/eqv? (car (range-segment test-num-range 5))
                        (range-take test-num-range 5))
           => #t)
    (check (range=/eqv? (apply range-append
                               (cdr (range-segment test-num-range 5)))
                        (range-drop test-num-range 5))
           => #t)
    (check (range=/eqv? (apply range-append (range-segment test-num-range 5))
                        test-num-range)
           => #t)
    (check (fold + 0 (map range-length (range-segment test-num-range 5)))
           => (range-length test-num-range))
    (check (fold + 0 (map range-length (range-segment test-num-range 7)))
           => (range-length test-num-range))

    (check (range-count always test-num-range) => (range-length test-num-range))
    (check (range-count never test-num-range)  => 0)
    (check (range-count even? test-num-range)  => (count even? test-num-seq))
    (check (range-count (lambda (x y) y) test-num-range test-bool-range)
           => 1)
    (check (range-count (lambda (x y) (zero? (+ x y)))
                        test-num-range
                        (range-map - test-num-range))
           => (range-length test-num-range))

    (check (range-any even? test-num-range) => #t)
    (check (range-any never test-num-range) => #f)
    (check (range-any (lambda (x y) y) test-num-range test-bool-range)
           => #t)
    (check (range-any (lambda (x y) (zero? (+ x y)))
                      test-num-range
                      test-num-range)
           => #f)

    (check (range-every number? test-num-range) => #t)
    (check (range-every even? test-num-range)   => #f)
    (check (range-every (lambda (x y) y) test-num-range test-bool-range)
           => #f)
    (check (range-every (lambda (x y) (zero? (+ x y)))
                        test-num-range
                        (range-map - test-num-range))
           => #t)

    ;;; map, filter-map, & for-each

    (check (range=/eqv? (range-map (lambda (x) (+ 1 x)) test-num-range)
                        (numeric-range 11 31))
           => #t)
    (check (equal? (range->list (range-map square test-num-range))
                   (map square test-num-seq))
           => #t)
    (check (range=/eqv? (range-map + test-num-range test-num-range)
                        (numeric-range 20 60 2))
           => #t)
    ;; range-map over ranges with unequal lengths terminates when
    ;; the shortest range is exhausted.
    (check (range=/eqv?
            (range-map (lambda (x _) x) test-num-range test-bool-range)
            (range-take test-num-range (range-length test-bool-range)))
           => #t)

    ;; (range-map->list f r) = (map f (range->list r))
    (check (equal? (range-map->list not test-bool-range)
                   (map not (range->list test-bool-range)))
           => #t)
    (check (equal? (range-map->list + test-num-range test-num-range)
                   (map + test-num-seq test-num-seq))
           => #t)

    ;; (range-map->vector f r) = (map f (range->vector r))
    (check (equal? (range-map->vector not test-bool-range)
                   (vector-map not (range->vector test-bool-range)))
           => #t)
    (let ((num-vec (list->vector test-num-seq)))
      (check (equal? (range-map->vector + test-num-range test-num-range)
                     (vector-map + num-vec num-vec))
             => #t))

    (check (%range-empty? (range-filter-map never test-bool-range)) => #t)
    (check (range=/eqv? (range-filter-map values test-num-range)
                        test-num-range)
           => #t)
    (check (equal?
            (range->list (range-filter-map (lambda (x) (and (even? x) x))
                                           test-num-range))
            (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
           => #t)
    (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
      (check (range=/eqv? (range-filter-map proc test-num-range test-num-range)
                          (numeric-range 20 60 4))
             => #t))

    (check (range-filter-map->list never test-bool-range) => '())
    (check (equal? (range-filter-map->list values test-num-range)
                   test-num-seq)
           => #t)
    (check (equal?
            (range-filter-map->list (lambda (x) (and (even? x) x))
                                    test-num-range)
            (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
           => #t)
    (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
      (check (equal? (range-filter-map->list proc
                                             test-num-range
                                             test-num-range)
                     (filter-map proc test-num-seq test-num-seq))
             => #t))

    (check (let ((v #f))
             (range-for-each (lambda (x) (set! v x)) test-bool-range)
             v)
           => #t)
    (check (let ((v #f))
             (range-for-each (lambda (x y) (when y (set! v x)))
                             test-num-range
                             test-bool-range)
             v)
           => 11)

    ;;; filter & remove

    (check (range=/eqv? (range-filter always test-bool-range)
                        test-bool-range)
           => #t)
    (check (%range-empty? (range-filter never test-bool-range)) => #t)
    (check (equal? (range->list (range-filter even? test-num-range))
                   (filter even? test-num-seq))
           => #t)

    (check (range-filter->list always test-bool-range) => '(#f #t))

    (check (null? (range-filter->list never test-bool-range)) => #t)

    ;; (range-filter->list pred r) = (filter pred (range->list r))
    (check (equal? (range-filter->list even? test-num-range)
                   (filter even? test-num-seq))
           => #t)

    (check (range=/eqv? (range-remove never test-bool-range)
                        test-bool-range)
           => #t)
    (check (%range-empty? (range-remove always test-bool-range))
           => #t)
    (check (equal? (range->list (range-remove even? test-num-range))
                   (remove even? test-num-seq))
           => #t)

    (check (equal? (range-remove->list never test-bool-range)
                   (range->list test-bool-range))
           => #t)

    (check (null? (range-remove->list always test-bool-range)) => #t)

    ;; (range-remove->list pred r) = (remove pred (range->list r))
    (check (equal? (range-remove->list even? test-num-range)
                   (remove even? test-num-seq))
           => #t)

    ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
    (check (= (range-fold (lambda (b _) (+ b 1)) 0 test-num-range)
              (range-length test-num-range))
           => #t)

    ;; (range-fold proc nil r) = (fold proc nil (range->list r))
    (check (equal? (range-fold + 0 test-num-range)
                   (fold + 0 test-num-seq))
           => #t)

    (check (= (range-fold + 0 test-num-range test-num-range)
              (fold + 0 test-num-seq test-num-seq))
           => #t)

    ;; range-fold over ranges with unequal lengths terminates when
    ;; the shortest range is exhausted.
    (check (= (range-fold (lambda (s x _) (+ s x))
                          0
                          test-num-range
                          test-bool-range)
              (range-fold + 0 (range-take test-num-range
                                          (range-length test-bool-range))))
           => #t)

    ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
    (check (= (range-fold-right (lambda (b _) (+ b 1)) 0 test-num-range)
              (range-length test-num-range))
           => #t)

    ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
    (check (equal? (range-fold-right + 0 test-num-range)
                   (fold-right + 0 test-num-seq))
           => #t)

    (check (= (range-fold-right + 0 test-num-range test-num-range)
              (fold-right + 0 test-num-seq test-num-seq))
           => #t)

    ;; range-fold-right over ranges with unequal lengths terminates when
    ;; the shortest range is exhausted.
    (check (= (range-fold-right (lambda (s x _) (+ s x))
                                0
                                test-num-range
                                test-bool-range)
              (range-fold-right + 0 (range-take test-num-range
                                                (range-length
                                                 test-bool-range))))
           => #t)

    (check (eqv? (range-first (range-reverse test-bool-range))
                 (range-last test-bool-range))
           => #t)

    (check (eqv? (range-last (range-reverse test-bool-range))
                 (range-first test-bool-range))
           => #t)

    (check (equal? (range->list (range-reverse test-num-range))
                   (reverse test-num-seq))
           => #t)

    (check (%range-empty? (range-append)) => #t)
    (check (range->list (range-append test-bool-range)) => '(#f #t))
    (check (range=/eqv? (range-append (numeric-range 10 20)
                                      (numeric-range 20 30))
                        test-num-range)
           => #t)
    (check (range=/eqv? (range-append (numeric-range 10 15)
                                      (numeric-range 15 20)
                                      (numeric-range 20 25)
                                      (numeric-range 25 30))
                        test-num-range)
           => #t)
    )

  ;;;; Searching

  (define (check-searching)
    (print-header "Running search tests...")

    (check (range-index always test-num-range) => 0)
    (check (range-index never test-num-range)  => #f)
    (check (range-index values test-bool-range) => 1)
    (check (range-index (lambda (x y) (and (odd? x) y))
                        test-num-range
                        test-bool-range)
           => 1)

    (check (eqv? (range-index-right always test-num-range)
                 (- (range-length test-num-range) 1))
           => #t)
    (check (range-index-right never test-num-range)  => #f)
    (check (range-index-right values test-bool-range) => 1)
    (check (range-index-right (lambda (x y) (< (+ x y) 30))
                              test-num-range
                              test-num-range)
           => 4)

    ;; range-index and range-index-right produce the same index if pred
    ;; is only satisfied by the element at that index.
    (let ((fifteen? (lambda (n) (= n 15))))
      (check (= (range-index fifteen? test-num-range)
                (range-index-right fifteen? test-num-range)
                (list-index fifteen? test-num-seq))
             => #t))

    ;; (range-take-while always r) = r
    (check (range=/eqv? (range-take-while always test-bool-range)
                        test-bool-range)
           => #t)

    ;; (range-take-while never r) = [empty range]
    (check (%range-empty? (range-take-while never test-bool-range)) => #t)

    (let ((pred (lambda (n) (< n 15))))
      (check (range->list (range-take-while pred test-num-range))
             => (take-while pred test-num-seq)))

    ;; (range-drop-while always r) = [empty range]
    (check (%range-empty? (range-drop-while always test-bool-range)) => #t)

    ;; (range-drop-while never r) = r
    (check (range=/eqv? (range-drop-while never test-bool-range)
                        test-bool-range)
           => #t)

    (let ((pred (lambda (n) (< n 15))))
      (check (range->list (range-drop-while pred test-num-range))
             => (drop-while pred test-num-seq)))

    ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
    (let ((pred (lambda (n) (< n 10))))
      (check (range=/eqv?
              (range-append (range-take-while pred test-num-range)
                            (range-drop-while pred test-num-range))
              test-num-range)
             => #t))

    ;; (range-take-while-right always r) = r
    (check (range=/eqv? (range-take-while-right always test-bool-range)
                        test-bool-range)
           => #t)

    ;; (range-take-while-right never r) = [empty range]
    (check (%range-empty? (range-take-while-right never test-bool-range)) => #t)

    (let ((pred (lambda (n) (>= n 15))))
      (check (range->list (range-take-while-right pred test-num-range))
             => (iota 15 15)))

    ;; (range-drop-while-right always r) = [empty range]
    (check (%range-empty? (range-drop-while-right always test-bool-range)) => #t)

    ;; (range-drop-while-right never r) = r
    (check (range=/eqv? (range-drop-while-right never test-bool-range)
                        test-bool-range)
           => #t)

    (let ((pred (lambda (n) (>= n 15))))
      (check (range->list (range-drop-while-right pred test-num-range))
             => (take test-num-seq 5)))

    ;; (range-append (range-drop-while-right p r)
    ;;               (range-take-while-right p r)) = r
    (let ((pred (lambda (n) (< n 10))))
      (check (range=/eqv?
              (range-append (range-drop-while-right pred test-num-range)
                            (range-take-while-right pred test-num-range))
              test-num-range)
             => #t))
    )

  (define (check-all)
    (check-predicates)
    (check-conversion)
    (check-constructors)
    (check-accessors)
    (check-iteration)
    (check-searching)
    #;(check-report))

  (check-all)

  (test-equal? "range sequence" (for/list ([i (numeric-range 5 10)]) i) '(5 6 7 8 9))
  (test-equal? "in-range-object" (for/list ([i (in-range-object (numeric-range 5 10))]) i) '(5 6 7 8 9))

  )
