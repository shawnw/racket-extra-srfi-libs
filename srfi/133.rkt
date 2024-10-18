#lang racket/base

(require racket/contract racket/list racket/mutability racket/vector racket/unsafe/ops)
(module+ test (require "private/testwrappers.rkt"))

(define (rest-star/c . contracts)
  (define ncontracts (length contracts))
  (make-flat-contract
   #:name (apply build-compound-type-name 'rest-star/c contracts)
   #:list-contract? #t
   #:first-order
   (lambda (vals)
     (and (list? vals)
          (= (remainder (length vals) ncontracts) 0)
          (let loop ([vals vals])
            (if (null? vals)
                #t
                (let-values ([(this-set rest) (split-at vals ncontracts)])
                  (if (andmap (lambda (contract val) (contract val)) contracts this-set)
                      (loop rest)
                      #f))))))))

(provide
 ; Reexported functions from racket/base and racket/vector
 vector-copy vector-append vector-empty?

 (contract-out
  #:unprotected-submodule unsafe
  [vector-unfold (-> procedure? exact-nonnegative-integer? any/c ... vector?)]
  [vector-unfold-right (-> procedure? exact-nonnegative-integer? any/c ... vector?)]
  [vector-reverse-copy (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-concatenate (-> (listof vector?) vector?)]
  [vector= (-> (-> any/c any/c any/c) vector? ... boolean?)]
  [vector-fold (-> procedure? any/c vector? vector? ... any/c)]
  [vector-fold-right (-> procedure? any/c vector? vector? ... any/c)]
  [vector-map (-> procedure? vector? vector? ... vector?)]
  [vector-map! (-> procedure? mutable-vector? vector? ... void?)]
  [vector-for-each (-> procedure? vector? vector? ... void?)]
  [vector-count (-> procedure? vector? vector? ... exact-nonnegative-integer?)]
  [vector-cumulate (-> procedure? any/c vector? vector?)]
  [vector-index (-> procedure? vector? vector? ... (or/c exact-nonnegative-integer? #f))]
  [vector-index-right (-> procedure? vector? vector? ... (or/c exact-nonnegative-integer? #f))]
  [vector-skip (-> procedure? vector? vector? ... (or/c exact-nonnegative-integer? #f))]
  [vector-skip-right (-> procedure? vector? vector? ... (or/c exact-nonnegative-integer? #f))]
  [vector-binary-search (->* (vector? any/c (-> any/c any/c exact-integer?)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [vector-any (-> procedure? vector? vector? ... any/c)]
  [vector-every (-> procedure? vector? vector? ... any/c)]
  [vector-partition (-> (-> any/c any/c) vector? (values vector? exact-nonnegative-integer?))]
  [vector-swap! (-> mutable-vector? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [vector-fill! (->* (mutable-vector? any/c) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [vector-reverse! (->* (mutable-vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [vector-reverse-copy! (->* (mutable-vector? exact-nonnegative-integer? vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [vector-unfold! (-> procedure? mutable-vector? exact-nonnegative-integer? exact-nonnegative-integer? any/c ... void?)]
  [vector-unfold-right! (-> procedure? mutable-vector? exact-nonnegative-integer? exact-nonnegative-integer? any/c ... void?)]
  [vector->list (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) list?)]
  [reverse-vector->list (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) list?)]
  [reverse-list->vector (-> list? vector?)]
  [string->vector (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector->string (->* ((vectorof char?)) (exact-nonnegative-integer? exact-nonnegative-integer?) string?)]
  [vector-append-subvectors (->* () () #:rest (rest-star/c vector? exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  ))
 
;;;;;; SRFI 43: Vector library                           -*- Scheme -*-
;;;
;;; $Id$
;;;
;;; Taylor Campbell wrote this code; he places it in the public domain.
;;; Will Clinger [wdc] made some corrections, also in the public domain.
;;; John Cowan modified this code for SRFI 133; his changes are also in
;;; the public domain.  However, in jurisdictions where it is not possible
;;; to dedicate something to the public domain, the entire implementation
;;; is made available under the same license as SRFI 133.

;;; --------------------
;;; Exported procedure index
;;;
;;; * Constructors
;;; vector-unfold                   vector-unfold-right
;;; vector-copy                     vector-reverse-copy
;;; vector-append                   vector-concatenate
;;; vector-append-subvectors
;;;
;;; * Predicates
;;; vector-empty?
;;; vector=
;;;
;;; * Iteration
;;; vector-fold                     vector-fold-right
;;; vector-map                      vector-map!
;;; vector-for-each
;;; vector-count                    vector-cumulate
;;;
;;; * Searching
;;; vector-index                    vector-skip
;;; vector-index-right              vector-skip-right
;;; vector-binary-search
;;; vector-any                      vector-every
;;; vector-partition
;;;
;;; * Mutators
;;; vector-swap!
;;; vector-fill!
;;; vector-reverse!
;;; vector-copy!                    vector-reverse-copy!
;;; vector-reverse!
;;; vector-unfold!                  vector-unfold-right!
;;;
;;; * Conversion
;;; vector->list                    reverse-vector->list
;;; list->vector                    reverse-list->vector
;;; vector->string                  string->vector

;;; --------------------
;;; Commentary on efficiency of the code

;;; This code is somewhat tuned for efficiency.  There are several
;;; internal routines that can be optimized greatly to greatly improve
;;; the performance of much of the library.  These internal procedures
;;; are already carefully tuned for performance, and lambda-lifted by
;;; hand.  Some other routines are lambda-lifted by hand, but only the
;;; loops are lambda-lifted, and only if some routine has two possible
;;; loops -- a fast path and an n-ary case --, whereas _all_ of the
;;; internal routines' loops are lambda-lifted so as to never cons a
;;; closure in their body (VECTOR-PARSE-START+END doesn't have a loop),
;;; even in Scheme systems that perform no loop optimization (which is
;;; most of them, unfortunately).
;;;
;;; Fast paths are provided for common cases in most of the loops in
;;; this library.
;;;
;;; All calls to primitive vector operations are protected by a prior
;;; type check; they can be safely converted to use unsafe equivalents
;;; of the operations, if available.  Ideally, the compiler should be
;;; able to determine this, but the state of Scheme compilers today is
;;; not a happy one.
;;;
;;; Efficiency of the actual algorithms is a rather mundane point to
;;; mention; vector operations are rarely beyond being straightforward.



;;; --------------------
;;; Utilities

;;; SRFI 8, too trivial to put in the dependencies list.
(define-syntax receive
  (syntax-rules ()
    ((receive ?formals ?producer ?body1 ?body2 ...)
     (call-with-values (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ...)))))

(define (between? x y z)
  (and (<  x y)
       (<= y z)))

;++ This should be implemented more efficiently.  It shouldn't cons a
;++ closure, and the cons cells used in the loops when using this could
;++ be reused.
(define (vectors-ref vectors i)
  (map (lambda (v) (unsafe-vector-ref v i)) vectors))



;;; --------------------
;;; Error checking

;;; (CHECK-INDEX <vector> <index> <callee>) -> index
;;;   Ensure that INDEX is a valid index into VECTOR; if not, signal an
;;;   error stating that it is not and that this happened in a call to
;;;   CALLEE.  Return INDEX when it is valid.  (Note that this does NOT
;;;   check that VECTOR is indeed a vector.)
(define (check-index vec index callee)
  (if (or (< index 0) (>= index (vector-length vec)))
      (raise-range-error callee "vector" "" index vec 0 (- (vector-length vec) 1))
      index))

(define (check-indices vec start end name)
  (when (or (< start 0) (>= start (vector-length vec)))
    (raise-range-error name "vector" "starting " start vec 0 (- (vector-length vec) 1)))
  (when (< end start)
    (raise-range-error name "vector" "ending " end vec start (vector-length vec) 0))
  (when (> end (vector-length vec))
    (raise-range-error name "vector" "ending " end vec 0 (vector-length vec))))

(define-syntax-rule (define/check-start+end (name vec arg ... start end) body ...)
  (define (name vec arg ... [start 0] [end (vector-length vec)])
    (check-indices vec start end (quote name))
    body ...))



;;; --------------------
;;; Internal routines

;;; (%SMALLEST-LENGTH <vector-list> <default-length> <callee>)
;;;       -> exact, nonnegative integer
;;;   Compute the smallest length of VECTOR-LIST.  DEFAULT-LENGTH is
;;;   the length that is returned if VECTOR-LIST is empty.  Common use
;;;   of this is in n-ary vector routines:
;;;     (define (f vec . vectors)
;;;       (let ((vec (check-type vector? vec f)))
;;;         ...(%smallest-length vectors (vector-length vec) f)...))
;;;   %SMALLEST-LENGTH takes care of the type checking -- which is what
;;;   the CALLEE argument is for --; thus, the design is tuned for
;;;   avoiding redundant type checks.
(define %smallest-length
  (letrec ((loop (lambda (vector-list length callee)
                   (if (null? vector-list)
                       length
                       (loop (cdr vector-list)
                             (min (unsafe-vector-length (car vector-list))
                                  length)
                             callee)))))
    loop))


;;; (%VECTOR-REVERSE-COPY! <target> <tstart> <source> <sstart> <send>)
;;;   Copy elements from SSTART to SEND from SOURCE to TARGET, in the
;;;   reverse order.
(define %vector-reverse-copy!
  (letrec ((loop (lambda (target source sstart i j)
                   (cond ((>= i sstart)
                          (unsafe-vector-set! target j (unsafe-vector-ref source i))
                          (loop target source sstart
                                (- i 1)
                                (+ j 1)))))))
    (lambda (target tstart source sstart send)
      (loop target source sstart
            (- send 1)
            tstart))))

;;; (%VECTOR-REVERSE! <vector>)
(define %vector-reverse!
  (letrec ((loop (lambda (vec i j)
                   (when (<= i j)
                     (let ((v (unsafe-vector-ref vec i)))
                       (unsafe-vector-set! vec i (unsafe-vector-ref vec j))
                       (unsafe-vector-set! vec j v)
                       (loop vec (+ i 1) (- j 1)))))))
    (lambda (vec start end)
      (loop vec start (- end 1)))))

;;; (%VECTOR-FOLD1 <kons> <knil> <vector>) -> knil'
;;;     (KONS <index> <knil> <elt>) -> knil'
(define %vector-fold1
  (letrec ((loop (lambda (kons knil len vec i)
                   (if (= i len)
                       knil
                       (loop kons
                             (kons knil (unsafe-vector-ref vec i))
                             len vec (+ i 1))))))
    (lambda (kons knil len vec)
      (loop kons knil len vec 0))))

;;; (%VECTOR-FOLD2+ <kons> <knil> <vector> ...) -> knil'
;;;     (KONS <index> <knil> <elt> ...) -> knil'
(define %vector-fold2+
  (letrec ((loop (lambda (kons knil len vectors i)
                   (if (= i len)
                       knil
                       (loop kons
                             (apply kons knil
                                    (vectors-ref vectors i))
                             len vectors (+ i 1))))))
    (lambda (kons knil len vectors)
      (loop kons knil len vectors 0))))

;;; (%VECTOR-MAP! <f> <target> <length> <vector>) -> target
;;;     (F <index> <elt>) -> elt'
(define %vector-map1!
  (letrec ((loop (lambda (f target vec i)
                   (if (zero? i)
                       target
                       (let ((j (- i 1)))
                         (unsafe-vector-set! target j
                                      (f (unsafe-vector-ref vec j)))
                         (loop f target vec j))))))
    (lambda (f target vec len)
      (loop f target vec len))))

;;; (%VECTOR-MAP2+! <f> <target> <vectors> <len>) -> target
;;;     (F <index> <elt> ...) -> elt'
(define %vector-map2+!
  (letrec ((loop (lambda (f target vectors i)
                   (if (zero? i)
                       target
                       (let ((j (- i 1)))
                         (unsafe-vector-set! target j
                           (apply f (vectors-ref vectors j)))
                         (loop f target vectors j))))))
    (lambda (f target vectors len)
      (loop f target vectors len))))

;;;;;;;;;;;;;;;;;;;;;;;; ***** vector-lib ***** ;;;;;;;;;;;;;;;;;;;;;;;

;;; --------------------
;;; Constructors

;;; (VECTOR-UNFOLD <f> <length> <initial-seed> ...) -> vector
;;;     (F <index> <seed> ...) -> [elt seed' ...]
;;;   The fundamental vector constructor.  Creates a vector whose
;;;   length is LENGTH and iterates across each index K between 0 and
;;;   LENGTH, applying F at each iteration to the current index and the
;;;   current seeds to receive N+1 values: first, the element to put in
;;;   the Kth slot and then N new seeds for the next iteration.
(define (vector-unfold f length . initial-seeds)
  (define vec (make-vector length))
  (%vector-unfold! f vec 0 length initial-seeds)
  vec)

(define (vector-unfold! f vec start end . initial-seeds)
  (check-indices vec start end 'vector-unfold!)
  (%vector-unfold! f vec start end initial-seeds))

;;; (VECTOR-UNFOLD! <vec> <start> <end> <f> <initial-seed> ...) -> vector
;;;     (F <index> <seed> ...) -> [elt seed' ...]
;;;   Like VECTOR-UNFOLD, but unfolds onto an existing vector starting
;;;   at <start> up to but not including <end>.
(define %vector-unfold!
  (letrec ((tabulate!                   ; Special zero-seed case.
            (lambda (f vec i len)
              (cond ((< i len)
                     (unsafe-vector-set! vec i (f i))
                     (tabulate! f vec (+ i 1) len)))))
           (unfold1!                    ; Fast path for one seed.
            (lambda (f vec i len seed)
              (when (< i len)
                  (receive (elt new-seed)
                           (f i seed)
                    (unsafe-vector-set! vec i elt)
                    (unfold1! f vec (+ i 1) len new-seed)))))
           (unfold2+!                   ; Slower variant for N seeds.
            (lambda (f vec i len seeds)
              (when (< i len)
                  (receive (elt . new-seeds)
                           (apply f i seeds)
                    (unsafe-vector-set! vec i elt)
                    (unfold2+! f vec (+ i 1) len new-seeds))))))
    (lambda (f vec start end initial-seeds)
      (cond ((null? initial-seeds)
             (tabulate! f vec start end))
            ((null? (cdr initial-seeds))
             (unfold1! f vec start end (car initial-seeds)))
            (else
             (unfold2+! f vec start end initial-seeds))))))

;;; (VECTOR-UNFOLD-RIGHT <f> <length> <initial-seed> ...) -> vector
;;;     (F <seed> ...) -> [seed' ...]
;;;   Like VECTOR-UNFOLD, but it generates elements from LENGTH to 0
;;;   (still exclusive with  LENGTH and inclusive with 0), not 0 to
;;;   LENGTH as with VECTOR-UNFOLD.
(define (vector-unfold-right f len . initial-seeds)
  (define vec (make-vector len))
  (%vector-unfold-right! f vec 0 len initial-seeds)
  vec)

(define (vector-unfold-right! f vec start end . initial-seeds)
  (check-indices vec start end 'vector-unfold-right!)
  (%vector-unfold-right! f vec start end initial-seeds))

;;; (VECTOR-UNFOLD-RIGHT! <vec> <start> <end> <f> <initial-seed> ...) -> vector
;;;   Like VECTOR-UNFOLD-RIGHT, but unfolds onto an existing vector.
(define (%vector-unfold-right! f vec start end initial-seeds)
  (letrec ((tabulate!
            (lambda (f vec i)
              (cond ((>= i start)
                     (unsafe-vector-set! vec i (f i))
                     (tabulate! f vec (- i 1))))))
           (unfold1!
            (lambda (f vec i seed)
              (when (>= i start)
                  (receive (elt new-seed)
                           (f i seed)
                    (unsafe-vector-set! vec i elt)
                    (unfold1! f vec (- i 1) new-seed)))))
           (unfold2+!
            (lambda (f vec i seeds)
              (when (>= i start)
                  (receive (elt . new-seeds)
                           (apply f i seeds)
                    (unsafe-vector-set! vec i elt)
                    (unfold2+! f vec (- i 1) new-seeds))))))
    (let ((i (- end 1)))
      (cond ((null? initial-seeds)
             (tabulate! f vec i))
            ((null? (cdr initial-seeds))
             (unfold1!  f vec i (car initial-seeds)))
            (else
             (unfold2+! f vec i initial-seeds))))))


;;; (VECTOR-REVERSE-COPY <vector> [<start> <end>]) -> vector
;;;   Create a newly allocated vector whose elements are the reversed
;;;   sequence of elements between START and END in VECTOR.  START's
;;;   default is 0; END's default is the length of VECTOR.
(define/check-start+end (vector-reverse-copy vec start end)
  (let ((new (make-vector (- end start))))
    (%vector-reverse-copy! new 0 vec start end)
    new))

;;; (VECTOR-CONCATENATE <vector-list>) -> vector
;;;   Concatenate the vectors in VECTOR-LIST.  This is equivalent to
;;;     (apply vector-append VECTOR-LIST)
;;;   but VECTOR-APPEND tends to be implemented in terms of
;;;   VECTOR-CONCATENATE, and some Schemes bork when the list to apply
;;;   a function to is too long.
;;;
;;; Actually, they're both implemented in terms of an internal routine.
(define (vector-concatenate vector-list)
  (vector-concatenate:aux vector-list vector-concatenate))

;;; Auxiliary for VECTOR-APPEND and VECTOR-CONCATENATE
(define vector-concatenate:aux
  (letrec ((compute-length
            (lambda (vectors len callee)
              (if (null? vectors)
                  len
                  (compute-length (cdr vectors)
                                  (+ (unsafe-vector-length (car vectors)) len)
                                  callee))))
           (concatenate!
            (lambda (vectors target to)
              (if (null? vectors)
                  target
                  (let* ((vec1 (car vectors))
                         (len (unsafe-vector-length vec1)))
                    (vector-copy! target to vec1 0 len)
                    (concatenate! (cdr vectors) target
                                  (+ to len)))))))
    (lambda (vectors callee)
      (cond ((null? vectors)            ;+++
             (make-vector 0))
            ((null? (cdr vectors))      ;+++
             ;; Blech, we still have to allocate a new one.
             (let* ([vec (car vectors)]
                    (len (vector-length vec))
                    (new (make-vector len)))
               (vector-copy! new 0 vec 0 len)
               new))
            (else
             (let ((new-vector
                    (make-vector (compute-length vectors 0 callee))))
               (concatenate! vectors new-vector 0)
               new-vector))))))

;;; (VECTOR-APPEND-SUBVECTORS <arg> ...) -> vector
;;;   Like VECTOR-APPEND but appends subvectors specified by
;;;   <vector> <start> <end> argument triples.
(define (vector-append-subvectors . args)
  ;; GATHER-ARGS returns three values: vectors, starts, ends
  (define (gather-args args)
    (let loop ((args args) (vecs '()) (starts '()) (ends '()))
      (if (null? args)
         (values (reverse vecs) (reverse starts) (reverse ends))
         (loop (cdddr args)
               (cons (car args) vecs)
               (cons (cadr args) starts)
               (cons (caddr args) ends)))))
  ;; TOTAL-LENGTH computes the length of all subvectors
  (define (total-length starts ends)
    (let loop ((count 0) (starts starts) (ends ends))
      (if (null? starts)
        count
        (let ((start (car starts)) (end (car ends)))
          (loop (+ count (- end start))
                (cdr starts)
                (cdr ends))))))
 ;; COPY-EACH! copies each subvector into a result vector
  (define (copy-each! result vecs starts ends)
    (let loop ((at 0) (vecs vecs) (starts starts) (ends ends))
      (if (null? vecs)
        result
        (let ((vec (car vecs)) (start (car starts)) (end (car ends)))
          (vector-copy! result at vec start end)
          (loop (+ at (- end start))
                (cdr vecs)
                (cdr starts)
                (cdr ends))))))
  ;; put them all together, they spell VECTOR-APPEND-SUBVECTORS
  (receive (vecs starts ends) (gather-args args)
    (define result (make-vector (total-length starts ends)))
    (copy-each! result vecs starts ends)))


;;; --------------------
;;; Predicates

;;; (VECTOR= <elt=?> <vector> ...) -> boolean
;;;     (ELT=? <value> <value>) -> boolean
;;;   Determine vector equality generalized across element comparators.
;;;   Vectors A and B are equal iff their lengths are the same and for
;;;   each respective elements E_a and E_b (elt=? E_a E_b) returns a
;;;   true value.  ELT=? is always applied to two arguments.
;;;
;;;   If the number of vector arguments is zero or one, then #T is
;;;   automatically returned.  If there are N vector arguments,
;;;   VECTOR_1 VECTOR_2 ... VECTOR_N, then VECTOR_1 & VECTOR_2 are
;;;   compared; if they are equal, the vectors VECTOR_2 ... VECTOR_N
;;;   are compared.  The precise order in which ELT=? is applied is not
;;;   specified.
(define (vector= elt=? . vectors)
  (cond ((null? vectors)
         #t)
        ((null? (cdr vectors))
         #t)
        (else
         (let loop ((vecs vectors))
           (let ((vec1 (car vecs))
                 (vec2+ (cdr vecs)))
             (or (null? vec2+)
                 (and (binary-vector= elt=? vec1 (car vec2+))
                      (loop vec2+))))))))

(define (binary-vector= elt=? vector-a vector-b)
  (let ((length-a (vector-length vector-a))
        (length-b (vector-length vector-b)))
    (and (= length-a length-b)
         (let loop ((i 0))
           (cond
             ((= i length-a) #t)
             ((elt=?
                (unsafe-vector-ref vector-a i)
                (unsafe-vector-ref vector-b i))
              (loop (+ i 1)))
             (else #f))))))



;;; --------------------
;;; Selectors



;;; --------------------
;;; Iteration

;;; (VECTOR-FOLD <kons> <initial-knil> <vector> ...) -> knil
;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors -> N+1 args
;;;   The fundamental vector iterator.  KONS is iterated over each
;;;   index in all of the vectors in parallel, stopping at the end of
;;;   the shortest; KONS is applied to an argument list of (list I
;;;   STATE (vector-ref VEC I) ...), where STATE is the current state
;;;   value -- the state value begins with KNIL and becomes whatever
;;;   KONS returned at the respective iteration --, and I is the
;;;   current index in the iteration.  The iteration is strictly left-
;;;   to-right.
;;;     (vector-fold KONS KNIL (vector E_1 E_2 ... E_N))
;;;       <=>
;;;     (KONS (... (KONS (KONS KNIL E_1) E_2) ... E_N-1) E_N)
(define (vector-fold kons knil vec . vectors)
  (if (null? vectors)
      (%vector-fold1 kons knil (unsafe-vector-length vec) vec)
      (%vector-fold2+ kons knil
                      (%smallest-length vectors
                                        (unsafe-vector-length vec)
                                        vector-fold)
                      (cons vec vectors))))

;;; (VECTOR-FOLD-RIGHT <kons> <initial-knil> <vector> ...) -> knil
;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors => N+1 args
;;;   The fundamental vector recursor.  Iterates in parallel across
;;;   VECTOR ... right to left, applying KONS to the elements and the
;;;   current state value; the state value becomes what KONS returns
;;;   at each next iteration.  KNIL is the initial state value.
;;;     (vector-fold-right KONS KNIL (vector E_1 E_2 ... E_N))
;;;       <=>
;;;     (KONS (... (KONS (KONS KNIL E_N) E_N-1) ... E_2) E_1)
;;;
;;; Not implemented in terms of a more primitive operations that might
;;; called %VECTOR-FOLD-RIGHT due to the fact that it wouldn't be very
;;; useful elsewhere.
(define vector-fold-right
  (letrec ((loop1 (lambda (kons knil vec i)
                    (if (negative? i)
                        knil
                        (loop1 kons (kons knil (unsafe-vector-ref vec i))
                               vec
                               (- i 1)))))
           (loop2+ (lambda (kons knil vectors i)
                     (if (negative? i)
                         knil
                         (loop2+ kons
                                 (apply kons knil
                                        (vectors-ref vectors i))
                                 vectors
                                 (- i 1))))))
    (lambda (kons knil vec . vectors)
      (if (null? vectors)
          (loop1  kons knil vec (- (vector-length vec) 1))
          (loop2+ kons knil (cons vec vectors)
                    (- (%smallest-length vectors
                                         (unsafe-vector-length vec)
                                         'vector-fold-right)
                       1))))))

;;; (VECTOR-MAP <f> <vector> ...) -> vector
;;;     (F <elt> ...) -> value ; N vectors -> N args
;;;   Constructs a new vector of the shortest length of the vector
;;;   arguments.  Each element at index I of the new vector is mapped
;;;   from the old vectors by (F I (vector-ref VECTOR I) ...).  The
;;;   dynamic order of application of F is unspecified.
(define (vector-map f vec . vectors)
  (if (null? vectors)
      (let ((len (vector-length vec)))
        (%vector-map1! f (make-vector len) vec len))
      (let ((len (%smallest-length vectors
                                   (vector-length vec)
                                   'vector-map)))
        (%vector-map2+! f (make-vector len) (cons vec vectors)
                        len))))

;;; (VECTOR-MAP! <f> <vector> ...) -> unspecified
;;;     (F <elt> ...) -> element' ; N vectors -> N args
;;;   Similar to VECTOR-MAP, but rather than mapping the new elements
;;;   into a new vector, the new mapped elements are destructively
;;;   inserted into the first vector.  Again, the dynamic order of
;;;   application of F is unspecified, so it is dangerous for F to
;;;   manipulate the first VECTOR.
(define (vector-map! f vec . vectors)
  (if (null? vectors)
      (%vector-map1!  f vec vec (vector-length vec))
      (%vector-map2+! f vec (cons vec vectors)
                      (%smallest-length vectors
                                        (vector-length vec)
                                        'vector-map!)))
    (void))

;;; (VECTOR-FOR-EACH <f> <vector> ...) -> unspecified
;;;     (F <elt> ...) ; N vectors -> N args
;;;   Simple vector iterator: applies F to each index in the range [0,
;;;   LENGTH), where LENGTH is the length of the smallest vector
;;;   argument passed, and the respective element at that index.  In
;;;   contrast with VECTOR-MAP, F is reliably applied to each
;;;   subsequent elements, starting at index 0 from left to right, in
;;;   the vectors.
(define vector-for-each
  (letrec ((for-each1
            (lambda (f vec i len)
              (cond ((< i len)
                     (f (unsafe-vector-ref vec i))
                     (for-each1 f vec (+ i 1) len)))))
           (for-each2+
            (lambda (f vecs i len)
              (cond ((< i len)
                     (apply f (vectors-ref vecs i))
                     (for-each2+ f vecs (+ i 1) len))))))
    (lambda (f vec . vectors)
      (if (null? vectors)
          (for-each1 f vec 0 (unsafe-vector-length vec))
          (for-each2+ f (cons vec vectors) 0
                      (%smallest-length vectors
                                        (unsafe-vector-length vec)
                                        vector-for-each))))))

;;; (VECTOR-COUNT <predicate?> <vector> ...)
;;;       -> exact, nonnegative integer
;;;     (PREDICATE? <value> ...) ; N vectors -> N args
;;;   PREDICATE? is applied element-wise to the elements of VECTOR ...,
;;;   and a count is tallied of the number of elements for which a
;;;   true value is produced by PREDICATE?.  This count is returned.
(define (vector-count pred? vec . vectors)
  (if (null? vectors)
      (%vector-fold1 (lambda (count elt)
                       (if (pred? elt)
                           (+ count 1)
                           count))
                     0
                     (vector-length vec)
                     vec)
      (%vector-fold2+ (lambda (count . elts)
                        (if (apply pred? elts)
                            (+ count 1)
                            count))
                      0
                      (%smallest-length vectors
                                        (vector-length vec)
                                        'vector-count)
                      (cons vec vectors))))

;;; (VECTOR-CUMULATE <f> <knil> <vector>)
;;;       -> vector
;;;   Returns a <new>ly allocated vector <new> with the same length as
;;;   <vec>. Each element <i> of <new> is set to the result of invoking <f> on
;;;   <new>[i-1] and <vec>[i], except that for the first call on <f>, the first
;;;   argument is <knil>. The <new> vector is returned.
(define (vector-cumulate f knil vec)
  (let* ((len (vector-length vec))
         (result (make-vector len)))
    (let loop ((i 0) (left knil))
      (if (= i len)
        result
        (let* ((right (unsafe-vector-ref vec i)) (r (f left right)))
          (unsafe-vector-set! result i r)
          (loop (+ i 1) r))))))



;;; --------------------
;;; Searching

;;; (VECTOR-INDEX <predicate?> <vector> ...)
;;;       -> exact, nonnegative integer or #F
;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
;;;   Search left-to-right across VECTOR ... in parallel, returning the
;;;   index of the first set of values VALUE ... such that (PREDICATE?
;;;   VALUE ...) returns a true value; if no such set of elements is
;;;   reached, return #F.
(define (vector-index pred? vec . vectors)
  (vector-index/skip pred? vec vectors 'vector-index))

;;; (VECTOR-SKIP <predicate?> <vector> ...)
;;;       -> exact, nonnegative integer or #F
;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
;;;   (vector-index (lambda elts (not (apply PREDICATE? elts)))
;;;                 VECTOR ...)
;;;   Like VECTOR-INDEX, but find the index of the first set of values
;;;   that do _not_ satisfy PREDICATE?.
(define (vector-skip pred? vec . vectors)
  (vector-index/skip (lambda elts (not (apply pred? elts)))
                     vec vectors
                     'vector-skip))

;;; Auxiliary for VECTOR-INDEX & VECTOR-SKIP
(define vector-index/skip
  (letrec ((loop1  (lambda (pred? vec len i)
                     (cond ((= i len) #f)
                           ((pred? (unsafe-vector-ref vec i)) i)
                           (else (loop1 pred? vec len (+ i 1))))))
           (loop2+ (lambda (pred? vectors len i)
                     (cond ((= i len) #f)
                           ((apply pred? (vectors-ref vectors i)) i)
                           (else (loop2+ pred? vectors len
                                         (+ i 1)))))))
    (lambda (pred? vec vectors callee)
      (if (null? vectors)
          (loop1 pred? vec (vector-length vec) 0)
          (loop2+ pred? (cons vec vectors)
                  (%smallest-length vectors
                                    (vector-length vec)
                                    callee)
                  0)))))

;;; (VECTOR-INDEX-RIGHT <predicate?> <vector> ...)
;;;       -> exact, nonnegative integer or #F
;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
;;;   Right-to-left variant of VECTOR-INDEX.
(define (vector-index-right pred? vec . vectors)
  (vector-index/skip-right pred? vec vectors 'vector-index-right))

;;; (VECTOR-SKIP-RIGHT <predicate?> <vector> ...)
;;;       -> exact, nonnegative integer or #F
;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
;;;   Right-to-left variant of VECTOR-SKIP.
(define (vector-skip-right pred? vec . vectors)
  (vector-index/skip-right (lambda elts (not (apply pred? elts)))
                           vec vectors
                           'vector-index-right))

(define vector-index/skip-right
  (letrec ((loop1  (lambda (pred? vec i)
                     (cond ((negative? i) #f)
                           ((pred? (unsafe-vector-ref vec i)) i)
                           (else (loop1 pred? vec (- i 1))))))
           (loop2+ (lambda (pred? vectors i)
                     (cond ((negative? i) #f)
                           ((apply pred? (vectors-ref vectors i)) i)
                           (else (loop2+ pred? vectors (- i 1)))))))
    (lambda (pred? vec vectors callee)
      (if (null? vectors)
          (loop1 pred? vec (- (vector-length vec) 1))
          (loop2+ pred? (cons vec vectors)
                  (- (%smallest-length vectors
                                       (vector-length vec)
                                       callee)
                     1))))))

;;; (VECTOR-BINARY-SEARCH <vector> <value> <cmp> [<start> <end>])
;;;       -> exact, nonnegative integer or #F
;;;     (CMP <value1> <value2>) -> integer
;;;       positive -> VALUE1 > VALUE2
;;;       zero     -> VALUE1 = VALUE2
;;;       negative -> VALUE1 < VALUE2
;;;   Perform a binary search through VECTOR for VALUE, comparing each
;;;   element to VALUE with CMP.
(define/check-start+end (vector-binary-search vec value cmp start end)
  (let loop ((start start) (end end) (j #f))
    (let ((i (quotient (+ start end) 2)))
      (if (or (= start end) (and j (= i j)))
          #f
          (let ([comparison (cmp (unsafe-vector-ref vec i) value)])
            (cond ((zero?     comparison) i)
                  ((positive? comparison) (loop start i i))
                  (else                   (loop i end i))))))))

;;; (VECTOR-ANY <pred?> <vector> ...) -> value
;;;   Apply PRED? to each parallel element in each VECTOR ...; if PRED?
;;;   should ever return a true value, immediately stop and return that
;;;   value; otherwise, when the shortest vector runs out, return #F.
;;;   The iteration and order of application of PRED? across elements
;;;   is of the vectors is strictly left-to-right.
(define vector-any
  (letrec ((loop1 (lambda (pred? vec i len len-1)
                    (and (not (= i len))
                         (if (= i len-1)
                             (pred? (unsafe-vector-ref vec i))
                             (or (pred? (unsafe-vector-ref vec i))
                                 (loop1 pred? vec (+ i 1)
                                        len len-1))))))
           (loop2+ (lambda (pred? vectors i len len-1)
                     (and (not (= i len))
                          (if (= i len-1)
                              (apply pred? (vectors-ref vectors i))
                              (or (apply pred? (vectors-ref vectors i))
                                  (loop2+ pred? vectors (+ i 1)
                                         len len-1)))))))
    (lambda (pred? vec . vectors)
      (if (null? vectors)
          (let ((len (vector-length vec)))
            (loop1 pred? vec 0 len (- len 1)))
          (let ((len (%smallest-length vectors
                                       (vector-length vec)
                                       'vector-any)))
            (loop2+ pred? (cons vec vectors) 0 len (- len 1)))))))

;;; (VECTOR-EVERY <pred?> <vector> ...) -> value
;;;   Apply PRED? to each parallel value in each VECTOR ...; if PRED?
;;;   should ever return #F, immediately stop and return #F; otherwise,
;;;   if PRED? should return a true value for each element, stopping at
;;;   the end of the shortest vector, return the last value that PRED?
;;;   returned.  In the case that there is an empty vector, return #T.
;;;   The iteration and order of application of PRED? across elements
;;;   is of the vectors is strictly left-to-right.
(define vector-every
  (letrec ((loop1 (lambda (pred? vec i len len-1)
                    (or (= i len)
                        (if (= i len-1)
                            (pred? (unsafe-vector-ref vec i))
                            (and (pred? (unsafe-vector-ref vec i))
                                 (loop1 pred? vec (+ i 1)
                                        len len-1))))))
           (loop2+ (lambda (pred? vectors i len len-1)
                     (or (= i len)
                         (if (= i len-1)
                             (apply pred? (vectors-ref vectors i))
                             (and (apply pred? (vectors-ref vectors i))
                                  (loop2+ pred? vectors (+ i 1)
                                          len len-1)))))))
    (lambda (pred? vec . vectors)
      (if (null? vectors)
          (let ((len (vector-length vec)))
            (loop1 pred? vec 0 len (- len 1)))
          (let ((len (%smallest-length vectors
                                       (vector-length vec)
                                       'vector-every)))
            (loop2+ pred? (cons vec vectors) 0 len (- len 1)))))))

;;; (VECTOR-PARTITION <pred?> <vector>) -> vector
;;;   A vector the same size as <vec> is newly allocated and filled with
;;;   all the elements of <vec> that satisfy <pred?> in their original
;;;   order followed by all the elements that do not satisfy <pred?>,
;;;   also in their original order.

;;;   Two values are returned, the newly allocated vector and the index
;;;   of the leftmost element that does not satisfy <pred?>.
(define (vector-partition pred? vec)
  (let* ((len (vector-length vec))
         (cnt (vector-count pred? vec))
         (result (make-vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (if (= i len)
        (values result cnt)
        (let ((elem (unsafe-vector-ref vec i)))
          (if (pred? elem)
            (begin
              (unsafe-vector-set! result yes elem)
              (loop (+ i 1) (+ yes 1) no))
            (begin
              (unsafe-vector-set! result no elem)
              (loop (+ i 1) yes (+ no 1)))))))))



;;; --------------------
;;; Mutators

;;; (VECTOR-SWAP! <vector> <index1> <index2>) -> unspecified
;;;   Swap the values in the locations at INDEX1 and INDEX2.
(define (vector-swap! vec i j)
  (let ((i (check-index vec i 'vector-swap!))
        (j (check-index vec j 'vector-swap!)))
    (let ((x (unsafe-vector-ref vec i)))
      (vector-set! vec i (unsafe-vector-ref vec j))
      (vector-set! vec j x))))

;;; (VECTOR-FILL! <vector> <value> [<start> <end>]) -> unspecified
;;;   [R5RS+] Fill the locations in VECTOR between START, whose default
;;;   is 0, and END, whose default is the length of VECTOR, with VALUE.
;;;
;;; This one can probably be made really fast natively.
(define/check-start+end (vector-fill! vec value start end)
  (do ((i start (+ i 1)))
    ((= i end))
    (unsafe-vector-set! vec i value)))

;;; (VECTOR-REVERSE-COPY! <target> <tstart> <source> [<sstart> <send>])
;;; [wdc] Corrected to allow 0 <= sstart <= send <= (vector-length source).
(define (vector-reverse-copy! target tstart source [sstart 0] [send (vector-length source)])
  (check-index target tstart 'vector-reverse-copy!)
  (check-indices source sstart send 'vector-reverse-copy!)
  (if (and (eq? target source)
           (or (between? sstart tstart send)
               (between? tstart sstart
                         (+ tstart (- send sstart)))))
      (raise-arguments-error 'vector-reverse-copy! "vector range for self-copying overlaps"
                             "tstart" tstart
                             "sstart" sstart
                             "send" send)
      (%vector-reverse-copy! target tstart source sstart send)))

;;; (VECTOR-REVERSE! <vector> [<start> <end>]) -> unspecified
;;;   Destructively reverse the contents of the sequence of locations
;;;   in VECTOR between START, whose default is 0, and END, whose
;;;   default is the length of VECTOR.
(define/check-start+end (vector-reverse! vec start end)
    (%vector-reverse! vec start end))



;;; --------------------
;;; Conversion

;;; (VECTOR->LIST <vector> [<start> <end>]) -> list
;;;   [R5RS+] Produce a list containing the elements in the locations
;;;   between START, whose default is 0, and END, whose default is the
;;;   length of VECTOR, from VECTOR.
(define/check-start+end (vector->list vec start end)
  (for/list ([elem (in-vector vec start end)]) elem))

;;; (REVERSE-VECTOR->LIST <vector> [<start> <end>]) -> list
;;;   Produce a list containing the elements in the locations between
;;;   START, whose default is 0, and END, whose default is the length
;;;   of VECTOR, from VECTOR, in reverse order.
(define/check-start+end (reverse-vector->list vec start end)
  (do ((i start (+ i 1))
       (result '() (cons (unsafe-vector-ref vec i) result)))
    ((= i end) result)))

(define (reverse-list->vector lst)
  (let ([vec (make-vector (length lst))])
    (for ([elem (in-list lst)]
          [i (in-inclusive-range (- (vector-length vec) 1) 0 -1)])
      (unsafe-vector-set! vec i elem))
    vec))

;;; (VECTOR->STRING <vector> [<start> <end>]) -> string
;;;   Produce a string containing the elements in the locations
;;;   between START, whose default is 0, and END, whose default is the
;;;   length of VECTOR, from VECTOR.
(define/check-start+end (vector->string vec start end)
  (build-string (- end start) (lambda (i) (unsafe-vector-ref vec (+ i start)))))

;;; (STRING->VECTOR <string> [<start> <end>]) -> vector
;;;   Produce a vector containing the elements in STRING
;;;   between START, whose default is 0, & END,
;;;   whose default is the length of STRING, from STRING.
(define (string->vector str [start 0] [end (string-length str)])
  (cond
    ((< end start)
     (raise-range-error 'string->vector "string" "ending " end str start (unsafe-string-length str) 0))
    ((> start (unsafe-string-length str))
     (raise-range-error 'string->vector "string" "starting " start str 0 (unsafe-string-length str)))
    ((> end (unsafe-string-length str))
     (raise-range-error 'string->vector "string" "ending " end str 0 (unsafe-string-length str) start))
    (else
     (build-vector (- end start) (lambda (i) (unsafe-string-ref str (+ i start)))))))

(module+ test
  
(test-group "vectors"
  (test-group "vectors/basics"
    (define v (make-vector 3 3))
    (test-assert (vector? #(1 2 3)))
    (test-assert (vector? (make-vector 10)))
    (test 3 (vector-ref v 0))
    (test 3 (vector-ref v 1))
    (test 3 (vector-ref v 2))
    ;(test-error (vector-ref v -1))
    ;(test-error (vector-ref v 3))
    (vector-set! v 0 -32)
    (test -32 (vector-ref v 0))
    (test 3 (vector-length v))
    (test 0 (vector-length '#()))
  ) ; end vectors/basics

  (test-group "vectors/constructors"
    (define a2i '#(a b c d e f g h i))
    (test '#(0 1 2 3 4) (vector 0 1 2 3 4))
    (test '#(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
          (vector-unfold (lambda (i x) (values x (- x 1))) 10 0))
    (test '#(0 1 2 3 4 5 6) (vector-unfold values 7))
    (test '#((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
          (vector-unfold-right (lambda (i x) (values (cons i x) (+ x 1))) 5 0))
    (test a2i (vector-copy a2i))
    (test-assert (not (eqv? a2i (vector-copy a2i))))
    (test '#(g h i) (vector-copy a2i 6))
    (test '#(d e f) (vector-copy a2i 3 6))
    (test '#(1 2 3 4) (vector-reverse-copy '#(5 4 3 2 1 0) 1 5))
    (test '#(x y) (vector-append '#(x) '#(y)))
    (test '#(a b c d) (vector-append '#(a) '#(b c d)))
    (test '#(a #(b) #(c)) (vector-append '#(a #(b)) '#(#(c))))
    (test '#(a b c d) (vector-concatenate '(#(a b) #(c d))))
    (test '#(a b h i) (vector-append-subvectors '#(a b c d e) 0 2 '#(f g h i j) 2 4))
  ) ; end vectors/constructors

  (test-group "vectors/predicates"
    (test #f (vector-empty? '#(a)))
    (test #f (vector-empty? '#(())))
    (test #f (vector-empty? '#(#())))
    (test-assert (vector-empty? '#()))
    (test-assert (vector= eq? '#(a b c d) '#(a b c d)))
    (test #f (vector= eq? '#(a b c d) '#(a b d c)))
    (test #f (vector= = '#(1 2 3 4 5) '#(1 2 3 4)))
    (test #f (vector= = '#(+nan.0) '#(+nan.0)))
    (test #f (let ((nan '+nan.0)) (vector= = (vector nan) (vector nan))))
    (test #f (let ((nanvec '#(+nan.0))) (vector= = nanvec nanvec)))
    (test-assert (vector= eq?))
    (test-assert (vector= eq? '#(a)))
    (test #f (vector= eq? (vector (vector 'a)) (vector (vector 'a))))
    (test-assert (vector= equal? (vector (vector 'a)) (vector (vector 'a))))
  ) ; end vectors/predicates

  (test-group "vectors/iteration"
    (define vos '#("abc" "abcde" "abcd"))
    (define vec '#(0 1 2 3 4 5))
    (define vec2 (vector 0 1 2 3 4))
    (define vec3 (vector 1 2 3 4 5))
    (define result '())
    (define (sqr x) (* x x))
    (test 5 (vector-fold (lambda (len str) (max (string-length str) len))
                         0 vos))
    (test '(5 4 3 2 1 0)
          (vector-fold (lambda (tail elt) (cons elt tail)) '() vec))
    (test 3 (vector-fold (lambda (ctr n) (if (even? n) (+ ctr 1) ctr)) 0 vec))
    (test '(a b c d) (vector-fold-right (lambda (tail elt) (cons elt tail))
                     '() '#(a b c d)))
    (test '#(1 4 9 16) (vector-map sqr '#(1 2 3 4)))
    (test '#(5 8 9 8 5) (vector-map * '#(1 2 3 4 5) '#(5 4 3 2 1)))
    (vector-map! sqr vec2)
    (test '#(0 1 4 9 16) (vector-copy vec2))
    (vector-map! * vec2 vec3)
    (test '#(0 2 12 36 80) (vector-copy vec2))
    (vector-for-each (lambda (x) (set! result (cons x result))) vec)
    (test '(5 4 3 2 1 0) (cons (car result) (cdr result)))
    (test 3 (vector-count even? '#(3 1 4 1 5 9 2 5 6)))
    (test 2 (vector-count < '#(1 3 6 9) '#(2 4 6 8 10 12)))
    (test '#(3 4 8 9 14 23 25 30 36) (vector-cumulate + 0 '#(3 1 4 1 5 9 2 5 6)))
  ) ; end vectors/iteration

  (test-group "vectors/searching"
    (define (cmp a b)
      (cond
         ((< a b) -1)
         ((= a b) 0)
         (else 1)))
    (define v '#(0 2 4 6 8 10 12))
    (test 2 (vector-index even? '#(3 1 4 1 5 9 6)))
    (test 1 (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2)))
    (test #f (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2)))
    (test 5 (vector-index-right odd? '#(3 1 4 1 5 9 6)))
    (test 3 (vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2)))
    (test 2 (vector-skip number? '#(1 2 a b 3 4 c d)))
    (test 2 (vector-skip = '#(1 2 3 4 5) '#(1 2 -3 4)))
    (test 7 (vector-skip-right number? '#(1 2 a b 3 4 c d)))
    (test 3 (vector-skip-right = '#(1 2 3 4 5) '#(1 2 -3 -4 5)))
    (test 0 (vector-binary-search v 0 cmp))
    (test 3 (vector-binary-search v 6 cmp))
    (test #f (vector-binary-search v 1 cmp))
    (test-assert (vector-any number? '#(1 2 x y z)))
    (test-assert (vector-any < '#(1 2 3 4 5) '#(2 1 3 4 5)))
    (test #f (vector-any number? '#(a b c d e)))
    (test #f (vector-any > '#(1 2 3 4 5) '#(1 2 3 4 5)))
    (test #f (vector-every number? '#(1 2 x y z)))
    (test-assert (vector-every number? '#(1 2 3 4 5)))
    (test #f (vector-every < '#(1 2 3) '#(2 3 3)))
    (test-assert (vector-every < '#(1 2 3) '#(2 3 4)))
    (test 'yes (vector-any (lambda (x) (if (number? x) 'yes #f)) '#(1 2 x y z)))
    (let-values (((new off) (vector-partition number? '#(1 x 2 y 3 z))))
      (test '#(1 2 3 x y z) (vector-copy new))
      (test 3 (+ off 0)))
  ) ; end vectors-searching

  (test-group "vectors/mutation"
    (define vs (vector 1 2 3))
    (define vf0 (vector 1 2 3))
    (define vf1 (vector 1 2 3))
    (define vf2 (vector 1 2 3))
    (define vr0 (vector 1 2 3))
    (define vr1 (vector 1 2 3))
    (define vr2 (vector 1 2 3))
    (define vc0 (vector 1 2 3 4 5))
    (define vc1 (vector 1 2 3 4 5))
    (define vc2 (vector 1 2 3 4 5))
    (define vrc0 (vector 1 2 3 4 5))
    (define vrc1 (vector 1 2 3 4 5))
    (define vrc2 (vector 1 2 3 4 5))
    (define vu0 (vector 1 2 3 4 5))
    (define vu1 (vector 1 2 3 4 5))
    (define vu2 (vector 1 2 3 4 5))
    (define vur0 (vector 1 2 3 4 5))
    (define vur1 (vector 1 2 3 4 5))
    (define vur2 (vector 1 2 3 4 5))
    (vector-swap! vs 0 1)
    (test '#(2 1 3) (vector-copy vs))
    (vector-fill! vf0 0)
    (test '#(0 0 0) (vector-copy vf0))
    (vector-fill! vf1 0 1)
    (test '#(1 0 0) (vector-copy vf1))
    (vector-fill! vf2 0 0 1)
    (test '#(0 2 3) (vector-copy vf2))
    (vector-reverse! vr0)
    (test '#(3 2 1) (vector-copy vr0))
    (vector-reverse! vr1 1)
    (test '#(1 3 2) (vector-copy vr1))
    (vector-reverse! vr2 0 2)
    (test '#(2 1 3) (vector-copy vr2))
    (vector-copy! vc0 1 '#(10 20 30))
    (test '#(1 10 20 30 5) (vector-copy vc0))
    (vector-copy! vc1 1 '#(0 10 20 30 40) 1)
    (test '#(1 10 20 30 40) (vector-copy vc1))
    (vector-copy! vc2 1 '#(0 10 20 30 40) 1 4)
    (test '#(1 10 20 30 5) (vector-copy vc2))
    (vector-reverse-copy! vrc0 1 '#(10 20 30))
    (test '#(1 30 20 10 5) (vector-copy vrc0))
    (vector-reverse-copy! vrc1 1 '#(0 10 20 30 40) 1)
    (test '#(1 40 30 20 10) (vector-copy vrc1))
    (vector-reverse-copy! vrc2 1 '#(0 10 20 30 40) 1 4)
    (test '#(1 30 20 10 5) (vector-copy vrc2))
    (vector-unfold! (lambda (i) (+ 10 i)) vu0 1 4)
    (test '#(1 11 12 13 5) (vector-copy vu0))
    (vector-unfold! (lambda (i x) (values (+ i x) (+ x 1))) vu1 1 4 0)
    (test '#(1 1 3 5 5) (vector-copy vu1))
    (vector-unfold! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vu2 1 4 0 0)
    (test '#(1 1 4 7 5) (vector-copy vu2))
    (vector-unfold-right! (lambda (i) (+ 10 i)) vur0 1 4)
    (test '#(1 11 12 13 5) (vector-copy vur0))
    (vector-unfold-right! (lambda (i x) (values (+ i x) (+ x 1))) vur1 1 4 0)
    (test '#(1 3 3 3 5) (vector-copy vur1))
    (vector-unfold-right! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vur2 1 4 0 0)
    (test '#(1 5 4 3 5) (vector-copy vur2))

  ) ; end vectors/mutation

  (test-group "vectors/conversion"
    (test '(1 2 3) (vector->list '#(1 2 3)))
    (test '(2 3) (vector->list '#(1 2 3) 1))
    (test '(1 2) (vector->list '#(1 2 3) 0 2))
    (test '#(1 2 3) (list->vector '(1 2 3)))
    (test '(3 2 1) (reverse-vector->list '#(1 2 3)))
    (test '(3 2) (reverse-vector->list '#(1 2 3) 1))
    (test '(2 1) (reverse-vector->list '#(1 2 3) 0 2))
    (test '#(3 2 1) (reverse-list->vector '(1 2 3)))
    (test "abc" (vector->string '#(#\a #\b #\c)))
    (test "bc" (vector->string '#(#\a #\b #\c) 1))
    (test "ab" (vector->string '#(#\a #\b #\c) 0 2))
    (test '#(#\a #\b #\c) (string->vector "abc"))
    (test '#(#\b #\c) (string->vector "abc" 1))
    (test '#(#\a #\b) (string->vector "abc" 0 2))
  ) ; end vectors/conversion
) ; end vectors
)