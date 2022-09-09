#lang racket/base
;;; SRFI-132 Sort

(require racket/contract
         (only-in racket/vector [vector-sort rkt-vector-sort] [vector-sort! rkt-vector-sort!] vector-copy)
         (only-in racket/unsafe/ops [unsafe-set-immutable-cdr! set-cdr!])         )
(module+ test (require rackunit))

(provide
 (contract-out
  [list-sorted? (-> (-> any/c any/c any/c) list? boolean?)]
  [vector-sorted? (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) boolean?)]
  [list-sort (-> (-> any/c any/c any/c) list? list?)]
  [list-sort! (-> (-> any/c any/c any/c) list? list?)]
  [list-stable-sort (-> (-> any/c any/c any/c) list? list?)]
  [list-stable-sort! (-> (-> any/c any/c any/c) list? list?)]
  [vector-sort (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-sort! (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [vector-stable-sort (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-stable-sort! (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [list-merge (-> (-> any/c any/c any/c) list? list? list?)]
  [list-merge! (-> (-> any/c any/c any/c) list? list? list?)]
  [vector-merge (->* ((-> any/c any/c any/c) vector? vector?)
                     (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
                     vector?)]
  [vector-merge! (->* ((-> any/c any/c any/c) vector? vector? vector?)
                      (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
                      void?)]
  [list-delete-neighbor-dups (-> (-> any/c any/c any/c) list? list?)]
  [list-delete-neighbor-dups! (-> (-> any/c any/c any/c) list? list?)]
  [vector-delete-neighbor-dups (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-delete-neighbor-dups! (->* ((-> any/c any/c any/c) vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [vector-find-median (->* ((-> any/c any/c any/c) vector? any/c) ((-> any/c any/c any/c)) vector?)]
  [vector-find-median! (->* ((-> any/c any/c any/c) vector? any/c) ((-> any/c any/c any/c)) vector?)]
  [vector-select! (->* ((-> any/c any/c any/c) vector? exact-nonnegative-integer?) (exact-nonnegative-integer? exact-nonnegative-integer?) any/c)]
  [vector-separate! (->* ((-> any/c any/c any/c) vector? exact-nonnegative-integer?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  ))

(define (list-sort < lst) (sort lst <))
(define (list-sort! < lst) (sort lst <))
(define (list-stable-sort < lst) (sort lst <))
(define (list-stable-sort! < lst) (sort lst <))

(define (vector-sort < v [start 0] [end (vector-length v)]) (rkt-vector-sort v < start end))
(define (vector-sort! < v [start 0] [end (vector-length v)]) (rkt-vector-sort! v < start end))
(define (vector-stable-sort < v [start 0] [end (vector-length v)]) (rkt-vector-sort v < start end))
(define (vector-stable-sort! < v [start 0] [end (vector-length v)]) (rkt-vector-sort! v < start end))


(define-syntax-rule (check-vector-range name v start end)
  (cond
    ((or (< start 0)
         (> start end))
     (raise-argument-error name "(integer-in 0 end)" start))
    ((> end (vector-length v))
     (raise-argument-error name "(<=/c (vector-length v))" end))
    (else (void))))

(define-syntax-rule (check-vector-ranges name v1 v2 start1 end1 start2 end2)
  (cond
    ((or (< start1 0)
         (> start1 end1))
     (raise-argument-error name "(integer-in 0 end1)" start1))
    ((> end1 (vector-length v1))
     (raise-argument-error name "(<=/c (vector-length v1))" end1))
    ((or (< start2 0)
         (> start2 end2))
     (raise-argument-error name "(integer-in 0 end2)" start2))
    ((> end2 (vector-length v2))
     (raise-argument-error name "(<=/c (vector-length v2))" end2))
    (else (void))))


;;; The sort package -- sorted predicates
;;; Olin Shivers 10/98.
;;;
;;; (list-sorted? < lis) -> boolean
;;; (vector-sorted? < v [start end]) -> boolean

(define (list-sorted? < list)
  (or (not (pair? list))
      (let lp ((prev (car list)) (tail (cdr list)))
	(or (not (pair? tail))
	    (let ((next (car tail)))
	      (and (not (< next prev))
		   (lp next (cdr tail))))))))

(define (vector-sorted? elt< v [start 0] [end (vector-length v)])
  (or (>= start end)			; Empty range
      (let lp ((i (+ start 1)) (vi-1 (vector-ref v start)))
        (or (>= i end)
            (let ((vi (vector-ref v i)))
              (and (not (elt< vi vi-1))
                   (lp (+ i 1) vi)))))))

;;; Copyright and porting non-notices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Give me a break. It's fifteen lines of code. I place this code in the
;;; public domain; help yourself.
;;;
;;; If your Scheme has a faster mechanism for handling optional arguments
;;; (e.g., Chez), you should definitely port over to it. Note that argument
;;; defaulting and error-checking are interleaved -- you don't have to
;;; error-check defaulted START/END args to see if they are fixnums that are
;;; legal vector indices for the corresponding vector, etc.

;;; This file extracts four merge procedures from lmsort.scm and vmsort.scm
;;; files written by Olin Shivers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Start of code extracted from Olin's lmsort.scm file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; list merge & list merge-sort	-*- Scheme -*-
;;; Copyright (c) 1998 by Olin Shivers.
;;; This code is open-source; see the end of the file for porting and
;;; more copyright information.
;;; Olin Shivers

;;; Exports:
;;; (list-merge  < lis lis) -> list
;;; (list-merge! < lis lis) -> list

;;; Merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These two merge procedures are stable -- ties favor list A.

(define (list-merge < a b)
  (cond ((not (pair? a)) b)
	((not (pair? b)) a)
	(else (let recur ((x (car a)) (a a)	; A is a pair; X = (CAR A).
			  (y (car b)) (b b))	; B is a pair; Y = (CAR B).
		(if (< y x)

		    (let ((b (cdr b)))
		      (if (pair? b)
			  (cons y (recur x a (car b) b))
			  (cons y a)))

		    (let ((a (cdr a)))
		      (if (pair? a)
			  (cons x (recur (car a) a y b))
			  (cons x b))))))))


;;; This destructive merge does as few SET-CDR!s as it can -- for example, if
;;; the list is already sorted, it does no SET-CDR!s at all. It is also
;;; iterative, running in constant stack.

(define (list-merge! < a b)
  ;; The logic of these two loops is completely driven by these invariants:
  ;;   SCAN-A: (CDR PREV) = A. X = (CAR A). Y = (CAR B).
  ;;   SCAN-B: (CDR PREV) = B. X = (CAR A). Y = (CAR B).
  (letrec ((scan-a (lambda (prev a x b y)		; Zip down A doing
		     (if (< y x)			; no SET-CDR!s until
			 (let ((next-b (cdr b)))	; we hit a B elt that
			   (set-cdr! prev b)		; has to be inserted.
			   (if (pair? next-b)
			       (scan-b b a x next-b (car next-b))
			       (set-cdr! b a)))

			 (let ((next-a (cdr a)))
			   (if (pair? next-a)
			       (scan-a a next-a (car next-a) b y)
			       (set-cdr! a b))))))

	   (scan-b (lambda (prev a x b y)		; Zip down B doing
		     (if (< y x)			; no SET-CDR!s until
			 (let ((next-b (cdr b)))	; we hit an A elt that
			   (if (pair? next-b)			  ; has to be
			       (scan-b b a x next-b (car next-b)) ; inserted.
			       (set-cdr! b a)))

			 (let ((next-a (cdr a)))
			   (set-cdr! prev a)
			   (if (pair? next-a)
			       (scan-a a next-a (car next-a) b y)
			       (set-cdr! a b)))))))

    (cond ((not (pair? a)) b)
	  ((not (pair? b)) a)

	  ;; B starts the answer list.
	  ((< (car b) (car a))
	   (let ((next-b (cdr b)))
	     (if (null? next-b)
		 (set-cdr! b a)
		 (scan-b b a (car a) next-b (car next-b))))
	   b)

	  ;; A starts the answer list.
	  (else (let ((next-a (cdr a)))
		  (if (null? next-a)
		      (set-cdr! a b)
		      (scan-a a next-a (car next-a) b (car b))))
		a))))

;;; Copyright
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah.


;;; Code tuning & porting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is very portable code. It's R4RS with the following exceptions:
;;; - The R5RS multiple-value VALUES & CALL-WITH-VALUES procedures for
;;;   handling multiple-value return.
;;;
;;; This code is *tightly* bummed as far as I can go in portable Scheme.
;;;
;;; - The fixnum arithmetic in LIST-MERGE-SORT! and COUNTED-LIST-MERGE!
;;;    that could be safely switched over to unsafe, fixnum-specific ops,
;;;    if you're sure that 2*maxlen is a fixnum, where maxlen is the length
;;;    of the longest list you could ever have.
;;;
;;; - I typically write my code in a style such that every CAR and CDR
;;;   application is protected by an upstream PAIR?. This is the case in this
;;;   code, so all the CAR's and CDR's could safely switched over to unsafe
;;;   versions. But check over the code before you do it, in case the source
;;;   has been altered since I wrote this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of code extracted from Olin's lmsort.scm file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Start of code extracted from Olin's vmsort.scm file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The sort package -- stable vector merge & merge sort -*- Scheme -*-
;;; Copyright (c) 1998 by Olin Shivers.
;;; This code is open-source; see the end of the file for porting and
;;; more copyright information.
;;; Olin Shivers 10/98.

;;; Exports:
;;; (vector-merge  < v1 v2 [start1 end1 start2 end2])          -> vector
;;; (vector-merge! < v v1 v2 [start0 start1 end1 start2 end2]) -> unspecific
;;;
;;; (vector-merge-sort  < v [start end temp]) -> vector
;;; (vector-merge-sort! < v [start end temp]) -> unspecific


;;; Merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (vector-merge < v1 v2 [start1 end1 start2 end2]) -> vector
;;; (vector-merge! < v v1 v2 [start start1 end1 start2 end2]) -> unspecific
;;;
;;; Stable vector merge -- V1's elements come out ahead of equal V2 elements.

(define (vector-merge < v1 v2 [start1 0] [end1 (vector-length v1)] [start2 0] [end2 (vector-length v2)])
  (check-vector-ranges 'vector-merge v1 v2 start1 end1 start2 start2)
  (let ((ans (make-vector (+ (- end1 start1) (- end2 start2)))))
    (%vector-merge! < ans v1 v2 0 start1 end1 start2 end2)
    ans))

(define (vector-merge! < v v1 v2 [start 0] [start1 0] [end1 (vector-length v1)] [start2 0] [end2 (vector-length v2)])
  (unless (and (>= start 0) (<= start (vector-length v)))
    (raise-argument-error 'vector-merge! "x(integer-in 0 (vector-length v))" start))
  (check-vector-ranges 'vector-merge! v1 v2 start1 end1 start2 start2)
  (%vector-merge! < v v1 v2 start start1 end1 start2 end2))

;;; This routine is not exported. The code is tightly bummed.
;;;
;;; If these preconditions hold, the routine can be bummed to run with
;;; unsafe vector-indexing and fixnum arithmetic ops:
;;;   - V V1 V2 are vectors.
;;;   - START START1 END1 START2 END2 are fixnums.
;;;   - (<= 0 START END0 (vector-length V),
;;;     where end0 = start + (end1 - start1) + (end2 - start2)
;;;   - (<= 0 START1 END1 (vector-length V1))
;;;   - (<= 0 START2 END2 (vector-length V2))
;;; If you put these error checks in the two client procedures above, you can
;;; safely convert this procedure to use unsafe ops -- which is why it isn't
;;; exported. This will provide *huge* speedup.

(define (%vector-merge! elt< v v1 v2 start start1 end1 start2 end2)
  (letrec ((vblit (lambda (fromv j i end) ; Blit FROMV[J,END) to V[I,?].
		    (let lp ((j j) (i i))
		      (vector-set! v i (vector-ref fromv j))
		      (let ((j (+ j 1)))
			(when (< j end) (lp j (+ i 1))))))))

    (cond ((<= end1 start1) (when (< start2 end2) (vblit v2 start2 start end2)))
          ((<= end2 start2) (vblit v1 start1 start end1))

	  ;; Invariants: I is next index of V to write; X = V1[J]; Y = V2[K].
	  (else (let lp ((i start)
			 (j start1)  (x (vector-ref v1 start1))
			 (k start2)  (y (vector-ref v2 start2)))
		  (let ((i1 (+ i 1)))	; "i+1" is a complex number in R4RS!
		    (if (elt< y x)
			(let ((k (+ k 1)))
			  (vector-set! v i y)
			  (if (< k end2)
			      (lp i1 j x k (vector-ref v2 k))
			      (vblit v1 j i1 end1)))
			(let ((j (+ j 1)))
			  (vector-set! v i x)
			  (if (< j end1)
			      (lp i1 j (vector-ref v1 j) k y)
			      (vblit v2 k i1 end2))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of code extracted from Olin's vmsort.scm file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The sort package -- delete neighboring duplicate elts
;;; Copyright (c) 1998 by Olin Shivers.
;;; This code is open-source; see the end of the file for porting and
;;; more copyright information.
;;; Olin Shivers 11/98.

;;; Problem:
;;; vector-delete-neighbor-dups pushes N stack frames, where N is the number
;;; of elements in the answer vector. This is arguably a very efficient thing
;;; to do, but it might blow out on a system with a limited stack but a big
;;; heap. We could rewrite this to "chunk" up answers in temp vectors if we
;;; push more than a certain number of frames, then allocate a final answer,
;;; copying all the chunks into the answer. But it's much more complex code.

;;; Exports:
;;; (list-delete-neighbor-dups  = lis) -> list
;;; (list-delete-neighbor-dups! = lis) -> list
;;; (vector-delete-neighbor-dups  = v [start end]) -> vector
;;; (vector-delete-neighbor-dups! = v [start end]) -> end'

;;; These procedures delete adjacent duplicate elements from a list or
;;; a vector, using a given element equality procedure. The first or leftmost
;;; element of a run of equal elements is the one that survives. The list
;;; or vector is not otherwise disordered.
;;;
;;; These procedures are linear time -- much faster than the O(n^2) general
;;; duplicate-elt deletors that do not assume any "bunching" of elements.
;;; If you want to delete duplicate elements from a large list or vector,
;;; sort the elements to bring equal items together, then use one of these
;;; procedures -- for a total time of O(n lg n).

;;; LIST-DELETE-NEIGHBOR-DUPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below are multiple versions of the LIST-DELETE-NEIGHBOR-DUPS procedure,
;;; from simple to complex. RECUR's contract: Strip off any leading X's from
;;; LIS, and return that list neighbor-dup-deleted.
;;;
;;; The final version
;;; - shares a common subtail between the input & output list, up to 1024
;;;   elements;
;;; - Needs no more than 1024 stack frames.

#;
;;; Simplest version.
;;; - Always allocates a fresh list / never shares storage.
;;; - Needs N stack frames, if answer is length N.
(define (list-delete-neighbor-dups = lis)
  (if (pair? lis)
      (let ((x0 (car lis)))
	(cons x0 (let recur ((x0 x0) (xs (cdr lis)))
		   (if (pair? xs)
		       (let ((x1  (car xs))
			     (x2+ (cdr xs)))
			  (if (= x0 x1)
			      (recur x0 x2+) ; Loop, actually.
			      (cons x1 (recur x1 x2+))))
		       xs))))
      lis))

;;; This version tries to use cons cells from input by sharing longest
;;; common tail between input & output. Still needs N stack frames, for ans
;;; of length N.
(define (list-delete-neighbor-dups = lis)
  (if (pair? lis)
      (let* ((x0 (car lis))
	     (xs (cdr lis))
	     (ans (let recur ((x0 x0) (xs xs))
		    (if (pair? xs)
			(let ((x1  (car xs))
			      (x2+ (cdr xs)))
			  (if (= x0 x1)
			      (recur x0 x2+)
			      (let ((ans-tail (recur x1 x2+)))
				(if (eq? ans-tail x2+) xs
				    (cons x1 ans-tail)))))
			xs))))
	(if (eq? ans xs) lis (cons x0 ans)))

      lis))

;;; LIST-DELETE-NEIGHBOR-DUPS!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code runs in constant list space, constant stack, and also
;;; does only the minimum SET-CDR!'s necessary.

(define (list-delete-neighbor-dups! = lis)
  (when (pair? lis)
      (let lp1 ((prev lis) (prev-elt (car lis)) (lis (cdr lis)))
	(when (pair? lis)
	    (let ((lis-elt (car lis))
		  (next (cdr lis)))
	      (when (= prev-elt lis-elt)

		  ;; We found the first elts of a run of dups, so we know
		  ;; we're going to have to do a SET-CDR!. Scan to the end of
		  ;; the run, do the SET-CDR!, and loop on LP1.
		  (let lp2 ((lis next))
		    (if (pair? lis)
			(let ((lis-elt (car lis))
			      (next (cdr lis)))
			  (if (= prev-elt lis-elt)
			      (lp2 next)
			      (begin (set-cdr! prev lis)
				     (lp1 lis lis-elt next))))
			(set-cdr! prev lis)))	; Ran off end => quit.

		  (lp1 lis lis-elt next))))))
  lis)


(define (vector-delete-neighbor-dups elt= v [start 0] [end (vector-length v)])
  (check-vector-range 'vector-delete-neighbor-dups v start end)
  (if (< start end)
      (let* ((x (vector-ref v start))
             (ans (let recur ((x x) (i start) (j 1))
                    (if (< i end)
                        (let ((y (vector-ref v i))
                              (nexti (+ i 1)))
                          (if (elt= x y)
                              (recur x nexti j)
                              (let ((ansvec (recur y nexti (+ j 1))))
                                (vector-set! ansvec j y)
                                ansvec)))
                        (make-vector j)))))
        (vector-set! ans 0 x)
        ans)
      '#()))

;;; Packs the surviving elements to the left, in range [start,end'),
;;; and returns END'.
(define (vector-delete-neighbor-dups! elt= v [start 0] [end (vector-length v)])
  (check-vector-range 'vector-delete-neighbor-dups! v start end)
  (if (>= start end)
      end
      ;; To eliminate unnecessary copying (read elt i then write the value
      ;; back at index i), we scan until we find the first dup.
      (let skip ((j start) (vj (vector-ref v start)))
        (let ((j+1 (+ j 1)))
          (if (>= j+1 end)
              end
              (let ((vj+1 (vector-ref v j+1)))
                (if (not (elt= vj vj+1))
                    (skip j+1 vj+1)

                    ;; OK -- j & j+1 are dups, so we're committed to moving
                    ;; data around. In lp2, v[start,j] is what we've done;
                    ;; v[k,end) is what we have yet to handle.
                    (let lp2 ((j j) (vj vj) (k (+ j 2)))
                      (let lp3 ((k k))
                        (if (>= k end)
                            (+ j 1) ; Done.
                            (let ((vk (vector-ref v k))
                                  (k+1 (+ k 1)))
                              (if (elt= vj vk)
                                  (lp3 k+1)
                                  (let ((j+1 (+ j 1)))
                                    (vector-set! v j+1 vk)
                                    (lp2 j+1 vk k+1))))))))))))))

;;; Copyright
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah. Don't you think source files should contain more lines
;;; of code than copyright notice?
;;;
;;; Code porting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; If your Scheme has a faster mechanism for handling optional arguments
;;; (e.g., Chez), you should definitely port over to it. Note that argument
;;; defaulting and error-checking are interleaved -- you don't have to
;;; error-check defaulted START/END args to see if they are fixnums that are
;;; legal vector indices for the corresponding vector, etc.

;;; Linear-time (average case) algorithms for:
;;;
;;; Selecting the kth smallest element from an unsorted vector.
;;; Selecting the kth and (k+1)st smallest elements from an unsorted vector.
;;; Selecting the median from an unsorted vector.

;;; These procedures are part of SRFI 132 but are missing from
;;; its reference implementation as of 10 March 2016.

;;; SRFI 132 says this procedure runs in O(n) time.
;;; As implemented, however, the worst-case time is O(n^2) because
;;; vector-select is implemented using randomized quickselect.
;;; The average time is O(n), and you'd have to be unlucky
;;; to approach the worst case.

(define (vector-find-median < v knil [mean (lambda (a b) (/ (+ a b) 2))])
  (let ((n (vector-length v)))
    (cond ((zero? n)
           knil)
          ((odd? n)
           (%vector-select < v (quotient n 2) 0 n))
          (else
           (call-with-values
            (lambda () (%vector-select2 < v (- (quotient n 2) 1) 0 n))
            (lambda (a b)
              (mean a b)))))))

;;; For this procedure, the SRFI 132 specification
;;; demands the vector be sorted (by side effect).

(define (vector-find-median! < v knil [mean (lambda (a b) (/ (+ a b) 2))])
  (let ((n (vector-length v)))
    (vector-sort! < v)
    (cond ((zero? n)
           knil)
          ((odd? n)
           (vector-ref v (quotient n 2)))
          (else
           (mean (vector-ref v (- (quotient n 2) 1))
                 (vector-ref v (quotient n 2)))))))

;;; SRFI 132 says this procedure runs in O(n) time.
;;; As implemented, however, the worst-case time is O(n^2).
;;; The average time is O(n), and you'd have to be unlucky
;;; to approach the worst case.
;;;
;;; After rest argument processing, calls the private version defined below.

(define (vector-select < v k [start 0] [end (vector-length v)])
  (check-vector-range 'vector-select v start end)
  (%vector-select < v k start end))

;;; The vector-select procedure is needed internally to implement
;;; vector-find-median, but SRFI 132 has been changed (for no good
;;; reason) to export vector-select! instead of vector-select.
;;; Fortunately, vector-select! is not required to have side effects.

(define vector-select! vector-select)

;;; This could be made slightly more efficient, but who cares?

(define (r7rs-vector-fill! v elem start end)
  (for ([n (in-range start end)])
    (vector-set! v n elem)))

(define (vector-separate! < v k [start 0] [end (vector-length v)])
  (check-vector-range 'vector-separate! v start end)
  (when (and (> k 0)
           (> end start))
      (let ((pivot (vector-select < v (- k 1) start end)))
        (call-with-values
         (lambda () (count-smaller < pivot v start end 0 0))
         (lambda (count count2)
           (let* ((v2 (make-vector count))
                  (v3 (make-vector (- end start count count2))))
             (copy-smaller! < pivot v2 0 v start end)
             (copy-bigger! < pivot v3 0 v start end)
             (vector-copy! v start v2)
             (r7rs-vector-fill! v pivot (+ start count) (+ start count count2))
             (vector-copy! v (+ start count count2) v3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For small ranges, sorting may be the fastest way to find the kth element.
;;; This threshold is not at all critical, and may not even be worthwhile.

(define just-sort-it-threshold 50)

;;; Given
;;;     an irreflexive total order <?
;;;     a vector v
;;;     an index k
;;;     an index start
;;;     an index end
;;; with
;;;     0 <= k < (- end start)
;;;     0 <= start < end <= (vector-length v)
;;; returns
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;; but is usually faster than that.

(define-syntax-rule (assert test)
  (unless test
    (error "assertion failed")))

(define (%vector-select <? v k start end)
  (assert (and 'vector-select
               (procedure? <?)
               (vector? v)
               (exact-integer? k)
               (exact-integer? start)
               (exact-integer? end)
               (<= 0 k (- end start 1))
               (<= 0 start end (vector-length v))))
  (%%vector-select <? v k start end))

;;; Given
;;;     an irreflexive total order <?
;;;     a vector v
;;;     an index k
;;;     an index start
;;;     an index end
;;; with
;;;     0 <= k < (- end start 1)
;;;     0 <= start < end <= (vector-length v)
;;; returns two values:
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k 1))
;;; but is usually faster than that.

(define (%vector-select2 <? v k start end)
  (assert (and 'vector-select
;               (procedure? <?)
;               (vector? v)
;               (exact-integer? k)
;               (exact-integer? start)
;               (exact-integer? end)
               (<= 0 k (- end start 1 1))
               (<= 0 start end (vector-length v))))
  (%%vector-select2 <? v k start end))

;;; Like %vector-select, but its preconditions have been checked.

(define (%%vector-select <? v k start end)
  (let ((size (- end start)))
    (cond ((= 1 size)
           (vector-ref v (+ k start)))
          ((= 2 size)
           (cond ((<? (vector-ref v start)
                      (vector-ref v (+ start 1)))
                  (vector-ref v (+ k start)))
                 (else
                  (vector-ref v (+ (- 1 k) start)))))
          ((< size just-sort-it-threshold)
           (vector-ref (vector-sort <? (vector-copy v start end)) k))
          (else
           (let* ((ip (random size))
                  (pivot (vector-ref v (+ start ip))))
             (call-with-values
              (lambda () (count-smaller <? pivot v start end 0 0))
              (lambda (count count2)
                (cond ((< k count)
                       (let* ((n count)
                              (v2 (make-vector n)))
                         (copy-smaller! <? pivot v2 0 v start end)
                         (%%vector-select <? v2 k 0 n)))
                      ((< k (+ count count2))
                       pivot)
                      (else
                       (let* ((n (- size count count2))
                              (v2 (make-vector n))
                              (k2 (- k count count2)))
                         (copy-bigger! <? pivot v2 0 v start end)
                         (%%vector-select <? v2 k2 0 n)))))))))))

;;; Like %%vector-select, but returns two values:
;;;
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k 1))
;;;
;;; Returning two values is useful when finding the median of an even
;;; number of things.

(define (%%vector-select2 <? v k start end)
  (let ((size (- end start)))
    (cond ((= 2 size)
           (let ((a (vector-ref v start))
                 (b (vector-ref v (+ start 1))))
             (cond ((<? a b)
                    (values a b))
                   (else
                    (values b a)))))
          ((< size just-sort-it-threshold)
           (let ((v2 (vector-sort <? (vector-copy v start end))))
             (values (vector-ref v2 k)
                     (vector-ref v2 (+ k 1)))))
          (else
           (let* ((ip (random size))
                  (pivot (vector-ref v (+ start ip))))
             (call-with-values
              (lambda () (count-smaller <? pivot v start end 0 0))
              (lambda (count count2)
                (cond ((= (+ k 1) count)
                       (values (%%vector-select <? v k start end)
                               pivot))
                      ((< k count)
                       (let* ((n count)
                              (v2 (make-vector n)))
                         (copy-smaller! <? pivot v2 0 v start end)
                         (%%vector-select2 <? v2 k 0 n)))
                      ((< k (+ count count2))
                       (values pivot
                               (if (< (+ k 1) (+ count count2))
                                   pivot
                                   (%%vector-select <? v (+ k 1) start end))))
                      (else
                       (let* ((n (- size count count2))
                              (v2 (make-vector n))
                              (k2 (- k count count2)))
                         (copy-bigger! <? pivot v2 0 v start end)
                         (%%vector-select2 <? v2 k2 0 n)))))))))))

;;; Counts how many elements within the range are less than the pivot
;;; and how many are equal to the pivot, returning both of those counts.

(define (count-smaller <? pivot v i end count count2)
  (cond ((= i end)
         (values count count2))
        ((<? (vector-ref v i) pivot)
         (count-smaller <? pivot v (+ i 1) end (+ count 1) count2))
        ((<? pivot (vector-ref v i))
         (count-smaller <? pivot v (+ i 1) end count count2))
        (else
         (count-smaller <? pivot v (+ i 1) end count (+ count2 1)))))

;;; Like vector-copy! but copies an element only if it is less than the pivot.
;;; The destination vector must be large enough.

(define (copy-smaller! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? (vector-ref src start) pivot)
         (vector-set! dst at (vector-ref src start))
         (copy-smaller! <? pivot dst (+ at 1) src (+ start 1) end))
        (else
         (copy-smaller! <? pivot dst at src (+ start 1) end))))

;;; Like copy-smaller! but copies only elements that are greater than the pivot.

(define (copy-bigger! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? pivot (vector-ref src start))
         (vector-set! dst at (vector-ref src start))
         (copy-bigger! <? pivot dst (+ at 1) src (+ start 1) end))
        (else
         (copy-bigger! <? pivot dst at src (+ start 1) end))))


(module+ test
  (require (only-in srfi/1 list-copy))
  ;;; Test program for SRFI 132 (Sort Libraries).

  ;;; Copyright © William D Clinger (2016).
  ;;;
  ;;; Permission is hereby granted, free of charge, to any person
  ;;; obtaining a copy of this software and associated documentation
  ;;; files (the "Software"), to deal in the Software without
  ;;; restriction, including without limitation the rights to use,
  ;;; copy, modify, merge, publish, distribute, sublicense, and/or
  ;;; sell copies of the Software, and to permit persons to whom the
  ;;; Software is furnished to do so, subject to the following
  ;;; conditions:
  ;;;
  ;;; The above copyright notice and this permission notice shall be
  ;;; included in all copies or substantial portions of the Software.
  ;;;
  ;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  ;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  ;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  ;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  ;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  ;;; OTHER DEALINGS IN THE SOFTWARE.

  ;;; Embeds Olin's test harness.  Here is his copyright notice:

  ;;; This code is
  ;;;     Copyright (c) 1998 by Olin Shivers.
  ;;; The terms are: You may do as you please with this code, as long as
  ;;; you do not delete this notice or hold me responsible for any outcome
  ;;; related to its use.
  ;;;
  ;;; Blah blah blah. Don't you think source files should contain more lines
  ;;; of code than copyright notice?


  (define (random-vector size)
    (let ((v (make-vector size)))
      (fill-vector-randomly! v (* 10 size))
      v))

  (define (fill-vector-randomly! v range)
    (let ((half (quotient range 2)))
      (do ((i (- (vector-length v) 1) (- i 1)))
          ((< i 0))
        (vector-set! v i (- (random range) half)))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Additional tests written specifically for SRFI 132.
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax or
    (syntax-rules (equal? fail)
      ((or (equal? tst expected)
           (fail id))
       (test-equal? (symbol->string id) tst expected))
      ((or tst (fail id))
       (test-not-false (symbol->string id) tst))))

  (or (list-sorted? > '())
      (fail 'list-sorted?:empty-list))

  (or (list-sorted? > '(987))
      (fail 'list-sorted?:singleton))

  (or (list-sorted? > '(9 8 7))
      (fail 'list-sorted?:non-empty-list))

  (or (vector-sorted? > '#())
      (fail 'vector-sorted?:empty-vector))

  (or (vector-sorted? > '#(987))
      (fail 'vector-sorted?:singleton))

  (or (vector-sorted? > '#(9 8 7 6 5))
      (fail 'vector-sorted?:non-empty-vector))

  (or (vector-sorted? > '#() 0)
      (fail 'vector-sorted?:empty-vector:0))

  (or (vector-sorted? > '#(987) 1)
      (fail 'vector-sorted?:singleton:1))

  (or (vector-sorted? > '#(9 8 7 6 5) 1)
      (fail 'vector-sorted?:non-empty-vector:1))

  (or (vector-sorted? > '#() 0 0)
      (fail 'vector-sorted?:empty-vector:0:0))

  (or (vector-sorted? > '#(987) 1 1)
      (fail 'vector-sorted?:singleton:1:1))

  (or (vector-sorted? > '#(9 8 7 6 5) 1 2)
      (fail 'vector-sorted?:non-empty-vector:1:2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (list-sort > (list))
              '())
      (fail 'list-sort:empty-list))

  (or (equal? (list-sort > (list 987))
              '(987))
      (fail 'list-sort:singleton))

  (or (equal? (list-sort > (list 987 654))
              '(987 654))
      (fail 'list-sort:doubleton))

  (or (equal? (list-sort > (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 7 6 5 4 3 2 1 0))
      (fail 'list-sort:iota10))

  (or (equal? (list-stable-sort > (list))
              '())
      (fail 'list-stable-sort:empty-list))

  (or (equal? (list-stable-sort > (list 987))
              '(987))
      (fail 'list-stable-sort:singleton))

  (or (equal? (list-stable-sort > (list 987 654))
              '(987 654))
      (fail 'list-stable-sort:doubleton))

  (or (equal? (list-stable-sort > (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 7 6 5 4 3 2 1 0))
      (fail 'list-stable-sort:iota10))

  (or (equal? (list-stable-sort (lambda (x y)
                                  (> (quotient x 2)
                                     (quotient y 2)))
                                (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 6 7 4 5 3 2 0 1))
      (fail 'list-stable-sort:iota10-quotient2))

  (or (equal? (let ((v (vector)))
                (vector-sort > v))
              '#())
      (fail 'vector-sort:empty-vector))

  (or (equal? (let ((v (vector 987)))
                (vector-sort > (vector 987)))
              '#(987))
      (fail 'vector-sort:singleton))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort > v))
              '#(987 654))
      (fail 'vector-sort:doubleton))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort > v))
              '#(9 8 7 6 5 4 3 2 1 0))
      (fail 'vector-sort:iota10))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort > v))
              '#())
      (fail 'vector-stable-sort:empty-vector))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort > (vector 987)))
              '#(987))
      (fail 'vector-stable-sort:singleton))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort > v))
              '#(987 654))
      (fail 'vector-stable-sort:doubleton))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort > v))
              '#(9 8 7 6 5 4 3 2 1 0))
      (fail 'vector-stable-sort:iota10))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort (lambda (x y)
                                      (> (quotient x 2)
                                         (quotient y 2)))
                                    v))
              '#(9 8 6 7 4 5 3 2 0 1))
      (fail 'vector-stable-sort:iota10-quotient2))

  (or (equal? (let ((v (vector)))
                (vector-sort > v 0))
              '#())
      (fail 'vector-sort:empty-vector:0))

  (or (equal? (let ((v (vector 987)))
                (vector-sort > (vector 987) 1))
              '#())
      (fail 'vector-sort:singleton:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort > v 1))
              '#(654))
      (fail 'vector-sort:doubleton:1))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort > v 3))
              '#(7 5 4 3 2 1 0))
      (fail 'vector-sort:iota10:3))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort > v 0))
              '#())
      (fail 'vector-stable-sort:empty-vector:0))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort > (vector 987) 1))
              '#())
      (fail 'vector-stable-sort:singleton:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort < v 0 2))
              '#(654 987))
      (fail 'vector-stable-sort:doubleton:0:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort > v 3))
              '#(7 5 4 3 2 1 0))
      (fail 'vector-stable-sort:iota10:3))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort (lambda (x y)
                                      (> (quotient x 2)
                                         (quotient y 2)))
                                    v
                                    3))
              '#(7 4 5 3 2 0 1))
      (fail 'vector-stable-sort:iota10-quotient2:3))

  (or (equal? (let ((v (vector)))
                (vector-sort > v 0 0))
              '#())
      (fail 'vector-sort:empty-vector:0:0))

  (or (equal? (let ((v (vector 987)))
                (vector-sort > (vector 987) 1 1))
              '#())
      (fail 'vector-sort:singleton:1:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort > v 1 2))
              '#(654))
      (fail 'vector-sort:doubleton:1:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort > v 4 8))
              '#(5 4 2 0))
      (fail 'vector-sort:iota10:4:8))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort > v 0 0))
              '#())
      (fail 'vector-stable-sort:empty-vector:0:0))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort > (vector 987) 1 1))
              '#())
      (fail 'vector-stable-sort:singleton:1:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort > v 1 2))
              '#(654))
      (fail 'vector-stable-sort:doubleton:1:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort > v 2 6))
              '#(6 4 3 0))
      (fail 'vector-stable-sort:iota10:2:6))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort (lambda (x y)
                                      (> (quotient x 2)
                                         (quotient y 2)))
                                    v
                                    1
                                    8))
              '#(8 6 4 5 3 2 0))
      (fail 'vector-stable-sort:iota10-quotient2:1:8))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (list-sort! > (list))
              '())
      (fail 'list-sort!:empty-list))

  (or (equal? (list-sort! > (list 987))
              '(987))
      (fail 'list-sort!:singleton))

  (or (equal? (list-sort! > (list 987 654))
              '(987 654))
      (fail 'list-sort!:doubleton))

  (or (equal? (list-sort! > (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 7 6 5 4 3 2 1 0))
      (fail 'list-sort!:iota10))

  (or (equal? (list-stable-sort! > (list))
              '())
      (fail 'list-stable-sort!:empty-list))

  (or (equal? (list-stable-sort! > (list 987))
              '(987))
      (fail 'list-stable-sort!:singleton))

  (or (equal? (list-stable-sort! > (list 987 654))
              '(987 654))
      (fail 'list-stable-sort!:doubleton))

  (or (equal? (list-stable-sort! > (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 7 6 5 4 3 2 1 0))
      (fail 'list-stable-sort!:iota10))

  (or (equal? (list-stable-sort! (lambda (x y)
                                   (> (quotient x 2)
                                      (quotient y 2)))
                                 (list 9 8 6 3 0 4 2 5 7 1))
              '(9 8 6 7 4 5 3 2 0 1))
      (fail 'list-stable-sort!:iota10-quotient2))

  (or (equal? (let ((v (vector)))
                (vector-sort! > v)
                v)
              '#())
      (fail 'vector-sort!:empty-vector))

  (or (equal? (let ((v (vector 987)))
                (vector-sort! > (vector 987))
                v)
              '#(987))
      (fail 'vector-sort!:singleton))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort! > v)
                v)
              '#(987 654))
      (fail 'vector-sort!:doubleton))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort! > v)
                v)
              '#(9 8 7 6 5 4 3 2 1 0))
      (fail 'vector-sort!:iota10))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort! > v)
                v)
              '#())
      (fail 'vector-stable-sort!:empty-vector))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort! > (vector 987))
                v)
              '#(987))
      (fail 'vector-stable-sort!:singleton))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort! > v)
                v)
              '#(987 654))
      (fail 'vector-stable-sort!:doubleton))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! > v)
                v)
              '#(9 8 7 6 5 4 3 2 1 0))
      (fail 'vector-stable-sort!:iota10))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! (lambda (x y)
                                       (> (quotient x 2)
                                          (quotient y 2)))
                                     v)
                v)
              '#(9 8 6 7 4 5 3 2 0 1))
      (fail 'vector-stable-sort!:iota10-quotient2))

  (or (equal? (let ((v (vector)))
                (vector-sort! > v 0)
                v)
              '#())
      (fail 'vector-sort!:empty-vector:0))

  (or (equal? (let ((v (vector 987)))
                (vector-sort! > (vector 987) 1)
                v)
              '#(987))
      (fail 'vector-sort!:singleton:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort! > v 1)
                v)
              '#(987 654))
      (fail 'vector-sort!:doubleton:1))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort! > v 3)
                v)
              '#(9 8 6 7 5 4 3 2 1 0))
      (fail 'vector-sort!:iota10:3))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort! > v 0)
                v)
              '#())
      (fail 'vector-stable-sort!:empty-vector:0))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort! > (vector 987) 1)
                v)
              '#(987))
      (fail 'vector-stable-sort!:singleton:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort! < v 0 2)
                v)
              '#(654 987))
      (fail 'vector-stable-sort!:doubleton:0:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! > v 3)
                v)
              '#(9 8 6 7 5 4 3 2 1 0))
      (fail 'vector-stable-sort!:iota10:3))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! (lambda (x y)
                                       (> (quotient x 2)
                                          (quotient y 2)))
                                     v
                                     3)
                v)
              '#(9 8 6 7 4 5 3 2 0 1))
      (fail 'vector-stable-sort!:iota10-quotient2:3))

  (or (equal? (let ((v (vector)))
                (vector-sort! > v 0 0)
                v)
              '#())
      (fail 'vector-sort!:empty-vector:0:0))

  (or (equal? (let ((v (vector 987)))
                (vector-sort! > (vector 987) 1 1)
                v)
              '#(987))
      (fail 'vector-sort!:singleton:1:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-sort! > v 1 2)
                v)
              '#(987 654))
      (fail 'vector-sort!:doubleton:1:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-sort! > v 4 8)
                v)
              '#(9 8 6 3 5 4 2 0 7 1))
      (fail 'vector-sort!:iota10:4:8))

  (or (equal? (let ((v (vector)))
                (vector-stable-sort! > v 0 0)
                v)
              '#())
      (fail 'vector-stable-sort!:empty-vector:0:0))

  (or (equal? (let ((v (vector 987)))
                (vector-stable-sort! > (vector 987) 1 1)
                v)
              '#(987))
      (fail 'vector-stable-sort!:singleton:1:1))

  (or (equal? (let ((v (vector 987 654)))
                (vector-stable-sort! > v 1 2)
                v)
              '#(987 654))
      (fail 'vector-stable-sort!:doubleton:1:2))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! > v 2 6)
                v)
              '#(9 8 6 4 3 0 2 5 7 1))
      (fail 'vector-stable-sort!:iota10:2:6))

  (or (equal? (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
                (vector-stable-sort! (lambda (x y)
                                       (> (quotient x 2)
                                          (quotient y 2)))
                                     v
                                     1
                                     8)
                v)
              '#(9 8 6 4 5 3 2 0 7 1))
      (fail 'vector-stable-sort!:iota10-quotient2:1:8))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (list-merge > (list) (list))
              '())
      (fail 'list-merge:empty:empty))

  (or (equal? (list-merge > (list) (list 9 6 3 0))
              '(9 6 3 0))
      (fail 'list-merge:empty:nonempty))

  (or (equal? (list-merge > (list 9 7 5 3 1) (list))
              '(9 7 5 3 1))
      (fail 'list-merge:nonempty:empty))

  (or (equal? (list-merge > (list 9 7 5 3 1) (list 9 6 3 0))
              '(9 9 7 6 5 3 3 1 0))
      (fail 'list-merge:nonempty:nonempty))

  (or (equal? (list-merge! > (list) (list))
              '())
      (fail 'list-merge!:empty:empty))

  (or (equal? (list-merge! > (list) (list 9 6 3 0))
              '(9 6 3 0))
      (fail 'list-merge!:empty:nonempty))

  (or (equal? (list-merge! > (list 9 7 5 3 1) (list))
              '(9 7 5 3 1))
      (fail 'list-merge!:nonempty:empty))

  (or (equal? (list-merge! > (list 9 7 5 3 1) (list 9 6 3 0))
              '(9 9 7 6 5 3 3 1 0))
      (fail 'list-merge!:nonempty:nonempty))

  (or (equal? (vector-merge > (vector) (vector))
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0))
              '#(9 6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector))
              '#(9 7 5 3 1))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0))
              '#(9 9 7 6 5 3 3 1 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector))
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0))
                v)
              '#( 9  6  3  0 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector))
                v)
              '#( 9  7  5  3  1 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0))
                v)
              '#( 9  9  7  6  5  3  3  1  0 #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:0))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 0)
                v)
              '#( 9  6  3  0 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:0))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 0)
                v)
              '#( 9  7  5  3  1 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:0))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 0)
                v)
              '#( 9  9  7  6  5  3  3  1  0 #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:0))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2)
                v)
              '#(#f #f 9  6  3  0 #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2)
                v)
              '#(#f #f  9  7  5  3  1 #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
                v)
              '#(#f #f 9  9  7  6  5  3  3  1  0 #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0)
              '#(9 6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2)
              '#(5 3 1))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
              '#(9 6 5 3 3 1 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0)
                v)
              '#(#f #f 9  6  3  0 #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2)
                v)
              '#(#f #f 5  3  1 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2)
                v)
              '#(#f #f  9   6  5  3  3  1  0 #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0)
              '#(9 6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 5)
              '#(5 3 1))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 5)
              '#(9 6 5 3 3 1 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
                v)
              '#(#f #f 9  6  3  0 #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 5)
                v)
              '#(#f #f 5  3  1 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 5)
                v)
              '#(#f #f  9  6  5  3  3  1  0 #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  ;;; Some tests are duplicated to make the pattern easier to discern.

  (or (equal? (vector-merge > (vector) (vector) 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0)
              '#(9 6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 4)
              '#(5 3))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4)
              '#(9 6 5 3 3 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
                v)
              '#(#f #f 9  6  3  0 #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4)
                v)
              '#(#f #f 5  3 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4)
                v)
              '#(#f #f  9  6  5  3  3  0 #f #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0 0)
              '#(9 6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
              '#(5 3))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 0)
              '#(9 6 5 3 3 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 0)
                v)
              '#(#f #f  9  6  3  0 #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
                v)
              '#(#f #f  5  3 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 0)
                v)
              '#(#f #f  9  6  5  3  3  0 #f #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0 1)
              '#(6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
              '#(5 3))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1)
              '#(6 5 3 3 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1)
                v)
              '#(#f #f  6  3  0 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
                v)
              '#(#f #f  5  3 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1)
                v)
              '#(#f #f  6  5  3  3  0 #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0 0 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 4)
              '#(6 3 0))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
              '#(5 3))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 4)
              '#(6 5 3 3 0))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 4)
                v)
              '#(#f #f  6  3  0 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
                v)
              '#(#f #f  5  3 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 4)
                v)
              '#(#f #f  6  5  3  3  0 #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  (or (equal? (vector-merge > (vector) (vector) 0 0 0 0)
              '#())
      (fail 'vector-merge:empty:empty))

  (or (equal? (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 2)
              '#(6))
      (fail 'vector-merge:empty:nonempty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
              '#(5 3))
      (fail 'vector-merge:nonempty:empty))

  (or (equal? (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 2)
              '#(6 5 3))
      (fail 'vector-merge:nonempty:nonempty))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector) 2 0 0 0 0)
                v)
              '#(#f #f #f #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 2)
                v)
              '#(#f #f  6 #f #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:empty:nonempty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
                v)
              '#(#f #f  5  3 #f #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:empty:2))

  (or (equal? (let ((v (make-vector 12 #f)))
                (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 2)
                v)
              '#(#f #f  6  5  3 #f #f #f #f #f #f #f))
      (fail 'vector-merge!:nonempty:nonempty:2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (list-delete-neighbor-dups char=? (list))
              '())
      (fail 'list-delete-neighbor-dups:empty))

  (or (equal? (list-delete-neighbor-dups char=? (list #\a))
              '(#\a))
      (fail 'list-delete-neighbor-dups:singleton))

  (or (equal? (list-delete-neighbor-dups char=? (list #\a #\a #\a #\b #\b #\a))
              '(#\a #\b #\a))
      (fail 'list-delete-neighbor-dups:nonempty))

  (or (equal? (list-delete-neighbor-dups! char=? (list))
              '())
      (fail 'list-delete-neighbor-dups!:empty))

  (or (equal? (list-delete-neighbor-dups! char=? (list #\a))
              '(#\a))
      (fail 'list-delete-neighbor-dups!:singleton))

  (or (equal? (list-delete-neighbor-dups! char=? (list #\a #\a #\a #\b #\b #\a))
              '(#\a #\b #\a))
      (fail 'list-delete-neighbor-dups!:nonempty))

  (or (equal? (let ((v (vector)))
                (vector-delete-neighbor-dups char=? v))
              '#())
      (fail 'vector-delete-neighbor-dups:empty))

  (or (equal? (let ((v (vector #\a)))
                (vector-delete-neighbor-dups char=? v))
              '#(#\a))
      (fail 'vector-delete-neighbor-dups:singleton))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (vector-delete-neighbor-dups char=? v))
              '#(#\a #\b #\a))
      (fail 'vector-delete-neighbor-dups:nonempty))

  (or (equal? (let ((v (vector)))
                (list (vector-delete-neighbor-dups! char=? v) v))
              '(0 #()))
      (fail 'vector-delete-neighbor-dups!:empty))

  (or (equal? (let ((v (vector #\a)))
                (list (vector-delete-neighbor-dups! char=? v) v))
              '(1 #(#\a)))
      (fail 'vector-delete-neighbor-dups!:singleton))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (list (vector-delete-neighbor-dups! char=? v) v))
              '(3 #(#\a #\b #\a #\b #\b #\a)))
      (fail 'vector-delete-neighbor-dups!:nonempty))

  (or (equal? (let ((v (vector)))
                (vector-delete-neighbor-dups char=? v 0))
              '#())
      (fail 'vector-delete-neighbor-dups:empty:0))

  (or (equal? (let ((v (vector #\a)))
                (vector-delete-neighbor-dups char=? v 0))
              '#(#\a))
      (fail 'vector-delete-neighbor-dups:singleton:0))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (vector-delete-neighbor-dups char=? v 0))
              '#(#\a #\b #\a))
      (fail 'vector-delete-neighbor-dups:nonempty:0))

  (or (equal? (let ((v (vector)))
                (list (vector-delete-neighbor-dups! char=? v 0) v))
              '(0 #()))
      (fail 'vector-delete-neighbor-dups!:empty:0))

  (or (equal? (let ((v (vector #\a)))
                (list (vector-delete-neighbor-dups! char=? v 0) v))
              '(1 #(#\a)))
      (fail 'vector-delete-neighbor-dups!:singleton:0))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (list (vector-delete-neighbor-dups! char=? v 0) v))
              '(3 #(#\a #\b #\a #\b #\b #\a)))
      (fail 'vector-delete-neighbor-dups!:nonempty:0))

  (or (equal? (let ((v (vector)))
                (vector-delete-neighbor-dups char=? v 0))
              '#())
      (fail 'vector-delete-neighbor-dups:empty:0))

  (or (equal? (let ((v (vector #\a)))
                (vector-delete-neighbor-dups char=? v 1))
              '#())
      (fail 'vector-delete-neighbor-dups:singleton:1))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (vector-delete-neighbor-dups char=? v 3))
              '#(#\b #\a))
      (fail 'vector-delete-neighbor-dups:nonempty:3))

  (or (equal? (let ((v (vector)))
                (list (vector-delete-neighbor-dups! char=? v 0) v))
              '(0 #()))
      (fail 'vector-delete-neighbor-dups!:empty:0))

  (or (equal? (let ((v (vector #\a)))
                (list (vector-delete-neighbor-dups! char=? v 1) v))
              '(1 #(#\a)))
      (fail 'vector-delete-neighbor-dups!:singleton:1))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (list (vector-delete-neighbor-dups! char=? v 3) v))
              '(5 #(#\a #\a #\a #\b #\a #\a)))
      (fail 'vector-delete-neighbor-dups!:nonempty:3))

  (or (equal? (let ((v (vector)))
                (vector-delete-neighbor-dups char=? v 0 0))
              '#())
      (fail 'vector-delete-neighbor-dups:empty:0:0))

  (or (equal? (let ((v (vector #\a)))
                (vector-delete-neighbor-dups char=? v 1 1))
              '#())
      (fail 'vector-delete-neighbor-dups:singleton:1:1))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (vector-delete-neighbor-dups char=? v 3 5))
              '#(#\b))
      (fail 'vector-delete-neighbor-dups:nonempty:3:5))

  (or (equal? (let ((v (vector)))
                (list (vector-delete-neighbor-dups! char=? v 0 0) v))
              '(0 #()))
      (fail 'vector-delete-neighbor-dups!:empty:0:0))

  (or (equal? (let ((v (vector #\a)))
                (list (vector-delete-neighbor-dups! char=? v 0 1) v))
              '(1 #(#\a)))
      (fail 'vector-delete-neighbor-dups!:singleton:0:1))

  (or (equal? (let ((v (vector #\a)))
                (list (vector-delete-neighbor-dups! char=? v 1 1) v))
              '(1 #(#\a)))
      (fail 'vector-delete-neighbor-dups!:singleton:1:1))

  (or (equal? (let ((v (vector #\a #\a #\a #\b #\b #\a)))
                (list (vector-delete-neighbor-dups! char=? v 3 5) v))
              '(4 #(#\a #\a #\a #\b #\b #\a)))
      (fail 'vector-delete-neighbor-dups!:nonempty:3:5))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (vector-find-median < (vector) "knil")
              "knil")
      (fail 'vector-find-median:empty))

  (or (equal? (vector-find-median < (vector 17) "knil")
              17)
      (fail 'vector-find-median:singleton))

  (or (equal? (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil")
              12)
      (fail 'vector-find-median:8same))

  (or (equal? (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil")
              23/2)
      (fail 'vector-find-median:8diff))

  (or (equal? (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil" list)
              (list 12 12))
      (fail 'vector-find-median:8samelist))

  (or (equal? (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil" list)
              (list 11 12))
      (fail 'vector-find-median:8difflist))

  (or (equal? (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil")
              7)
      (fail 'vector-find-median:9))

  (or (equal? (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil" list)
              7)
      (fail 'vector-find-median:9list))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (let ((v (vector 19)))
                (vector-select! < v 0))
              19)
      (fail 'vector-select!:singleton:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0))
              3)
      (fail 'vector-select!:ten:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2))
              9)
      (fail 'vector-select!:ten:2))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 8))
              22)
      (fail 'vector-select!:ten:8))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 9))
              23)
      (fail 'vector-select!:ten:9))

  (or (equal? (let ((v (vector 19)))
                (vector-select! < v 0 0))
              19)
      (fail 'vector-select!:singleton:0:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0 0))
              3)
      (fail 'vector-select!:ten:0:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2 0))
              9)
      (fail 'vector-select!:ten:2:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 8 0))
              22)
      (fail 'vector-select!:ten:8:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 9 0))
              23)
      (fail 'vector-select!:ten:9:0))

  (or (equal? (let ((v (vector 19)))
                (vector-select! < v 0 0 1))
              19)
      (fail 'vector-select!:singleton:0:0:1))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0 0 10))
              3)
      (fail 'vector-select!:ten:0:0:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2 0 10))
              9)
      (fail 'vector-select!:ten:2:0:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 8 0 10))
              22)
      (fail 'vector-select!:ten:8:0:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 9 0 10))
              23)
      (fail 'vector-select!:ten:9:0:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0 4 10))
              3)
      (fail 'vector-select!:ten:0:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2 4 10))
              13)
      (fail 'vector-select!:ten:2:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 4 4 10))
              21)
      (fail 'vector-select!:ten:4:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 5 4 10))
              23)
      (fail 'vector-select!:ten:5:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0 4 10))
              3)
      (fail 'vector-select!:ten:0:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2 4 10))
              13)
      (fail 'vector-select!:ten:2:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 3 4 10))
              13)
      (fail 'vector-select!:ten:3:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 4 4 10))
              21)
      (fail 'vector-select!:ten:4:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 5 4 10))
              23)
      (fail 'vector-select!:ten:9:4:10))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 0 4 8))
              9)
      (fail 'vector-select!:ten:0:4:8))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 1 4 8))
              13)
      (fail 'vector-select!:ten:1:4:8))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 2 4 8))
              13)
      (fail 'vector-select!:ten:2:4:8))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-select! < v 3 4 8))
              21)
      (fail 'vector-select!:ten:3:4:8))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or (equal? (let ((v (vector)))
                (vector-separate! < v 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:empty:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:singleton:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 1)
                (vector-sort < (vector-copy v 0 1)))
              '#(19))
      (fail 'vector-separate!:singleton:1))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:ten:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 3)
                (vector-sort < (vector-copy v 0 3)))
              '#(3 8 9))
      (fail 'vector-separate!:ten:3))

  (or (equal? (let ((v (vector)))
                (vector-separate! < v 0 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:empty:0:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 0 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:singleton:0:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 1 0)
                (vector-sort < (vector-copy v 0 1)))
              '#(19))
      (fail 'vector-separate!:singleton:1:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 0 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:ten:0:0))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 3 0)
                (vector-sort < (vector-copy v 0 3)))
              '#(3 8 9))
      (fail 'vector-separate!:ten:3:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 0 1)
                (vector-sort < (vector-copy v 1 1)))
              '#())
      (fail 'vector-separate!:singleton:0:1))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 0 2)
                (vector-sort < (vector-copy v 2 2)))
              '#())
      (fail 'vector-separate!:ten:0:2))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 3 2)
                (vector-sort < (vector-copy v 2 5)))
              '#(3 9 13))
      (fail 'vector-separate!:ten:3:2))

  (or (equal? (let ((v (vector)))
                (vector-separate! < v 0 0 0)
                (vector-sort < (vector-copy v 0 0)))
              '#())
      (fail 'vector-separate!:empty:0:0:0))

  (or (equal? (let ((v (vector 19)))
                (vector-separate! < v 0 1 1)
                (vector-sort < (vector-copy v 1 1)))
              '#())
      (fail 'vector-separate!:singleton:0:1:1))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 0 2 8)
                (vector-sort < (vector-copy v 2 2)))
              '#())
      (fail 'vector-separate!:ten:0:2:8))

  (or (equal? (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
                (vector-separate! < v 3 2 8)
                (vector-sort < (vector-copy v 2 5)))
              '#(9 13 13))
      (fail 'vector-separate!:ten:3:2:8))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Sorting routines often have internal boundary cases or
  ;;; randomness, so it's prudent to run a lot of tests with
  ;;; different lengths.
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (all-sorts-okay? m n)
    (if (> m 0)
        (let* ((v (random-vector n))
               (v2 (vector-copy v))
               (lst (vector->list v))
               (ans (vector-sort < v2))
               (med (cond ((= n 0) -97)
                          ((odd? n)
                           (vector-ref ans (quotient n 2)))
                          (else
                           (/ (+ (vector-ref ans (- (quotient n 2) 1))
                                 (vector-ref ans (quotient n 2)))
                              2)))))
          (define (dsort vsort!)
            (let ((v2 (vector-copy v)))
              (vsort! < v2)
              v2))
          (and (equal? ans (list->vector (list-sort < lst)))
               (equal? ans (list->vector (list-stable-sort < lst)))
               (equal? ans (list->vector (list-sort! < (list-copy lst))))
               (equal? ans (list->vector (list-stable-sort! < (list-copy lst))))
               (equal? ans (vector-sort < v2))
               (equal? ans (vector-stable-sort < v2))
               (equal? ans (dsort vector-sort!))
               (equal? ans (dsort vector-stable-sort!))
               (equal? med (vector-find-median < v2 -97))
               (equal? v v2)
               (equal? lst (vector->list v))
               (equal? med (vector-find-median! < v2 -97))
               (equal? ans v2)
               (all-sorts-okay? (- m 1) n)))
        #t))

  (define (test-all-sorts m n)
    (test-true (format "test-all-sorts ~A ~A" m n) (all-sorts-okay? m n)))

  (for-each test-all-sorts
            '( 3  5 10 10 10 20 20 10 10 10 10 10  10  10  10  10  10)
            '( 0  1  2  3  4  5 10 20 30 40 50 99 100 101 499 500 501))

)
