#lang racket/base

;;; SRFI-1 for mutable lists

(require (only-in compatibility/mlist mlistof list->mlist [mlist? proper-mlist?] mlist mlist-ref mlength
                  mappend mappend! mreverse mreverse! mmemq mmemv massq massv)
         racket/contract racket/function racket/undefined
         (only-in srfi/1 car+cdr null-list? reduce reduce-right delete) (only-in "235.rkt" flip) "239.rkt")
(module+ test
  (require racket/local rackunit)

  (define-syntax test-values
    (syntax-rules (values)
      ((test-values name expr (values expected ...))
       (test-equal? name
                    (call-with-values (lambda () expr) list)
                    (list expected ...))))))

;;; Predicates needed for contracts
(define (dotted-mlist? x)
  (let lp ((x x) (lag x))
    (if (mpair? x)
	(let ((x (mcdr x)))
	  (if (mpair? x)
	      (let ((x   (mcdr x))
		    (lag (mcdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-mlist? x)
  (let lp ((x x) (lag x))
    (and (mpair? x)
         (let ((x (mcdr x)))
	   (and (mpair? x)
		(let ((x   (mcdr x))
		      (lag (mcdr lag)))
		  (or (eq? x lag) (lp x lag))))))))


(define finite-mlist/c (or/c proper-mlist? dotted-mlist?))
(define non-empty-finite-mlist/c (and/c (not/c null?) (or/c proper-mlist? dotted-mlist?)))
(define proper-or-circular-mlist/c (or/c proper-mlist? circular-mlist?))
(define mlist-or-null/c (or/c mpair? null?))
(define malist/c (mlistof mpair?))

(provide
 (all-from-out compatibility/mlist)
 (contract-out
  [dotted-mlist? predicate/c]
  [circular-mlist? predicate/c]
  [mxcons (-> any/c any/c mpair?)]
  [mcons* (-> any/c any/c ... any/c)]
  [make-mlist (->* (exact-nonnegative-integer?) (any/c) proper-mlist?)]
  [mlist-tabulate (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? any/c) proper-mlist?)]
  [mlist-copy (-> finite-mlist/c finite-mlist/c)]
  [miota (->* (exact-nonnegative-integer?) (real? real?) proper-mlist?)]
  [null-mlist? (-> proper-or-circular-mlist/c boolean?)]
  [not-mpair? predicate/c]
  [mlist= (-> (-> any/c any/c any/c) proper-mlist? ... boolean?)]
  [mfirst (-> proper-mlist? any/c)]
  [msecond (-> proper-mlist? any/c)]
  [mthird (-> proper-mlist? any/c)]
  [mfourth (-> proper-mlist? any/c)]
  [mfifth (-> proper-mlist? any/c)]
  [msixth (-> proper-mlist? any/c)]
  [mseventh (-> proper-mlist? any/c)]
  [meighth (-> proper-mlist? any/c)]
  [mninth (-> proper-mlist? any/c)]
  [mtenth (-> proper-mlist? any/c)]
  [mcar+mcdr (-> mpair? (values any/c any/c))]
  [mtake (-> mlist-or-null/c exact-nonnegative-integer? proper-mlist?)]
  [mtake! (-> mlist-or-null/c exact-nonnegative-integer? proper-mlist?)]
  [mdrop (-> mlist-or-null/c exact-nonnegative-integer? any/c)]
  [mtake-right (-> finite-mlist/c exact-nonnegative-integer? any/c)]
  [mdrop-right (-> finite-mlist/c exact-nonnegative-integer? proper-mlist?)]
  [mdrop-right! (-> finite-mlist/c exact-nonnegative-integer? proper-mlist?)]
  [msplit-at (-> mlist-or-null/c exact-nonnegative-integer? (values proper-mlist? any/c))]
  [msplit-at! (-> mlist-or-null/c exact-nonnegative-integer? (values proper-mlist? any/c))]
  [mlast (-> non-empty-finite-mlist/c any/c)]
  [mlast-pair (-> non-empty-finite-mlist/c mpair?)]
  [mlength+ (-> proper-or-circular-mlist/c (or/c exact-nonnegative-integer? #f))]
  [mconcatenate (-> (mlistof proper-mlist?) proper-mlist?)]
  [mconcatenate! (-> (mlistof proper-mlist?) proper-mlist?)]
  [mappend-reverse (-> proper-mlist? any/c any/c)]
  [mappend-reverse! (-> proper-mlist? any/c any/c)]
  [mzip (-> proper-or-circular-mlist/c proper-or-circular-mlist/c ... proper-mlist?)]
  [munzip1 (-> proper-mlist? proper-mlist?)]
  [munzip2 (-> proper-mlist? (values proper-mlist? proper-mlist?))]
  [munzip3 (-> proper-mlist? (values proper-mlist? proper-mlist? proper-mlist?))]
  [munzip4 (-> proper-mlist? (values proper-mlist? proper-mlist? proper-mlist? proper-mlist?))]
  [munzip5 (-> proper-mlist? (values proper-mlist? proper-mlist? proper-mlist? proper-mlist? proper-mlist?))]
  [mcount (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... exact-nonnegative-integer?)]
  [mfold (-> procedure? any/c proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mfold-right (-> procedure? any/c proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mpair-fold (-> procedure? any/c proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mpair-fold-right (-> procedure? any/c proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mreduce (-> procedure? any/c proper-mlist? any/c)]
  [mreduce-right (-> procedure? any/c proper-mlist? any/c)]
  [munfold (->* ((-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c) ((-> any/c any/c)) proper-mlist?)]
  [munfold-right (->* ((-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c) (any/c) proper-mlist?)]
  [mmap (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... proper-mlist?)]
  [mmap-in-order (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... proper-mlist?)]
  [mappend-map (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mappend-map! (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mmap! (-> procedure? proper-mlist? proper-or-circular-mlist/c ... proper-mlist?)]
  [mfor-each (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... void?)]
  [mpair-for-each (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... void?)]
  [mfilter-map (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... proper-mlist?)]
  [mfilter (-> (-> any/c any/c) proper-mlist? proper-mlist?)]
  [mfilter! (-> (-> any/c any/c) proper-mlist? proper-mlist?)]
  [mpartition (-> (-> any/c any/c) proper-mlist? (values proper-mlist? proper-mlist?))]
  [mpartition! (-> (-> any/c any/c) proper-mlist? (values proper-mlist? proper-mlist?))]
  [mremove (-> (-> any/c any/c) proper-mlist? proper-mlist?)]
  [mremove! (-> (-> any/c any/c) proper-mlist? proper-mlist?)]
  [mfind (-> (-> any/c any/c) proper-or-circular-mlist/c any/c)]
  [mfind-tail (-> (-> any/c any/c) proper-or-circular-mlist/c (or/c proper-or-circular-mlist/c #f))]
  [mtake-while (-> (-> any/c any/c) proper-or-circular-mlist/c proper-mlist?)]
  [mtake-while! (-> (-> any/c any/c) proper-or-circular-mlist/c proper-mlist?)]
  [mdrop-while (-> (-> any/c any/c) proper-or-circular-mlist/c proper-or-circular-mlist/c)]
  [mspan (-> (-> any/c any/c) proper-or-circular-mlist/c (values proper-mlist? proper-or-circular-mlist/c))]
  [mspan! (-> (-> any/c any/c) proper-mlist? (values proper-mlist? proper-mlist?))]
  [mbreak (-> (-> any/c any/c) proper-or-circular-mlist/c (values proper-mlist? proper-or-circular-mlist/c))]
  [mbreak! (-> (-> any/c any/c) proper-mlist? (values proper-mlist? proper-mlist?))]
  [many (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mevery (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... any/c)]
  [mlist-index (-> procedure? proper-or-circular-mlist/c proper-or-circular-mlist/c ... (or/c exact-nonnegative-integer? #f))]
  [mmember (->* (any/c proper-mlist?) ((-> any/c any/c any/c)) (or/c proper-mlist? #f))]
  [mdelete (->* (any/c proper-mlist?) ((-> any/c any/c any/c)) proper-mlist?)]
  [mdelete! (->* (any/c proper-mlist?) ((-> any/c any/c any/c)) proper-mlist?)]
  [mdelete-duplicates (->* (proper-mlist?) ((-> any/c any/c any/c)) proper-mlist?)]
  [mdelete-duplicates! (->* (proper-mlist?) ((-> any/c any/c any/c)) proper-mlist?)]
  [massoc (->* (any/c malist/c) ((-> any/c any/c any/c)) (or/c proper-mlist? #f))]
  [malist-cons (-> any/c any/c malist/c malist/c)]
  [malist-copy (-> malist/c malist/c)]
  [malist-delete (->* (any/c malist/c) ((-> any/c any/c any/c)) malist/c)]
  [malist-delete! (->* (any/c malist/c) ((-> any/c any/c any/c)) malist/c)]
  [mlset<= (-> (-> any/c any/c any/c) proper-mlist? ... boolean?)]
  [mlset= (-> (-> any/c any/c any/c) proper-mlist? ... boolean?)]
  [mlset-adjoin (-> (-> any/c any/c any/c) proper-mlist? any/c ... proper-mlist?)]
  [mlset-union (-> (-> any/c any/c any/c) proper-mlist? ... proper-mlist?)]
  [mlset-intersection (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... proper-mlist?)]
  [mlset-difference (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... proper-mlist?)]
  [mlset-xor (-> (-> any/c any/c any/c) proper-mlist? ... proper-mlist?)]
  [mlset-diff+intersection (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... (values proper-mlist? proper-mlist?))]
  [mlset-union! (-> (-> any/c any/c any/c) proper-mlist? ... proper-mlist?)]
  [mlset-intersection! (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... proper-mlist?)]
  [mlset-difference! (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... proper-mlist?)]
  [mlset-xor! (-> (-> any/c any/c any/c) proper-mlist? ... proper-mlist?)]
  [mlset-diff+intersection! (-> (-> any/c any/c any/c) proper-mlist? proper-mlist? ... (values proper-mlist? proper-mlist?))]
  ))

;;; Constructors
;; mcons - builtin
;; mlist - from c/m

(define (mxcons a b)
   (mcons b a))

(module+ test
  (test-equal? "mxcons" (mxcons (mlist 'b 'c) 'a) (mlist 'a 'b 'c)))

(define (mcons* first . args)
  (%mcons* (cons first args)))

(define (%mcons* args)
  (cond
    [(null? args) '()]
    [(null? (cdr args)) (car args)]
    [else (mcons (car args) (%mcons* (cdr args)))]))

(module+ test
  (test-equal? "mcons* 1" (mcons* 1 2 3 4) (mcons 1 (mcons 2 (mcons 3 4))))
  (test-equal? "mcons* 2" (mcons* 1) 1))

(define (make-mlist n [fill undefined])
  (let loop ([result '()]
             [n n])
    (if (zero? n)
        result
        (loop (mcons fill result) (sub1 n)))))

(module+ test
  (test-equal? "make-mlist" (make-mlist 4 'c) (mlist 'c 'c 'c 'c)))

(define (mlist-tabulate n init-proc)
  (let loop ([result '()]
             [n (sub1 n)])
    (if (= n -1)
        result
        (loop (mcons (init-proc n) result) (sub1 n)))))

(module+ test
  (test-equal? "mlist-tabulate" (mlist-tabulate 4 values) (mlist 0 1 2 3)))

(define (mlist-copy source)
  (cond
    [(null? source)
     '()]
    [(mpair? source)
     (if (mpair? (mcdr source))
         (mcons (mcar source) (mlist-copy (mcdr source)))
         (mcons (mcar source) (mcdr source)))]
    [else source]))

(define (circular-mlist element . elements)
  (define head (mcons element '()))
  (cond
    [(null? elements)
     (set-mcdr! head head)
     head]
    [else
     (let loop ([clist head]
                [elements elements])
       (list-case elements
         [()
          (begin
            (set-mcdr! clist head)
            head)]
         [(ecar . ecdr)
          (begin
            (set-mcdr! clist (mcons ecar '()))
            (loop (mcdr clist) ecdr))]))]))

(define (miota count [start 0] [step 1])
  (if (= count 0)
      '()
      (mcons start (miota (sub1 count) (+ start step) step))))

(module+ test
  (test-equal? "miota 1" (miota 5) (mlist 0 1 2 3 4))
  (test-case "miota 2" (check-within (miota 5 0 -0.1) (mlist 0 -0.1 -0.2 -0.3 -0.4) 0.0000001)))

;;; More predicates

(define (null-mlist? obj) (null? obj))

(define (not-mpair? obj) (not (mpair? obj)))

(define mlist=
  (case-lambda
    [(elt=) #t]
    [(elt= a) #t]
    [(elt= a b . rest)
     (and
      (or (eq? a b)
          (let loop ([a a]
                     [b b])
            (cond
              [(and (null? a) (null? b)) #t]
              [(or (null? a) (null? b)) #f]
              [(elt= (mcar a) (mcar b))
               (loop (mcdr a) (mcdr b))]
              [else #f])))
      (apply mlist= elt= a rest))]))

(module+ test
  (test-true "mlist= 1" (mlist= eq?))
  (test-true "mlist= 2" (mlist= eq? (mlist 'a)))
  (test-false "mlist= 3" (mlist= eq? (mlist 'a 'b) (mlist 'a)))
  (test-false "mlist= 4" (mlist= eq? (mlist 'a 'b) (mlist 'a 'b) (mlist 'a))))

;; mcaar, mcddr, etc. Why define each one by hand?
(require (for-syntax racket/base racket/list racket/set racket/syntax))
(begin-for-syntax
  (define (make-selector-combos)
    (set->list
     (for*/set ([len (in-inclusive-range 2 4)]
                [combos (in-combinations (append (make-list len #\a) (make-list len #\d)) len)]
                [perm (in-permutations combos)])
       perm))))

(define-syntax (make-selectors stx)
  (syntax-case stx ()
    [(_)
     (let* ([combos (make-selector-combos)]
            [names (map (lambda (combo) (format-id stx "mc~ar" (apply string combo))) combos)])
       #`(begin
           #,@(map (lambda (name combo)
                     #`(define #,name (compose1 #,@(map (lambda (sel) (if (char=? sel #\a) #'mcar #'mcdr)) combo))))
                   names combos)
           (provide #,@names)))]))

(make-selectors)

;; mlist-ref - from c/m

(define mfirst mcar)
(define msecond mcadr)
(define mthird mcaddr)
(define mfourth mcadddr)
(define mfifth (curryr mlist-ref 4))
(define msixth (curryr mlist-ref 5))
(define mseventh (curryr mlist-ref 6))
(define meighth (curryr mlist-ref 7))
(define mninth (curryr mlist-ref 8))
(define mtenth (curryr mlist-ref 9))

(module+ test
  (define tens (mlist 1 2 3 4 5 6 7 8 9 10 11 12))
  (test-equal? "mfirst" (mfirst tens) 1)
  (test-equal? "msecond" (msecond tens) 2)
  (test-equal? "mthird" (mthird tens) 3)
  (test-equal? "mfourth" (mfourth tens) 4)
  (test-equal? "mfifth" (mfifth tens) 5)
  (test-equal? "msixth" (msixth tens) 6)
  (test-equal? "mseventh" (mseventh tens) 7)
  (test-equal? "meighth" (meighth tens) 8)
  (test-equal? "mninth" (mninth tens) 9)
  (test-equal? "mtenth" (mtenth tens) 10))

(define (mcar+mcdr x) (values (mcar x) (mcdr x)))

(define (mtake lst n)
  (if (zero? n)
      '()
      (mcons (mcar lst) (mtake (mcdr lst) (sub1 n)))))

(define (mdrop lst n)
  (if (zero? n)
      lst
      (mdrop (mcdr lst) (sub1 n))))

(module+ test
  (test-equal? "mtake 1" (mtake (mlist 'a 'b 'c 'd 'e) 2) (mlist 'a 'b))
  (test-equal? "mtake 2" (mtake (mcons* 1 2 3 'd) 2) (mlist 1 2))
  (test-equal? "mtake 3" (mtake (mcons* 1 2 3 'd) 3) (mlist 1 2 3))
  (test-equal? "mdrop 1" (mdrop (mlist 'a 'b 'c 'd 'e) 2) (mlist 'c 'd 'e))
  (test-equal? "mdrop 2" (mdrop (mcons* 1 2 3 'd) 2) (mcons 3 'd))
  (test-equal? "mdrop 3" (mdrop (mcons* 1 2 3 'd) 3) 'd))

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.

(define (mtake-right lis k)
  (let lp ((lag lis)  (lead (mdrop lis k)))
    (if (mpair? lead)
	(lp (mcdr lag) (mcdr lead))
	lag)))

(define (mdrop-right lis k)
  (let recur ((lag lis) (lead (mdrop lis k)))
    (if (mpair? lead)
	(mcons (mcar lag) (recur (mcdr lag) (mcdr lead)))
	'())))

(module+ test
  (test-equal? "mtake-right 1" (mtake-right (mlist 'a 'b 'c 'd 'e) 2) (mlist 'd 'e))
  (test-equal? "mtake-right 2" (mtake-right (mcons* 1 2 3 'd) 2) (mcons* 2 3 'd))
  (test-equal? "mtake-right 3" (mtake-right (mcons* 1 2 3 'd) 0) 'd)
  (test-equal? "mdrop-right 1" (mdrop-right (mlist 'a 'b 'c 'd 'e) 2) (mlist 'a 'b 'c))
  (test-equal? "mdrop-right 2" (mdrop-right (mcons* 1 2 3 'd) 2) (mlist 1))
  (test-equal? "mdrop-right 3" (mdrop-right (mcons* 1 2 3 'd) 0) (mlist 1 2 3)))

(define (mtake! lis k)
  (cond
    [(zero? k)
     '()]
    [else
     (set-mcdr! (mdrop lis (- k 1)) '())
     lis]))

(module+ test
  (test-equal? "mtake! 1" (mtake! (mlist 'a 'b 'c 'd 'e) 2) (mlist 'a 'b))
  (test-equal? "mtake! 2" (mtake! (mcons* 1 2 3 'd) 2) (mlist 1 2))
  (test-equal? "mtake! 3" (mtake! (mcons* 1 2 3 'd) 3) (mlist 1 2 3)))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().
(define (mdrop-right! lis k)
  (let ((lead (mdrop lis k)))
    (if (mpair? lead)

	(let lp ((lag lis)  (lead (mcdr lead)))	; Standard case
          (cond
            [(mpair? lead)
             (lp (mcdr lag) (mcdr lead))]
            [else
             (set-mcdr! lag '())
             lis]))

	'())))	; Special case dropping everything -- no cons to side-effect.

(module+ test
  (test-equal? "mdrop-right! 1" (mdrop-right! (mlist 'a 'b 'c 'd 'e) 2) (mlist 'a 'b 'c))
  (test-equal? "mdrop-right! 2" (mdrop-right! (mcons* 1 2 3 'd) 2) (mlist 1))
  (test-equal? "mdrop-right! 3" (mdrop-right! (mcons* 1 2 3 'd) 0) (mlist 1 2 3)))

(define (msplit-at x k)
  (let recur ((lis x) (k k))
    (if (zero? k) (values '() lis)
        (let-values ([(prefix suffix) (recur (mcdr lis) (- k 1))])
          (values (mcons (mcar lis) prefix) suffix)))))

(module+ test
  (test-values "msplit-at 1" (msplit-at (mlist 'a 'b 'c 'd 'e 'f 'g 'h) 3)
               (values (mlist 'a 'b 'c) (mlist 'd 'e 'f 'g 'h))))

(define (msplit-at! x k)
  (if (zero? k) (values '() x)
      (let* ((prev (mdrop x (- k 1)))
	     (suffix (mcdr prev)))
	(set-mcdr! prev '())
	(values x suffix))))

(module+ test
  (test-values "msplit-at! 1" (msplit-at! (mlist 'a 'b 'c 'd 'e 'f 'g 'h) 3)
               (values (mlist 'a 'b 'c) (mlist 'd 'e 'f 'g 'h))))

(define (mlast lis) (mcar (mlast-pair lis)))
(define (mlast-pair lis)
  (let lp ((lis lis))
    (let ((tail (mcdr lis)))
      (if (mpair? tail) (lp tail) lis))))

(module+ test
  (test-equal? "mlast 1" (mlast (mlist 'a 'b 'c)) 'c)
  (test-equal? "mlast-pair 1" (mlast-pair (mlist 'a 'b 'c)) (mcons 'c '())))

;;; Miscellaneous

;; mlength - in c/m

(define (mlength+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (mpair? x)
	(let ((x (mcdr x))
	      (len (+ len 1)))
	  (if (mpair? x)
	      (let ((x   (mcdr x))
		    (lag (mcdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))

(module+ test
  (test-equal? "mlength+ 1" (mlength+ (mlist 1 2 3 4)) 4)
  (test-equal? "mlength+ 2" (mlength+ (circular-mlist 1 2 3 4)) #f))

;; mappend, mappend! - from c/m

(define (mconcatenate  lists) (mreduce-right mappend  '() lists))
(define (mconcatenate! lists) (mreduce-right mappend! '() lists))

;; mreverse, mreverse! - from c/m

(define (mappend-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-mlist? rev-head) tail
	(lp (mcdr rev-head) (mcons (mcar rev-head) tail)))))

(define (mappend-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-mlist? rev-head) tail
	(let ((next-rev (mcdr rev-head)))
	  (set-mcdr! rev-head tail)
	  (lp next-rev rev-head)))))

(module+ test
  (test-equal? "mappend-reverse 1" (mappend-reverse (mlist 'c 'b 'a) (mlist 'd)) (mlist 'a 'b 'c 'd))
  (test-equal? "mappend-reverse 2" (mappend-reverse (mlist 'c 'b 'a) 'd) (mcons* 'a 'b 'c 'd))
  (test-equal? "mappend-reverse! 1" (mappend-reverse! (mlist 'c 'b 'a) (mlist 'd)) (mlist 'a 'b 'c 'd))
  (test-equal? "mappend-reverse! 2" (mappend-reverse! (mlist 'c 'b 'a) 'd) (mcons* 'a 'b 'c 'd)))

(define (mzip clist1 . clists)
  (apply mmap mlist (cons clist1 clists)))

;;; Unzippers -- 1 through 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (munzip1 lis) (mmap mcar lis))

(define (munzip2 lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values lis lis)	; Use NOT-PAIR? to handle
	(let ((elt (mcar lis)))			; dotted lists.
	  (let-values ([(a b) (recur (mcdr lis))])
            (values (mcons (mcar  elt) a)
		    (mcons (mcadr elt) b)))))))

(define (munzip3 lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values lis lis lis)
	(let ((elt (mcar lis)))
	  (let-values ([(a b c) (recur (mcdr lis))])
	    (values (mcons (mcar   elt) a)
		    (mcons (mcadr  elt) b)
		    (mcons (mcaddr elt) c)))))))

(define (munzip4 lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values lis lis lis lis)
	(let ((elt (mcar lis)))
	  (let-values ([(a b c d) (recur (mcdr lis))])
	    (values (mcons (mcar    elt) a)
		    (mcons (mcadr   elt) b)
		    (mcons (mcaddr  elt) c)
		    (mcons (mcadddr elt) d)))))))

(define (munzip5 lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values lis lis lis lis lis)
	(let ((elt (mcar lis)))
	  (let-values ([(a b c d e) (recur (mcdr lis))])
	    (values (mcons (mcar     elt) a)
		    (mcons (mcadr    elt) b)
		    (mcons (mcaddr   elt) c)
		    (mcons (mcadddr  elt) d)
		    (mcons (mcar (mcddddr  elt)) e)))))))

(module+ test
  (test-values "munzip2" (munzip2 (mlist (mlist 1 'one) (mlist 2 'two) (mlist 3 'three)))
               (values (mlist 1 2 3) (mlist 'one 'two 'three))))


;;; count
;;;;;;;;;
(define (mcount pred list1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((list1 list1) (lists lists) (i 0))
	(if (null-mlist? list1) i
	    (let-values ([(as ds) (%cars+cdrs lists)])
	      (if (null? as) i
		  (lp (mcdr list1) ds
		      (if (apply pred (mcar list1) as) (+ i 1) i))))))

      ;; Fast path
      (let lp ((lis list1) (i 0))
	(if (null-mlist? lis) i
	    (lp (mcdr lis) (if (pred (mcar lis)) (+ i 1) i))))))

(module+ test
  (test-equal? "mcount 1" (mcount even? (mlist 3 1 4 1 5 9 2 5 6)) 3)
  (test-equal? "mcount 2" (mcount < (mlist 1 2 4 8) (mlist 2 4 6 8 10 12 14 16)) 3)
  (test-equal? "mcount 3" (mcount < (mlist 3 1 4 1) (circular-mlist 1 10)) 2))

;;; Fold, unfold & map

;;; Fold/map internal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These little internal utilities are used by the general
;;; fold & mapper funs for the n-ary cases . It'd be nice if they got inlined.
;;; One the other hand, the n-ary cases are painfully inefficient as it is.
;;; An aggressive implementation should simply re-write these functions
;;; for raw efficiency; I have written them for as much clarity, portability,
;;; and simplicity as can be achieved.
;;;
;;; I use the dreaded call/cc to do local aborts. A good compiler could
;;; handle this with extreme efficiency. An implementation that provides
;;; a one-shot, non-persistent continuation grabber could help the compiler
;;; out by using that in place of the call/cc's in these routines.
;;;
;;; These functions have funky definitions that are precisely tuned to
;;; the needs of the fold/map procs -- for example, to minimize the number
;;; of times the argument lists need to be examined.

;;; Return (map cdr lists).
;;; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs lists)
  (let/ec abort
    (let recur ((lists lists))
      (list-case lists
        ((lis . lcdr)
         (if (null-mlist? lis) (abort '())
             (cons (mcdr lis) (recur lcdr))))
        (() '())))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (list-case lists
      ((lcar . lcdr)
       (cons (mcar lcar) (recur lcdr)))
      (()
       (list last-elt)))))

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].

;; not sure which version to prefer; escape continuation or delimited continuation?
(define (%cars+cdrs lists)
  (let/ec abort
    (let recur ((lists lists))
      (list-case lists
          [(list . other-lists)
           (if (null-mlist? list)
               (abort '() '()) ; LIST is empty -- bail out
               (let-values ([(a d) (mcar+mcdr list)]
                            [(cars cdrs) (recur other-lists)])
                 (values (cons a cars) (cons d cdrs))))]
        [() (values '() '())]))))

#|
(require racket/control)
(define (%cars+cdrs lists)
  (reset
   (let recur ((lists lists))
     (if (pair? lists)
         (let-values ([(list other-lists) (car+cdr lists)])
                  (if (null-mlist? list) (shift _ (values '() '())) ; LIST is empty -- bail out
                      (let-values ([(a d) (mcar+mcdr list)]
                                   [(cars cdrs) (recur other-lists)])
                         (values (cons a cars) (cons d cdrs)))))
          (values '() '())))))
|#

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list. What a hack.
(define (%cars+cdrs+ lists cars-final)
  (let/ec abort
    (let recur ((lists lists))
      (list-case lists
        [(list . other-lists)
         (if (null-mlist? list)
             (abort '() '()) ; LIST is empty -- bail out
             (let-values ([(a d) (mcar+mcdr list)]
                          [(cars cdrs) (recur other-lists)])
               (values (cons a cars) (cons d cdrs))))]
          [() (values (list cars-final) '())]))))

;;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (list-case lists
    [(list . other-lists)
     (let-values ([(a d) (mcar+mcdr list)]
                  [(cars cdrs) (recur other-lists)])
       (values (cons a cars) (cons d cdrs)))]
      [() (values '() '())])))

;;; fold/unfold
;;;;;;;;;;;;;;;

(define (munfold-right p f g seed [maybe-tail '()])
  (let lp ((seed seed) (ans maybe-tail))
    (if (p seed) ans
	(lp (g seed)
	    (mcons (f seed) ans)))))

(module+ test
  (test-equal? "munfold-right 1"
               (munfold-right zero?
                              (lambda (x) (* x x))
                              (lambda (x) (- x 1))
                              10)
               (mlist 1 4 9 16 25 36 49 64 81 100)))

(define (munfold p f g seed [tail-gen (lambda (_) '())])
  (let recur ((seed seed))
    (if (p seed) (tail-gen seed)
        (mcons (f seed) (recur (g seed))))))

(module+ test
  (test-equal? "munfold 1"
                (munfold (lambda (x) (> x 10))
                         (lambda (x) (* x x))
                         (lambda (x) (+ x 1))
                         1)
                (mlist 1 4 9 16 25 36 49 64 81 100)))

(define (mfold kons knil lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(let-values ([(cars+ans cdrs) (%cars+cdrs+ lists ans)])
	  (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))

      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-mlist? lis) ans
	    (lp (mcdr lis) (kons (mcar lis) ans))))))


(define (mfold-right kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-mlist? lis) knil
	    (let ((head (mcar lis)))
	      (kons head (recur (mcdr lis))))))))


(define (mpair-fold-right f zero lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) zero
	      (apply f (append lists (list (recur cdrs)))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-mlist? lis) zero (f lis (recur (mcdr lis)))))))

(define (mpair-fold f zero lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))	; N-ary case
	(let ((tails (%cdrs lists)))
	  (if (null? tails) ans
	      (lp tails (apply f (append lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
	(if (null-mlist? lis) ans
	    (let ((tail (mcdr lis)))		; Grab the cdr now,
	      (lp tail (f lis ans)))))))	; in case F SET-CDR!s LIS.


;;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;;; These cannot meaningfully be n-ary.

(define (mreduce f ridentity lis)
  (if (null-mlist? lis)
      ridentity
      (mfold f (mcar lis) (mcdr lis))))

(define (mreduce-right f ridentity lis)
  (if (null-mlist? lis)
      ridentity
      (let recur ((head (mcar lis))
                  (lis (mcdr lis)))
        (if (mpair? lis)
            (f head (recur (mcar lis) (mcdr lis)))
            head))))

;;; Mappers: append-map append-map! pair-for-each map! filter-map map-in-order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mappend-map f lis1 . lists)
  (really-append-map mappend  f lis1 lists))
(define (mappend-map! f lis1 . lists)
  (really-append-map mappend! f lis1 lists))

(define (really-append-map appender f lis1 lists)
  (if (pair? lists)
      (let-values ([(cars cdrs) (%cars+cdrs (cons lis1 lists))])
	(if (null? cars)
            '()
	    (let recur ((cars cars)
                        (cdrs cdrs))
	      (let ((vals (apply f cars)))
		(let-values ([(cars2 cdrs2) (%cars+cdrs cdrs)])
                  (if (null? cars2)
                      vals
                      (appender vals (recur cars2 cdrs2))))))))

      ;; Fast path
      (if (null-mlist? lis1) '()
	  (let recur ((elt (mcar lis1))
                      (rest (mcdr lis1)))
	    (let ((vals (f elt)))
	      (if (null-mlist? rest) vals
		  (appender vals (recur (mcar rest) (mcdr rest)))))))))

;;; Map F across lists, guaranteeing to go left-to-right.
;;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;;; in which case this procedure may simply be defined as a synonym for MAP.

(define (mmap-in-order f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (let-values ([(cars cdrs) (%cars+cdrs lists)])
          (if (pair? cars)
              (let ((x (apply f cars)))		; Do head first,
                (mcons x (recur cdrs)))		; then tail.
              '())))

      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-mlist? lis) lis
	    (let ((tail (mcdr lis))
		  (x (f (mcar lis))))		; Do head first,
	      (mcons x (recur tail)))))))	; then tail.


;;; We extend MAP to handle arguments of unequal length.
(define mmap mmap-in-order)

;;; Map F across L, and save up all the non-false results.
(define (mfilter-map f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(let-values ([(cars cdrs) (%cars+cdrs lists)])
          (if (pair? cars)
              (cond
                ((apply f cars) => (lambda (x) (mcons x (recur cdrs))))
                (else (recur cdrs))) ; Tail call in this arm.
              '())))

      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-mlist? lis)
            lis
	    (let ((tail (recur (mcdr lis))))
	      (cond
                ((f (mcar lis)) => (lambda (x) (mcons x tail)))
                (else tail)))))))

(module+ test
  (test-equal? "mfilter-map 1"
               (mfilter-map (lambda (x) (and (number? x) (* x x))) (mlist 'a 1 'b 3 'c 7))
               (mlist 1 9 49)))

(define (mfor-each f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (let-values ([(cars cdrs) (%cars+cdrs lists)])
          (cond
            [(pair? cars)
             (apply f cars)
             (recur cdrs)]
            [else (void)])))

      ;; Fast path.
      (let recur ((lis lis1))
	(cond
          [(null-mlist? lis)
           (void)]
          [else
           (define tail (mcdr lis))
           (f (mcar lis))
           (recur tail)]))))

(module+ test
  (test-equal? "mfor-each 1"
               (let ((v (make-vector 5)))
                 (mfor-each (lambda (i)
                              (vector-set! v i (* i i)))
                            (mlist 0 1 2 3 4))
                 v)
               #(0 1 4 9 16))
  (test-equal? "mfor-each 2"
               (let ((v (make-vector 5)))
                 (mfor-each (lambda (i j)
                              (vector-set! v i j))
                            (mlist 0 1 2 3 4)
                            (mlist 'a 'b 'c 'd))
                 v)
               '#(a b c d 0)))

(define (mpair-for-each proc lis1 . lists)
  (if (pair? lists)

      (let lp ((lists (cons lis1 lists)))
	(let ((tails (%cdrs lists)))
	  (when (pair? tails)
            (apply proc lists)
            (lp tails))))

      ;; Fast path.
      (let lp ((lis lis1))
	(when (not (null-mlist? lis))
	    (let ((tail (mcdr lis)))	; Grab the cdr now,
	      (proc lis)		; in case PROC SET-CDR!s LIS.
	      (lp tail))))))

;;; We stop when LIS1 runs out, not when any list runs out.
(define (mmap! f lis1 . lists)
  (if (pair? lists)
      (let lp ((lis1 lis1)
               (lists lists))
	(when (not (null-mlist? lis1))
	    (let-values ([(heads tails) (%cars+cdrs/no-test lists)])
              (set-mcar! lis1 (apply f (mcar lis1) heads))
	      (lp (mcdr lis1) tails))))

      ;; Fast path.
      (mpair-for-each (lambda (pair) (set-mcar! pair (f (mcar pair)))) lis1))
  lis1)

(module+ test
  (test-equal? "mmap! 1" (mmap! add1 (mlist 1 2 3 4)) (mlist 2 3 4 5))
  (test-equal? "mmap! 2" (mmap + (mlist 1 2 3 4) (circular-mlist 2)) (mlist 3 4 5 6)))

;;; Filtering and partitioning

;;; filter, remove, partition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
;;; disorder the elements of their argument.

;; This FILTER shares the longest tail of L that has no deleted elements.
;; If Scheme had multi-continuation calls, they could be made more efficient.

(define (mfilter pred lis)			; Sleazing with EQ? makes this one faster.
  (let recur ((lis lis))
    (if (null-mlist? lis) lis			; Use NOT-PAIR? to handle dotted lists.
	(let ((head (mcar lis))
	      (tail (mcdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (mcons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.

;;; This implementation of FILTER!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice the tail of one run of ins to the
;;; beginning of the next.

(define (mfilter! pred lis)
  (let lp ((ans lis))
    (cond ((null-mlist? ans)       ans)			; Scan looking for
	  ((not (pred (mcar ans))) (lp (mcdr ans)))	; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (when (mpair? lis)
					(if (pred (mcar lis))
					    (scan-in lis (mcdr lis))
					    (scan-out prev (mcdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (mpair? lis)
					   (if (pred (mcar lis))
					       (begin (set-mcdr! prev lis)
						      (scan-in lis (mcdr lis)))
					       (lp (mcdr lis)))
					   (set-mcdr! prev lis))))))
		  (scan-in ans (mcdr ans))
		  ans)))))

(module+ test
  (test-equal? "mfilter 1" (mfilter even? (mlist 0 7 8 8 43 -4)) (mlist 0 8 8 -4))
  (test-equal? "mfilter! 1" (mfilter! even? (mlist 0 7 8 8 43 -4)) (mlist 0 8 8 -4)))

;;; Answers share common tail with LIS where possible;
;;; the technique is slightly subtle.

(define (mpartition pred lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values lis lis)	; Use NOT-PAIR? to handle dotted lists.
	(let ((elt (mcar lis))
	      (tail (mcdr lis)))
	  (let-values ([(in out) (recur tail)])
	    (if (pred elt)
		(values (if (mpair? out) (mcons elt in) lis) out)
		(values in (if (mpair? in) (mcons elt out) lis))))))))

;;; This implementation of PARTITION!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice these runs together into the result
;;; lists.

(define (mpartition! pred lis)
  (if (null-mlist? lis) (values lis lis)

      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
			  (let lp ((in-prev in-prev) (lis lis))
			    (if (mpair? lis)
				(if (pred (mcar lis))
				    (lp lis (mcdr lis))
				    (begin (set-mcdr! out-prev lis)
					   (scan-out in-prev lis (mcdr lis))))
				(set-mcdr! out-prev lis))))) ; Done.

	       (scan-out (lambda (in-prev out-prev lis)
			   (let lp ((out-prev out-prev) (lis lis))
			     (if (mpair? lis)
				 (if (pred (mcar lis))
				     (begin (set-mcdr! in-prev lis)
					    (scan-in lis out-prev (mcdr lis)))
				     (lp lis (mcdr lis)))
				 (set-mcdr! in-prev lis)))))) ; Done.

	;; Crank up the scan&splice loops.
	(if (pred (mcar lis))
	    ;; LIS begins in-list. Search for out-list's first pair.
	    (let lp ((prev-l lis) (l (mcdr lis)))
	      (cond ((not (mpair? l)) (values lis l))
		    ((pred (mcar l)) (lp l (mcdr l)))
		    (else (scan-out prev-l l (mcdr l))
			  (values lis l))))	; Done.

	    ;; LIS begins out-list. Search for in-list's first pair.
	    (let lp ((prev-l lis) (l (mcdr lis)))
	      (cond ((not (mpair? l)) (values l lis))
		    ((pred (mcar l))
		     (scan-in l prev-l (mcdr l))
		     (values l lis))		; Done.
		    (else (lp l (mcdr l)))))))))

;;; Inline us, please.
(define (mremove  pred l) (mfilter  (negate pred) l))
(define (mremove! pred l) (mfilter! (negate pred) l))

(module+ test
  (test-equal? "mfilter 1" (mfilter even? (mlist 0 7 8 8 43 -4)) (mlist 0 8 8 -4))
  (test-equal? "mfilter! 1" (mfilter! even? (mlist 0 7 8 8 43 -4)) (mlist 0 8 8 -4))
  (test-values "mpartition 1" (mpartition symbol? (mlist 'one 2 3 'four 'five 6))
               (values (mlist 'one 'four 'five) (mlist 2 3 6)))
  (test-values "mpartition! 1" (mpartition! symbol? (mlist 'one 2 3 'four 'five 6))
               (values (mlist 'one 'four 'five) (mlist 2 3 6))))


;;; Searching

(define (mfind pred lst)
  (for/first ([elem (in-mlist lst)]
              #:when (pred elem))
    elem))

(define (mfind-tail pred lst)
  (cond
    [(null-mlist? lst)
     #f]
    [(pred (mcar lst))
     lst]
    [else (mfind-tail pred (mcdr lst))]))

(module+ test
  (test-equal? "mfind 1" (mfind even? (mlist 3 1 4 1 5 9)) 4)
  (test-equal? "mfind-tail 1" (mfind-tail even? (mlist 3 1 37 -8 -5 0 0)) (mlist -8 -5 0 0))
  (test-equal? "mfind-tail 2" (mfind-tail even? (mlist 3 1 37 -5)) #f))

(define (mtake-while pred lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) '()
	(let ((x (mcar lis)))
	  (if (pred x)
	      (mcons x (recur (mcdr lis)))
	      '())))))

(define (mtake-while! pred lis)
  (if (or (null-mlist? lis) (not (pred (mcar lis)))) '()
      (begin (let lp ((prev lis) (rest (mcdr lis)))
	       (when (mpair? rest)
		   (let ((x (mcar rest)))
		     (if (pred x) (lp rest (mcdr rest))
			 (set-mcdr! prev '())))))
	     lis)))

(define (mdrop-while pred lis)
  (let lp ((lis lis))
    (if (null-mlist? lis) '()
	(if (pred (mcar lis))
	    (lp (mcdr lis))
	    lis))))

(module+ test
  (test-equal? "mtake-while 1" (mtake-while even? (mlist 2 18 3 10 22 9)) (mlist 2 18))
  (test-equal? "mtake-while! 1" (mtake-while! even? (mlist 2 18 3 10 22 9)) (mlist 2 18))
  (test-equal? "mdrop-while 1" (mdrop-while even? (mlist 2 18 3 10 22 9)) (mlist 3 10 22 9)))

(define (mspan pred lis)
  (let recur ((lis lis))
    (if (null-mlist? lis) (values '() '())
	(let ((x (mcar lis)))
	  (if (pred x)
	      (let-values ([(prefix suffix) (recur (mcdr lis))])
                                (values (mcons x prefix) suffix))
	      (values '() lis))))))

(define (mspan! pred lis)
  (if (or (null-mlist? lis) (not (pred (mcar lis)))) (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (mcdr lis)))
		      (if (null-mlist? rest) rest
			  (let ((x (mcar rest)))
			    (if (pred x) (lp rest (mcdr rest))
				(begin (set-mcdr! prev '())
				       rest)))))))
	(values lis suffix))))


(define (mbreak  pred lis) (mspan  (negate pred) lis))
(define (mbreak! pred lis) (mspan! (negate pred) lis))

(module+ test
  (test-values "mspan 1" (mspan even? (mlist 2 18 3 10 22 9))
               (values (mlist 2 18) (mlist 3 10 22 9)))
  (test-values "mspan! 1" (mspan! even? (mlist 2 18 3 10 22 9))
               (values (mlist 2 18) (mlist 3 10 22 9)))
  (test-values "mbreak 1" (mbreak even? (mlist 3 1 4 1 5 9))
               (values (mlist 3 1) (mlist 4 1 5 9)))
  (test-values "mbreak! 1" (mbreak! even? (mlist 3 1 4 1 5 9))
               (values (mlist 3 1) (mlist 4 1 5 9))))

(define (many pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let-values ([(heads tails) (%cars+cdrs (cons lis1 lists))])
	(and (pair? heads)
	     (let lp ((heads heads) (tails tails))
	       (let-values ([(next-heads next-tails) (%cars+cdrs tails)])
		 (if (pair? next-heads)
		     (or (apply pred heads) (lp next-heads next-tails))
		     (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (and (not (null-mlist? lis1))
	   (let lp ((head (mcar lis1)) (tail (mcdr lis1)))
	     (if (null-mlist? tail)
		 (pred head)		; Last PRED app is tail call.
		 (or (pred head) (lp (mcar tail) (mcdr tail))))))))

(define (mevery pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let-values ([(heads tails) (%cars+cdrs (cons lis1 lists))])
	(or (not (pair? heads))
	    (let lp ((heads heads) (tails tails))
	      (let-values ([(next-heads next-tails) (%cars+cdrs tails)])
		(if (pair? next-heads)
		    (and (apply pred heads) (lp next-heads next-tails))
		    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-mlist? lis1)
	  (let lp ((head (mcar lis1))  (tail (mcdr lis1)))
	    (if (null-mlist? tail)
		(pred head)	; Last PRED app is tail call.
		(and (pred head) (lp (mcar tail) (mcdr tail))))))))

(module+ test
  (test-true "many 1" (many integer? (mlist 'a 3 'b 2.7)))
  (test-false "many 2" (many integer? (mlist 'a 3.1 'b 2.7)))
  (test-true "many 3" (many < (mlist 3 1 4 1 5) (mlist 2 7 1 8 2)))
  (test-true "mevery 1" (mevery integer? (mlist 1 2 3 4)))
  (test-false "mevery 2" (mevery integer? (mlist 1 2 3.14 4)))
  (test-true "mevery 3" (mevery < (mlist 1 2 3 4) (mlist 2 3 4 5)))
  (test-false "mevery 4" (mevery < (mlist 1 2 3 4) (mlist 2 3 4 1))))

  
(define (mlist-index pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((lists (cons lis1 lists)) (n 0))
	(let-values ([(heads tails) (%cars+cdrs lists)])
	  (and (pair? heads)
	       (if (apply pred heads) n
		   (lp tails (+ n 1))))))

      ;; Fast path
      (let lp ((lis lis1) (n 0))
	(and (not (null-mlist? lis))
	     (if (pred (mcar lis)) n (lp (mcdr lis) (+ n 1)))))))

(module+ test
  (test-equal? "mlist-index 1" (mlist-index even? (mlist 3 1 4 1 5 9)) 2)
  (test-equal? "mlist-index 2" (mlist-index < (mlist 3 1 4 1 5 9 2 5 6) (mlist 2 7 1 8 2)) 1)
  (test-false "mlist-index 3" (mlist-index = (mlist 3 1 4 1 5 9 2 5 6) (mlist 2 7 1 8 2))))

(define (mmember x lst [= equal?])
  (mfind-tail (curry = x) lst))

(define (mdelete x lis [= equal?])
  (mfilter (lambda (y) (not (= x y))) lis))

(define (mdelete! x lis [= equal?])
  (mfilter! (lambda (y) (not (= x y))) lis))

;;; right-duplicate deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete-duplicates delete-duplicates!
;;;
;;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;;; in long lists, sort the list to bring duplicates together, then use a
;;; linear-time algorithm to kill the dups. Or use an algorithm based on
;;; element-marking. The former gives you O(n lg n), the latter is linear.

(define (mdelete-duplicates lis [elt= equal?])
  (let recur ((lis lis))
    (if (null-mlist? lis) lis
        (let* ((x (mcar lis))
               (tail (mcdr lis))
               (new-tail (recur (mdelete x tail elt=))))
          (if (eq? tail new-tail) lis (mcons x new-tail))))))

(define (mdelete-duplicates! lis [elt= equal?])
  (let recur ((lis lis))
    (if (null-mlist? lis) lis
        (let* ((x (mcar lis))
               (tail (mcdr lis))
               (new-tail (recur (mdelete! x tail elt=))))
          (if (eq? tail new-tail) lis (mcons x new-tail))))))

(module+ test
  (test-equal? "mdelete-duplicates 1" (mdelete-duplicates (mlist 'a 'b 'a 'c 'a 'b 'c 'z)) (mlist 'a 'b 'c 'z))
  (test-equal? "mdelete-duplicates! 1" (mdelete-duplicates! (mlist 'a 'b 'a 'c 'a 'b 'c 'z)) (mlist 'a 'b 'c 'z))
  (test-equal? "mdelete-duplicates 2" (mdelete-duplicates (mlist '(a . 3) '(b . 7) '(a . 9) '(c . 1))
                                                          (lambda (x y) (eq? (car x) (car y))))
               (mlist '(a . 3) '(b . 7) '(c . 1)))
  (test-equal? "mdelete-duplicates! 2" (mdelete-duplicates! (mlist '(a . 3) '(b . 7) '(a . 9) '(c . 1))
                                                            (lambda (x y) (eq? (car x) (car y))))
               (mlist '(a . 3) '(b . 7) '(c . 1))))

(define (massoc x lst [= equal?])
  (mfind (lambda (elt) (= x (mcar elt))) lst))

(module+ test
  (local
    [(define e (mlist (mlist 'a 1) (mlist 'b 2) (mlist 'c 3)))]
    (test-equal? "massq 1" (massq 'a e) (mlist 'a 1))
    (test-equal? "massq 2" (massq 'b e) (mlist 'b 2))
    (test-false "massq 3" (massq 'd e))
    (test-false "massq 4" (massq (mlist 'a) (mlist (mlist (mlist 'a)) (mlist (mlist 'b)) (mlist (mlist 'c)))))
    (test-equal? "massoc 1" (massoc (mlist 'a) (mlist (mlist (mlist 'a)) (mlist (mlist 'b)) (mlist (mlist 'c)))) (mlist (mlist 'a)))
    (test-equal? "massv 1" (massv 5 (mlist (mlist 2 3) (mlist 5 7) (mlist 11 13))) (mlist 5 7))))


(define (malist-cons key value malist)
  (mcons (mcons key value) malist))

(define (malist-copy malist)
  (mmap (lambda (elem) (mcons (mcar elem) (mcdr elem))) malist))

(define (malist-delete key malist [= equal?])
  (mfilter (lambda (elem) (not (= key (mcar elem)))) malist))

(define (malist-delete! key malist [= equal?])
  (mfilter! (lambda (elem) (not (= key (mcar elem)))) malist))

;;; Lists-as-sets
;;;;;;;;;;;;;;;;;

;;; This is carefully tuned code; do not modify casually.
;;; - It is careful to share storage when possible;
;;; - Side-effecting code tries not to perform redundant writes.
;;; - It tries to avoid linear-time scans in special cases where constant-time
;;;   computations can be performed.
;;; - It relies on similar properties from the other list-lib procs it calls.
;;;   For example, it uses the fact that the implementations of MEMBER and
;;;   FILTER in this source code share longest common tails between args
;;;   and results to get structure sharing in the lset procedures.

(define (%lset2<= = lis1 lis2) (mevery (lambda (x) (mmember x lis2 =)) lis1))

(define (mlset<= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)	; Fast path
		       (%lset2<= = s1 s2)) ; Real test
		   (lp s2 rest)))))))

(define (mlset= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
        (or (not (pair? rest))
            (let ((s2   (car rest))
                  (rest (cdr rest)))
              (and (or (eq? s1 s2)            ; Fast path
                       (and (%lset2<= = s1 s2) ; Real test
                            (%lset2<= (flip =) s2 s1)))
                   (lp s2 rest)))))))

(define (mlset-adjoin = lis . elts)
  (foldl (lambda (elt ans) (if (mmember elt ans =) ans (mcons elt ans)))
	lis elts))


(define (mlset-union = . lists)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis)	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (mfold (lambda (elt ans) (if (many (lambda (x) (= x elt)) ans)
                                                ans
                                                (mcons elt ans)))
                          ans lis))))
	  '() lists))

(define (mlset-union! = . lists)
  (reduce (lambda (lis ans)		; Splice new elts of LIS onto the front of ANS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis)	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (mpair-fold (lambda (pair ans)
				(let ((elt (mcar pair)))
				  (if (many (lambda (x) (= x elt)) ans)
				      ans
				      (begin (set-mcdr! pair ans) pair))))
                               ans lis))))
          '() lists))


(define (mlset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((ormap null-mlist? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (mfilter (lambda (x)
			  (andmap (lambda (lis) (mmember x lis =)) lists))
			lis1)))))

(define (mlset-intersection! = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((ormap null-mlist? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (mfilter! (lambda (x)
			   (andmap (lambda (lis) (mmember x lis =)) lists))
			 lis1)))))


(define (mlset-difference = lis1 . lists)
  (let ((lists (filter mpair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (mfilter (lambda (x)
			  (andmap (lambda (lis) (not (mmember x lis =)))
				 lists))
			lis1)))))

(define (mlset-difference! = lis1 . lists)
  (let ((lists (filter mpair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (mfilter! (lambda (x)
			   (andmap (lambda (lis) (not (mmember x lis =)))
				  lists))
			 lis1)))))


(define (mlset-xor = . lists)
  (reduce-right (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (let-values ([(a-b a-int-b)   (mlset-diff+intersection = a b)])
	      (cond ((null? a-b)     (mlset-difference = b a))
		    ((null? a-int-b) (mappend b a))
		    (else (mfold (lambda (xb ans)
				  (if (mmember xb a-int-b =) ans (mcons xb ans)))
				a-b
				b)))))
	  '() lists))


(define (mlset-xor! = . lists)
  (reduce-right (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (let-values ([(a-b a-int-b)   (mlset-diff+intersection! = a b)])
	      (cond ((null? a-b)     (mlset-difference! = b a))
		    ((null? a-int-b) (mappend! b a))
		    (else (mpair-fold (lambda (b-pair ans)
				       (if (mmember (mcar b-pair) a-int-b =) ans
					   (begin (set-mcdr! b-pair ans) b-pair)))
				     a-b
				     b)))))
	  '() lists))


(define (mlset-diff+intersection = lis1 . lists)
  (cond ((andmap null-mlist? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (mpartition (lambda (elt)
			   (not (ormap (lambda (lis) (mmember elt lis =))
				     lists)))
			 lis1))))

(define (mlset-diff+intersection! = lis1 . lists)
  (cond ((andmap null-mlist? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (mpartition! (lambda (elt)
			    (not (ormap (lambda (lis) (mmember elt lis =))
				      lists)))
			  lis1))))

(module+ test
  (test-true "mlset<= 1" (mlset<= eq? (mlist 'a) (mlist 'a 'b 'a) (mlist 'a 'b 'c 'c)))
  (test-true "mlset<= 2" (mlset<= eq?))
  (test-true "mlset<= 3" (mlset<= eq? (mlist 'a)))

  (test-true "mlset= 1" (mlset= eq? (mlist 'b 'e 'a) (mlist 'a 'e 'b) (mlist 'e 'e 'b 'a)))
  (test-true "mlset= 2" (mlset= eq?))
  (test-true "mlset= 3" (mlset= eq? (mlist 'a)))

  (test-equal? "mlset-adjoin 1" (mlset-adjoin eq? (mlist 'a 'b 'c 'd 'c 'e) 'a 'e 'i 'o 'u)
               (mlist 'u 'o 'i 'a 'b 'c 'd 'c 'e))

  (test-equal? "mlset-union 1" (mlset-union eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'u 'o 'i 'a 'b 'c 'd 'e))
  (test-equal? "mlset-union 2" (mlset-union eq? (mlist 'a 'a 'c) (mlist 'x 'a 'x)) (mlist 'x 'a 'a 'c))
  (test-equal? "mlset-union 3" (mlset-union eq?) '())
  (test-equal? "mlset-union 4" (mlset-union eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))
  (test-equal? "mlset-union! 1" (mlset-union! eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'u 'o 'i 'a 'b 'c 'd 'e))
  (test-equal? "mlset-union! 2" (mlset-union! eq? (mlist 'a 'a 'c) (mlist 'x 'a 'x)) (mlist 'x 'a 'a 'c))
  (test-equal? "mlset-union! 3" (mlset-union! eq?) '())
  (test-equal? "mlset-union! 4" (mlset-union! eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))

  (test-equal? "mlset-intersection 1" (mlset-intersection eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'a 'e))
  (test-equal? "mlset-intersection 2" (mlset-intersection eq? (mlist 'a 'x 'y 'a) (mlist 'x 'a 'x 'z))
               (mlist 'a 'x 'a))
  (test-equal? "mlset-intersection 3" (mlset-intersection eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))
  (test-equal? "mlset-intersection! 1" (mlset-intersection! eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'a 'e))
  (test-equal? "mlset-intersection! 2" (mlset-intersection! eq? (mlist 'a 'x 'y 'a) (mlist 'x 'a 'x 'z))
               (mlist 'a 'x 'a))
  (test-equal? "mlset-intersection! 3" (mlset-intersection! eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))

  (test-equal? "mlset-difference 1" (mlset-difference eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'b 'c 'd))
  (test-equal? "mlset-difference 2" (mlset-difference eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))
  (test-equal? "mlset-difference! 1" (mlset-difference! eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'b 'c 'd))
  (test-equal? "mlset-difference! 2" (mlset-difference! eq? (mlist 'a 'b 'c)) (mlist 'a 'b 'c))

  (test-equal? "mlset-xor 1" (mlset-xor eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'd 'c 'b 'i 'o 'u))
  (test-equal? "mlset-xor 2" (mlset-xor eq?) '())
  (test-equal? "mlset-xor 3" (mlset-xor eq? (mlist 'a 'b 'c 'd 'e)) (mlist 'a 'b 'c 'd 'e))
  (test-equal? "mlset-xor! 1" (mlset-xor! eq? (mlist 'a 'b 'c 'd 'e) (mlist 'a 'e 'i 'o 'u))
               (mlist 'd 'c 'b 'i 'o 'u))
  (test-equal? "mlset-xor! 2" (mlset-xor! eq?) '())
  (test-equal? "mlset-xor! 3" (mlset-xor! eq? (mlist 'a 'b 'c 'd 'e)) (mlist 'a 'b 'c 'd 'e)))