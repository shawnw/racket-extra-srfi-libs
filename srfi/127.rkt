#lang racket/base
;;; SRFI-127 Lazy Sequences

;;; Based on the reference implementation but translated to Racket mcons lists.

(require racket/contract racket/function
         (only-in compatibility/mlist [list->mlist list->lseq])
         "1m.rkt"
         (only-in "158.rkt" gappend generator->list generator?) "190.rkt")
(module+ test (require rackunit))
(provide
 list->lseq
 (contract-out
  [lseq? predicate/c]
  [make-lseq (-> any/c ... proper-mlist?)]
  [generator->lseq (-> (-> any/c) lseq?)]
  [lseq=? (-> (-> any/c any/c any/c) lseq? lseq? boolean?)]
  [lseq-car (-> lseq? any/c)]
  [lseq-first (-> lseq? any/c)]
  [lseq-cdr (-> lseq? lseq?)]
  [lseq-rest (-> lseq? lseq?)]
  [lseq-ref (-> lseq? exact-nonnegative-integer? any/c)]
  [lseq-take (-> lseq? exact-nonnegative-integer? lseq?)]
  [lseq-drop (-> lseq? exact-nonnegative-integer? lseq?)]
  [lseq-realize (-> lseq? list?)]
  [lseq->generator (-> lseq? (-> any/c))]
  [lseq-length (-> lseq? exact-nonnegative-integer?)]
  [lseq-append (-> lseq? ... lseq?)]
  [lseq-zip (-> lseq? lseq? ... lseq?)]
  [lseq-map (->i ([proc (lseqs) (and/c (unconstrained-domain-> any/c)
                                       (lambda (f) (procedure-arity-includes? f (+ (length lseqs) 1))))]
                  [lseq1 lseq?])
                 () #:rest [lseqs (listof lseq?)]
                 [_ lseq?])]
  [lseq-for-each (->i ([proc (lseqs) (lambda (f)
                                       (and (procedure? f)
                                            (procedure-arity-includes? f (+ (length lseqs) 1))))]
                       [lseq1 lseq?])
                      () #:rest [lseqs (listof lseq?)]
                      [_ void?])]
  [lseq-filter (-> (-> any/c any/c) lseq? lseq?)]
  [lseq-remove (-> (-> any/c any/c) lseq? lseq?)]
  [lseq-find (-> (-> any/c any/c) lseq? any/c)]
  [lseq-find-tail (-> (-> any/c any/c) lseq? (or/c lseq? #f))]
  [lseq-take-while (-> (-> any/c any/c) lseq? lseq?)]
  [lseq-drop-while (-> (-> any/c any/c) lseq? lseq?)]
  [lseq-any (->i ([proc (lseqs) (and/c (unconstrained-domain-> any/c)
                                      (lambda (f) (procedure-arity-includes? f (+ (length lseqs) 1))))]
                  [lseq1 lseq?])
                 () #:rest [lseqs (listof lseq?)]
                 [_ any/c])]
  [lseq-every (->i ([proc (lseqs) (and/c (unconstrained-domain-> any/c)
                                         (lambda (f) (procedure-arity-includes? f (+ (length lseqs) 1))))]
                    [lseq1 lseq?])
                   () #:rest [lseqs (listof lseq?)]
                   [_ any/c])]
  [lseq-index (->i ([proc (lseqs) (and/c (unconstrained-domain-> any/c)
                                         (lambda (f) (procedure-arity-includes? f (+ (length lseqs) 1))))]
                    [lseq1 lseq?])
                   () #:rest [lseqs (listof lseq?)]
                   [_ (or/c exact-nonnegative-integer? #f)])]
  [lseq-member (->* (any/c lseq?) ((-> any/c any/c any/c)) lseq?)]
  [lseq-memq (-> any/c lseq? lseq?)]
  [lseq-memv (-> any/c lseq? lseq?)]
  [in-lseq (-> lseq? sequence?)]))

(define (lseq? obj)
  (cond
    ((null? obj) #t)
    ((not-mpair? obj) #f)
    ((generator? (mcdr obj)) #t)
    (else (lseq? (mcdr obj)))))

#;(define-coroutine-generator (make-generator . args)
  (for ([elem (in-list args)])
    (yield elem)))

(define (make-lseq . args) (list->lseq args))

(define (generator->lseq g)
  (let ([value (g)])
    (if (eof-object? value)
        '()
        (mcons value g))))

(define (lseq=? = a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or (null? a) (null? b)) #f)
    ((= (mcar a) (mcar b))
     (lseq=? = (lseq-cdr a) (lseq-cdr b)))
    (else #f)))

(define lseq-car (procedure-rename mcar 'lseq-car))
(define lseq-first (procedure-rename mcar 'lseq-first))

(define (lseq-cdr lseq)
  (if (generator? (mcdr lseq))
      (let ([value ((mcdr lseq))])
        (cond
          ((eof-object? value)
           (set-mcdr! lseq '())
           '())
          (else
           (set-mcdr! lseq (mcons value (mcdr lseq)))
           (mcdr lseq))))
      (mcdr lseq)))

(define lseq-rest (procedure-rename lseq-cdr 'lseq-rest))

(define (lseq-take lseq i)
  (generator->lseq
   (coroutine-generator
    (let loop ([i i]
               [lseq lseq])
      (unless (= i 0)
        (yield (mcar lseq))
        (loop (- i 1) (lseq-cdr lseq)))))))

(define (lseq-drop lseq i)
  (if (= i 0)
      lseq
      (lseq-drop (lseq-cdr lseq) (- i 1))))

(define (lseq-ref lseq i)
  (mcar (lseq-drop lseq i)))

(define (lseq-realize lseq)
  (generator->list (lseq->generator lseq)))

;; Return a generator that steps through the elements of the lseq
(define-coroutine-generator (lseq->generator lseq)
  (let loop ([lseq lseq])
    (unless (null? lseq)
      (yield (mcar lseq))
      (loop (lseq-cdr lseq)))))

(define (lseq-length lseq)
  (length (lseq-realize lseq)))

(define (lseq-append . lseqs)
  (generator->lseq (apply gappend (map lseq->generator lseqs))))

(define (safe-lseq-cdr lseq)
  (if (null? lseq)
      '()
      (lseq-cdr lseq)))

(define (any-null? list)
  (cond
    ((null? list) #f)
    ((null? (car list)) #t)
    (else (any-null? (cdr list)))))

;; Lazily map lseqs through a proc to produce another lseq
(define (lseq-map proc . lseqs)
  (%lseq-map proc lseqs))

(define (%lseq-map proc lseqs)
  (generator->lseq
   (coroutine-generator
    (let loop ([lseqs lseqs])
      (unless (any-null? lseqs)
        (yield (apply proc (map mcar lseqs)))
        (loop (map safe-lseq-cdr lseqs)))))))

;; Zip cars of lseqs into a list and return an lseq of those lists
(define (lseq-zip . lseqs) (%lseq-map list lseqs))

;; Eagerly apply a proc to the elements of lseqs
;; Included because it's a common operation, even though it is trivial
(define (lseq-for-each proc . lseqs)
  (apply for-each proc (map lseq-realize lseqs)))

;; Filter an lseq lazily to include only elements that satisfy pred
(define (lseq-filter pred lseq)
  (generator->lseq
   (coroutine-generator
    (let loop ([lseq1 lseq])
      (unless (null? lseq1)
        (let ([result (mcar lseq1)])
          (when (pred result)
            (yield result))
          (loop (lseq-cdr lseq1))))))))

;; Negated filter
(define (lseq-remove pred lseq)
  (lseq-filter (lambda (x) (not (pred x))) lseq))

;; Find an element that satisfies a pred, or #f if no such element
(define (lseq-find pred lseq)
  (cond
    ((null? lseq) #f)
    ((pred (mcar lseq)) (mcar lseq))
    (else (lseq-find pred (lseq-cdr lseq)))))

;; Find the tail of an lseq whose car satisfies a pred, or #f if no such
(define (lseq-find-tail pred lseq)
  (cond
    ((null? lseq) #f)
    ((pred (mcar lseq)) lseq)
    (else (lseq-find-tail pred (lseq-cdr lseq)))))

;; Return initial elements of lseq that satisfy pred
(define (lseq-take-while pred lseq)
  (generator->lseq
   (coroutine-generator
    (let loop ([lseq lseq])
      (when (pred (mcar lseq))
        (yield (mcar lseq))
        (loop (lseq-cdr lseq)))))))

;; Return all but initial of lseq that satisfy pred
;; No reason not to do it eagerly
(define (lseq-drop-while pred lseq)
  (cond
    ((null? lseq) '())
    ((not (pred (mcar lseq)))
     lseq)
    (else
     (lseq-drop-while pred (lseq-cdr lseq)))))

;; Apply predicate across lseqs, returning result if it is true
(define (lseq-any pred . lseqs)
  (let loop ([lseqs lseqs])
    (if (any-null? lseqs)
      #f
      (let ([result (apply pred (map mcar lseqs))])
        (if result
            result
            (loop (map lseq-cdr lseqs)))))))

;; Apply predicate across lseqs, returning false if predicate does
(define (lseq-every pred . lseqs)
  (let loop ([lseqs lseqs]
             [last-result #t])
    (if (any-null? lseqs)
        last-result
        (let ([result (apply pred (map mcar lseqs))])
          (if result
              (loop (map lseq-cdr lseqs) result)
              #f)))))

;; Return the index of the first element of lseq that satisfies pred
(define (lseq-index pred . lseqs)
  (let loop ([lseqs lseqs]
             [n 0])
    (cond
      ((any-null? lseqs) #f)
      ((apply pred (map mcar lseqs)) n)
      (else (loop (map safe-lseq-cdr lseqs) (+ n 1))))))

;; Return tail of lseq whose first element is x in the sense of = (default equal?)
(define (lseq-member x lseq [= equal?])
  (cond
    ((null? lseq) #f)
    ((= x (mcar lseq)) lseq)
    (else (lseq-member x (lseq-cdr lseq) =))))

(define (lseq-memq val lseq) (lseq-member val lseq eq?))
(define (lseq-memv val lseq) (lseq-member val lseq eqv?))

(define (in-lseq lseq)
  (make-do-sequence
   (lambda ()
     (values
      lseq-car
      lseq-cdr
      lseq
      (negate null?)
      #f
      #f))))

(module+ test
  (define-syntax-rule (test-group name tests ...)
    (let ()
      #;(printf "Running ~A~%" name)
      tests ...))
  (define-syntax-rule (test expected test-expr)
    (check-equal? test-expr expected))
  (define-syntax-rule (test-assert test-expr)
    (check-not-false test-expr))
  (define-syntax-rule (test-error test-expr)
    (check-exn exn:fail? (lambda () test-expr)))

  (test-group "lseqs"
              (test-group "lseqs/constructor"
                          (define one23 (make-lseq 1 2 3))
                          (test 1 (mcar one23))
                          #;(test-assert (procedure? (mcdr one23)))
                          (test '(1 2 3) (lseq-realize one23))
                          ) ; end lseqs/constructor

              (test-group "lseqs/predicates"
                          (test-assert (lseq? '()))
                          (test-assert (lseq? (mlist 1 2 3)))
                          (test-assert (lseq? (make-lseq 1 2 3)))
                          (test-assert (lseq? (mcons 'x (lambda () 'x))))

                          (test-assert (lseq=? = '() '()))
                          (test-assert (lseq=? = (mlist 1 2 3) (mlist 1 2 3)))
                          (test-assert (lseq=? = (make-lseq 1 2 3)
                                               (make-lseq 1 2 3)))
                          #;(test-assert (lseq=? = (make-lseq 1 2 3) (list->lseq '(1 2 3))))
                          ) ; end lseqs/predicates

              (test-group "lseqs/selectors"
                          #;(test-error (lseq-car (make-generator)))
                          (test 1 (lseq-car (make-lseq 1 2 3)))
                          (test 1 (lseq-car (mlist 1 2 3)))
                          (test-error (lseq-car 2))
                          #;(test-error (lseq-first (make-generator)))
                          (test 1 (lseq-first (make-lseq 1 2 3)))
                          (test 1 (lseq-first (mlist 1 2 3)))
                          (test-error (lseq-first 2))

                          #;(test-error (lseq-cdr (make-generator)))
                          #;(test 2 (lseq-cdr '(1 . 2)))
                          (test 2 (lseq-car (lseq-cdr (mlist 1 2 3 4))))
                          (test 2 (lseq-car (lseq-cdr (make-lseq 1 2 3))))

                          #;(test-error (lseq-rest (make-generator)))
                          #;(test 2 (lseq-rest '(1 . 2)))
                          (test 2 (lseq-car (lseq-rest (mlist 1 2 3))))
                          (test 2 (lseq-car (lseq-rest (make-lseq 1 2 3))))
                          (test-error (lseq-rest 2))

                          (test-error (lseq-ref '() 0))
                          (test 1 (lseq-ref (mlist 1) 0))
                          (test 2 (lseq-ref (mlist 1 2) 1))
                          (test-error (lseq-ref (make-lseq) 0))
                          (test 1 (lseq-ref (make-lseq 1) 0))
                          (test 1 (lseq-ref (make-lseq 1 2) 0))
                          (test 2 (lseq-ref (make-lseq 1 2) 1))

                          (test-error (lseq-take '() 1))
                          (test-error (lseq-take (make-lseq) 1))
                          (test-assert (procedure? (mcdr (lseq-take (make-lseq 1 2 3 4 5) 3)))) ; test laziness
                          (test '(1 2 3) (lseq-realize (lseq-take (make-lseq 1 2 3 4 5) 3)))

                          (test-error (lseq-drop '() 1))
                          (test-error (lseq-drop (make-lseq 1) 2))
                          (test '(3 4 5) (lseq-realize (lseq-drop (mlist 1 2 3 4 5) 2)))
                          (test '(3 4 5) (lseq-realize (lseq-drop (make-lseq 1 2 3 4 5) 2)))
                          ) ; end lseqs/selectors

              (test-group "lseqs/whole"
                          (test '() (lseq-realize '()))
                          (test '(1 2 3) (lseq-realize (mlist 1 2 3)))
                          (test '() (lseq-realize (make-lseq)))
                          (test '(1 2 3) (lseq-realize (make-lseq 1 2 3)))

                          (define g (lseq->generator (mlist 1 2 3)))
                          (begin
                            (test 1 (g))
                            (test 2 (g))
                            (test 3 (g))
                            (test-assert (eof-object? (g))))
                          (define g2 (lseq->generator (make-lseq 1 2 3)))
                          (begin
                            (test 1 (g2))
                            (test 2 (g2))
                            (test 3 (g2))
                            (test-assert (eof-object? (g2))))

                          (test 0 (lseq-length '()))
                          (test 3 (lseq-length (mlist 1 2 3)))
                          (test 3 (lseq-length (make-lseq 1 2 3)))

                          (test '(1 2 3 a b c) (lseq-realize (lseq-append (mlist 1 2 3) (mlist 'a 'b 'c))))
                          (define one23abc (lseq-append (make-lseq 1 2 3) (make-lseq 'a 'b 'c)))
                          (test-assert (procedure? (mcdr one23abc)))
                          (test-assert (lseq-realize one23abc))

                          (define one2345 (make-lseq 1 2 3 4 5))
                          (define oddeven (make-lseq 'odd 'even 'odd 'even 'odd 'even 'odd 'even))
                          (test '((one 1 odd) (two 2 even) (three 3 odd))
                                (lseq-realize (lseq-zip (mlist 'one 'two 'three) one2345 oddeven)))
                          ) ; end lseqs/whole

              (test-group "lseqs/mapping"
                          (test '() (lseq-map - '()))
                          (test '(-1 -2 -3) (lseq-realize (lseq-map - (mlist 1 2 3))))
                          (test '(-1 -2 -3) (lseq-realize (lseq-map - (make-lseq 1 2 3))))
                          (test-assert (procedure? (mcdr (lseq-map - (mlist 1 2 3)))))

                          (define output '())
                          (define out! (lambda (x) (set! output (cons x output))))
                          (lseq-for-each out! '())
                          (test output '())
                          (lseq-for-each out! (mlist 'a 'b 'c))
                          (test output '(c b a))
                          (lseq-for-each out! (make-lseq 1 2 3))
                          (test output '(3 2 1 c b a))

                          (test '() (lseq-filter odd? '()))
                          (define odds (lseq-filter odd? (mlist 1 2 3 4 5)))
                          (test-assert (procedure? (mcdr odds)))
                          (test '(1 3 5) (lseq-realize odds))
                          (test '(1 3 5) (lseq-realize (lseq-filter odd? (make-lseq 1 2 3 4 5))))

                          (test '() (lseq-remove even? '()))
                          (define odds2 (lseq-remove even? (mlist 1 2 3 4 5)))
                          (test-assert (procedure? (mcdr odds2)))
                          (test '(1 3 5) (lseq-realize odds2))
                          (test '(1 3 5) (lseq-realize (lseq-remove even? (make-lseq 1 2 3 4 5))))

                          ) ; end lseqs/mapping

              (test-group "lseqs/searching"
                          (test 4 (lseq-find even? (mlist 3 1 4 1 5 9 2 6)))
                          (test 4 (lseq-find even? (make-lseq 3 1 4 1 5 9 2 6)))
                          (test #f (lseq-find negative? (make-lseq 1 2 3 4 5)))

                          (test '(-8 -5 0 0) (lseq-realize (lseq-find-tail even? (mlist 3 1 37 -8 -5 0 0))))
                          (test '(-8 -5 0 0) (lseq-realize (lseq-find-tail even?
                                                                           (make-lseq 3 1 37 -8 -5 0 0))))
                          (test #f (lseq-find-tail even? '()))
                          (test #f (lseq-find-tail negative? (make-lseq 1 2 3 4 5)))

                          (test '(2 18) (lseq-realize (lseq-take-while even? (mlist 2 18 3 10 22 9))))
                          (test '(2 18) (lseq-realize (lseq-take-while even?
                                                                       (make-lseq 2 18 3 10 22 9))))
                          (test '(2 18) (lseq-realize (lseq-take-while even?
                                                                       (make-lseq 2 18 3 10 22 9))))

                          (test '(3 10 22 9) (lseq-realize (lseq-drop-while even? (mlist 2 18 3 10 22 9))))
                          (test '(3 10 22 9) (lseq-realize (lseq-drop-while even?
                                                                            (make-lseq 2 18 3 10 22 9))))

                          (test #t (lseq-any integer? (mlist 'a 3 'b 2.7)))
                          (test #t (lseq-any integer? (make-lseq 'a 3 'b 2.7)))
                          (test #f (lseq-any integer? (mlist 'a 3.1 'b 2.7)))
                          (test #f (lseq-any integer? (make-lseq 'a 3.1 'b 2.7)))
                          (test #t (lseq-any < (mlist 3 1 4 1 5) (mlist 2 7 1 8 2)))
                          (define (factorial n)
                            (cond
                              ((< n 0) #f)
                              ((= n 0) 1)
                              (else (* n (factorial (- n 1))))))
                          (test 6 (lseq-any factorial (mlist -1 -2 3 4)))
                          (test 6 (lseq-any factorial (make-lseq -1 -2 3 4)))

                          (test 24 (lseq-every factorial (mlist 1 2 3 4)))
                          (test 24 (lseq-every factorial (make-lseq 1 2 3 4)))

                          (test 2 (lseq-index even? (mlist 3 1 4 1 5 9)))
                          (test 1 (lseq-index < (mlist 3 1 4 1 5 9 2 5 6) (mlist 2 7 1 8 2)))
                          (test #f (lseq-index = (mlist 3 1 4 1 5 9 2 5 6) (mlist 2 7 1 8 2)))

                          (test '(a b c) (lseq-realize (lseq-memq 'a (mlist 'a 'b 'c))))
                          (test '(a b c) (lseq-realize (lseq-memq 'a (make-lseq 'a 'b 'c))))
                          (test #f (lseq-memq 'a (make-lseq 'b 'c 'd)))
                          (test #f (lseq-memq (list 'a) (mlist 'b 'c 'd)))
                          (test #f (lseq-memq (list 'a) (make-lseq 'b 'c 'd)))

                          (test '(101 102) (lseq-realize (lseq-memv 101 (make-lseq 100 101 102))))

                          (test '((a) c) (lseq-realize (lseq-member (list 'a) (make-lseq 'b '(a) 'c))))
                          (test '(2 3) (lseq-realize (lseq-member 2.0 (make-lseq 1 2 3) =)))
                          )) ; end lseqs/searching

  (test-group "in-lseq"
              (test-equal? "in-lseq populated"
                           '(a b c d)
                           (for/list ([elem (in-lseq (make-lseq 'a 'b 'c 'd))]) elem))
              (test-equal? "in-lseq empty"
                           '()
                           (for/list ([elem (in-lseq (make-lseq))]) elem))))
