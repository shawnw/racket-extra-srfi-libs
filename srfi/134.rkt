#lang racket/base

;;; Version for Racket 8.13+ using treelists.
;;; Written by Shawn Wagner

(require racket/contract racket/sequence racket/struct racket/stream racket/treelist syntax/parse/define (for-syntax racket/base syntax/for-body)
         (only-in srfi/1 unfold list= zip append-map iota take drop split-at) (only-in "158.rkt" generator generator->list))
(module+ test (require "private/testwrappers.rkt" srfi/8))

(provide
 (contract-out
  [ideque? predicate/c]
  [ideque (-> any/c ... ideque?)]
  [ideque-tabulate (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? any/c) ideque?)]
  [ideque-unfold (-> (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c ideque?)]
  [ideque-unfold-right (-> (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c ideque?)]
  [ideque-empty? (-> ideque? boolean?)]
  [ideque= (-> (-> any/c any/c any/c) ideque? ... boolean?)]
  [ideque-any (-> (-> any/c any/c) ideque? any/c)]
  [ideque-every (-> (-> any/c any/c) ideque? any/c)]

  [ideque-front (-> (and/c ideque? (not/c ideque-empty?)) any/c)]
  [ideque-add-front (-> ideque? any/c ideque?)]
  [ideque-remove-front (-> (and/c ideque? (not/c ideque-empty?)) ideque?)]
  [ideque-back (-> (and/c ideque? (not/c ideque-empty?)) any/c)]
  [ideque-add-back (-> ideque? any/c ideque?)]
  [ideque-remove-back (-> (and/c ideque? (not/c ideque-empty?)) ideque?)]

  [ideque-ref (-> ideque? exact-nonnegative-integer? any/c)]
  [ideque-take (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-take-right (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-drop (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-drop-right (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-split-at (-> ideque? exact-nonnegative-integer? (values ideque? ideque?))]

  [ideque-length (-> ideque? exact-nonnegative-integer?)]
  [ideque-append (-> ideque? ... ideque?)]
  [ideque-reverse (-> ideque? ideque?)]
  [ideque-count (-> (-> any/c any/c) ideque? exact-nonnegative-integer?)]
  [ideque-zip (-> ideque? ideque? ... ideque?)]

  [ideque-map (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-filter-map (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-for-each (-> (-> any/c any/c) ideque? void?)]
  [ideque-for-each-right (-> (-> any/c any/c) ideque? void?)]
  [ideque-fold (-> (-> any/c any/c any/c) any/c ideque? any/c)]
  [ideque-fold-right (-> (-> any/c any/c any/c) any/c ideque? any/c)]
  [ideque-append-map (-> (-> any/c list?) ideque? ideque?)]

  [ideque-filter (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-remove (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-partition (-> (-> any/c any/c) ideque? (values ideque? ideque?))]

  [ideque-find (->* ((-> any/c any/c) ideque?) ((-> any/c)) any/c)]
  [ideque-find-right (->* ((-> any/c any/c) ideque?) ((-> any/c)) any/c)]
  [ideque-take-while (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-take-while-right (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-drop-while (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-drop-while-right (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-span (-> (-> any/c any/c) ideque? (values ideque? ideque?))]
  [ideque-break (-> (-> any/c any/c) ideque? (values ideque? ideque?))]

  [list->ideque (-> list? ideque?)]
  [ideque->list (-> ideque? list?)]
  [generator->ideque (-> (-> any/c) ideque?)]
  [ideque->generator (-> ideque? (-> any/c))]

  ;; Extras
  [in-ideque (-> ideque? sequence?)]
  [in-ideque-backwards (-> ideque? sequence?)]
  )
 for/ideque for*/ideque
 )

;;;; ideque type

(struct ideque (tl)
  #:name <ideque>
  #:constructor-name real-make-ideque
  #:sealed
  #:methods gen:equal+hash
  [(define (equal-proc a b equal-wrapper?)
     (equal-wrapper? (ideque-tl a) (ideque-tl b)))
   (define (hash-proc a hash-code)
     (hash-code (ideque-tl a)))
   (define (hash2-proc a hash-code)
     (hash-code (ideque-tl a)))]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'ideque)
      (lambda (obj) (in-ideque obj))))]
  #:property prop:custom-print-quotable 'never
  #:property prop:sequence (lambda (dq) (in-ideque dq))
  #:methods gen:stream
  [(define (stream-empty? dq) (ideque-empty? dq))
   (define (stream-first dq) (ideque-front dq))
   (define (stream-rest dq) (ideque-remove-front dq))]
  )

(define *empty-ideque* (real-make-ideque empty-treelist))

(define (make-ideque tl)
  (if (treelist-empty? tl)
      *empty-ideque*
      (real-make-ideque tl)))

;;;; Basic operations

(define (ideque-empty? dq)
  (or (eq? dq *empty-ideque*)
      (treelist-empty? (ideque-tl dq))))

(define (ideque-add-front dq x)
  (make-ideque (treelist-cons (ideque-tl dq) x)))

(define (ideque-front dq)
  (if (ideque-empty? dq)
      (raise-argument-error 'ideque-front "(not/c ideque-empty?)" dq)
      (treelist-first (ideque-tl dq))))

(define (ideque-remove-front dq)
  (case (treelist-length (ideque-tl dq))
    ((0)
     (raise-argument-error 'ideque-remove-front "(not/c ideque-empty?)" dq))
    ((1) *empty-ideque*)
    (else
     (make-ideque (treelist-rest (ideque-tl dq))))))

(define (ideque-add-back dq x)
  (make-ideque (treelist-add (ideque-tl dq) x)))

(define (ideque-back dq)
  (if (ideque-empty? dq)
      (raise-argument-error 'ideque-back "(not/c ideque-empty?)" dq)
      (treelist-last (ideque-tl dq))))

(define (ideque-remove-back dq)
  (case (treelist-length (ideque-tl dq))
    ((0)
     (raise-argument-error 'ideque-remove-front "(not/c ideque-empty?)" dq))
    ((1) *empty-ideque*)
    (else
     (make-ideque (treelist-drop-right (ideque-tl dq) 1)))))

(define (ideque-reverse dq)
  (make-ideque (treelist-reverse (ideque-tl dq))))

;;; Exported constructors

(define (ideque . args)
  (list->ideque args))

(define (ideque-tabulate size init)
  (make-ideque (for/treelist ([i (in-range size)])
         (init i))))

(define (ideque-unfold p f g seed)
  (list->ideque (unfold p f g seed)))

(define (ideque-unfold-right p f g seed)
  (make-ideque (treelist-reverse (list->treelist (unfold p f g seed)))))

;;;; Other operations

(define ideque=
  (case-lambda
    ((elt=) #t)
    ((elt= dq1 dq2) ; fast path
     (%ideque=-binary elt= dq1 dq2))
    ((elt= . dqs)
     ;; The comparison scheme is the same as srfi-1's list=.
     ;; This could be tuned.
     (apply list= elt= (map ideque->list dqs)))))

(define (%ideque-same-length dq1 dq2)
  (= (ideque-length dq1) (ideque-length dq2)))

(define (%ideque=-binary elt= dq1 dq2)
  (or (eq? dq1 dq2)
      (and (%ideque-same-length dq1 dq2)
           (for/and ([a (in-ideque dq1)]
                     [b (in-ideque dq2)])
             (elt= a b)))))

(define (ideque-ref dq n)
  (if (>= n (treelist-length (ideque-tl dq)))
      (raise-range-error 'ideque-ref "ideque" "" n dq 0 (treelist-length (ideque-tl dq)))
      (treelist-ref (ideque-tl dq) n)))

(define-syntax-rule (define/index-in-range (name dq n) body ...)
  (define (name dq n)
    (unless (<= 0 n (ideque-length dq))
      (raise-range-error (quote name) "ideque" "" n dq 0 (- (ideque-length dq) 1)))
    body ...))

(define/index-in-range (ideque-take dq n)
  (make-ideque (treelist-take (ideque-tl dq) n)))

(define/index-in-range (ideque-take-right dq n)
  (make-ideque (treelist-take-right (ideque-tl dq) n)))

(define/index-in-range (ideque-drop dq n)
  (make-ideque (treelist-drop (ideque-tl dq) n)))

(define/index-in-range (ideque-drop-right dq n)
   (make-ideque (treelist-drop-right (ideque-tl dq) n)))

(define/index-in-range (ideque-split-at dq n)
  (values (make-ideque (treelist-take (ideque-tl dq) n))
          (make-ideque (treelist-drop (ideque-tl dq) n))))

(define (ideque-length dq)
  (treelist-length (ideque-tl dq)))

(define (ideque-append . dqs)
  (make-ideque (apply treelist-append (map ideque-tl dqs))))

(define (ideque-count pred dq)
  (inexact->exact
   (for/sum ([elem (in-ideque dq)]
             #:when (pred elem))
     1)))

;;;TODO
(define (ideque-zip dq . dqs)
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (list->ideque elts)))

(define (ideque-map proc dq)
  (make-ideque (treelist-map (ideque-tl dq) proc)))

(define (ideque-filter-map proc dq)
  (make-ideque (for/treelist ([elem (in-ideque dq)]
                              #:do [(define mapped (proc elem))]
                              #:when mapped)
                 mapped)))

(define (ideque-for-each proc dq)
  (treelist-for-each (ideque-tl dq) proc))

(define (ideque-for-each-right proc dq)
  (for ([elem (in-ideque-backwards dq)])
    (proc elem)))

(define (ideque-fold proc knil dq)
  (for/fold ([r knil])
            ([elem (in-ideque dq)])
    (proc elem r)))

(define (ideque-fold-right proc knil dq)
  (for/foldr ([r knil])
    ([elem (in-ideque dq)])
    (proc elem r)))

(define (ideque-append-map proc dq)
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(define (ideque-filter pred dq)
  (make-ideque (for/treelist ([elem (in-ideque dq)]
                              #:when (pred elem))
                 elem)))

(define (ideque-remove pred dq)
  (make-ideque (for/treelist ([elem (in-ideque dq)]
                              #:unless (pred elem))
                 elem)))

(define (ideque-partition pred dq)
  (for/fold ([satisfies empty-treelist]
             [fails empty-treelist]
             #:result (values (make-ideque satisfies) (make-ideque fails)))
            ([elem (in-ideque dq)])
    (if (pred elem)
        (values (treelist-add satisfies elem) fails)
        (values satisfies (treelist-add fails elem)))))

(define (ideque-find pred dq [failure (lambda () #f)])
  (define tl (ideque-tl dq))
  (define len (treelist-length tl))
  (let loop ([i 0])
    (cond
      [(= i len) (failure)]
      [(pred (treelist-ref tl i)) (treelist-ref tl i)]
      [else (loop (add1 i))])))

(define (ideque-find-right pred dq [failure (lambda () #f)])
  (define tl (ideque-tl dq))
  (let loop ([i (sub1 (treelist-length tl))])
    (cond
      [(< i 0) (failure)]
      [(pred (treelist-ref tl i)) (treelist-ref tl i)]
      [else (loop (sub1 i))])))

(define (ideque-take-while pred dq)
  (define tl (ideque-tl dq))
  (define len (treelist-length tl))
  (let loop ([i 0])
    (cond
      [(= i len) dq]
      [(pred (treelist-ref tl i))
       (loop (add1 i))]
      [else
       (make-ideque (treelist-sublist tl 0 i))])))

(define (ideque-take-while-right pred dq)
  (define tl (ideque-tl dq))
  (let loop ([i (sub1 (treelist-length tl))])
    (cond
      [(< i 0) dq]
      [(pred (treelist-ref tl i))
       (loop (sub1 i))]
      [else
       (make-ideque (treelist-sublist tl (add1 i) (treelist-length tl)))])))

(define (ideque-drop-while pred dq)
  (define tl (ideque-tl dq))
  (define len (treelist-length tl))
  (let loop ([i 0])
    (cond
      [(= i len) *empty-ideque*]
      [(pred (treelist-ref tl i))
       (loop (add1 i))]
      [else (make-ideque (treelist-sublist tl i len))])))

(define (ideque-drop-while-right pred dq)
  (define tl (ideque-tl dq))
  (let loop ([i (sub1 (treelist-length tl))])
    (cond
      [(< i 0) *empty-ideque*]
      [(pred (treelist-ref tl i))
       (loop (sub1 i))]
      [else (make-ideque (treelist-sublist tl 0 (add1 i)))])))

(define (ideque-span pred dq)
  (define tl (ideque-tl dq))
  (define len (treelist-length tl))
  (let loop ([i 0])
    (cond
      [(= i len)
       (values dq *empty-ideque*)]
      [(pred (treelist-ref tl i))
       (loop (add1 i))]
      [else
       (values (make-ideque (treelist-sublist tl 0 i)) (make-ideque (treelist-sublist tl i len)))])))

(define (ideque-break pred dq)
  (define tl (ideque-tl dq))
  (define len (treelist-length tl))
  (let loop ([i 0])
    (cond
      [(= i len)
       (values dq *empty-ideque*)]
      [(pred (treelist-ref tl i))
       (values (make-ideque (treelist-sublist tl 0 i)) (make-ideque (treelist-sublist tl i len)))]
      [else
       (loop (add1 i))])))

(define (ideque-any pred dq)
  (for/or ([elem (in-ideque dq)])
    (pred elem)))

(define (ideque-every pred dq)
  (for/and ([elem (in-ideque dq)])
    (pred elem)))

(define (ideque->list dq)
  (treelist->list (ideque-tl dq)))

(define (list->ideque lis)
  (if (null? lis)
      *empty-ideque*
      (make-ideque (list->treelist lis))))

(define (ideque->generator dq)
  (lambda ()
    (if (ideque-empty? dq)
        eof
        (let ((v (ideque-front dq)))
          (set! dq (ideque-remove-front dq))
          v))))

(define (generator->ideque gen)
  (list->ideque (generator->list gen)))

(define (in-ideque dq)
  (in-treelist (ideque-tl dq)))

(define (in-ideque-backwards dq)
  (make-do-sequence
   (lambda ()
     (initiate-sequence
      #:pos->element (lambda (i) (ideque-ref dq i))
      #:next-pos sub1
      #:init-pos (sub1 (ideque-length dq))
      #:continue-with-pos? exact-nonnegative-integer?))))

(define-syntax-parse-rule (for/ideque clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ((dq (ideque)))
    clauses
    pre-body ...
    (ideque-add-back dq (let () post-body ...))))

(define-syntax-parse-rule (for*/ideque clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ((dq (ideque)))
    clauses
    pre-body ...
    (ideque-add-back dq (let () post-body ...))))

(module+ test

  (test-group "ideque/constructors"
              (test '() (ideque->list (ideque)))
              (test '() (ideque->list (list->ideque '())))
              (test '(1 2 3) (ideque->list (ideque 1 2 3)))
              (test '(4 5 6 7) (ideque->list (list->ideque '(4 5 6 7))))
              (test '(10 9 8 7 6 5 4 3 2 1)
                    (ideque->list (ideque-unfold zero? values (lambda (n) (- n 1)) 10)))
              (test '(1 2 3 4 5 6 7 8 9 10)
                    (ideque->list (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10)))
              (test '(0 2 4 6 8 10)
                    (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2)))))

              ;; corner cases
              (test '() (ideque->list
                         (ideque-unfold (lambda (n) #t) values (lambda (n) (+ n 1)) 0)))
              (test '() (ideque->list
                         (ideque-unfold-right (lambda (n) #t) values (lambda (n) (+ n 1)) 0)))
              (test '() (ideque->list (ideque-tabulate 0 values)))
              )

  (test-group "ideque/predicates"
              (test-assert (ideque? (ideque)))
              (test-assert (not (ideque? 1)))
              (test-assert (ideque-empty? (ideque)))
              (test-assert (not (ideque-empty? (ideque 1))))
              (test-assert (ideque= eq?))
              (test-assert (ideque= eq? (ideque 1)))
              (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B)))
              (test-assert (ideque= char-ci=? (ideque) (ideque)))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B #\c))))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A))))
              (test-assert (ideque= char-ci=? (ideque) (ideque) (ideque)))
              (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\a #\B)))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A) (ideque #\a #\B))))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\A #\B #\c))))
              )

  (test-group "ideque/queue-operations"
              (test-error (ideque-front (ideque)))
              (test-error (ideque-back (ideque)))
              (test 1 (ideque-front (ideque 1 2 3)))
              (test 3 (ideque-back (ideque 1 2 3)))
              (test 2 (ideque-front (ideque-remove-front (ideque 1 2 3))))
              (test 2 (ideque-back (ideque-remove-back (ideque 1 2 3))))
              (test 1 (ideque-front (ideque-remove-back (ideque 1 2 3))))
              (test 3 (ideque-back (ideque-remove-front (ideque 1 2 3))))
              (test-assert (ideque-empty? (ideque-remove-front (ideque 1))))
              (test-assert (ideque-empty? (ideque-remove-back (ideque 1))))
              (test 0 (ideque-front (ideque-add-front (ideque 1 2 3) 0)))
              (test 0 (ideque-back (ideque-add-back (ideque 1 2 3) 0)))
              ;; loss of front ideque
              (let ((id (ideque #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (test #f (ideque-front (ideque-take-right id 12))))
              )

  (test-group "ideque/other-accessors"
              (define (check name ideque-op list-op n)
                (let* ((lis (iota n))
                       (dq (list->ideque lis)))
                  (for-each (lambda (i)
                              (test (string-append (symbol->string name)
                                                   " "
                                                   (number->string i))
                                    (receive xs (list-op lis i) xs)
                                    (receive xs (ideque-op dq i)
                                             (map ideque->list xs))))
                            lis)))
              (check 'ideque-take ideque-take take 7)
              (test '(1 2 3 4) (ideque->list (ideque-take (ideque 1 2 3 4) 4)))
              (test '(1 2 3 4) (ideque->list (ideque-take-right (ideque 1 2 3 4) 4)))
              (check 'ideque-drop ideque-drop drop 6)
              (test '() (ideque->list (ideque-drop (ideque 1 2 3 4) 4)))
              (test '() (ideque->list (ideque-drop-right (ideque 1 2 3 4) 4)))
              (check 'ideque-split-at ideque-split-at split-at 8)
              ;; out-of-range conditions
              (test-error (ideque->list (ideque-take (ideque 1 2 3 4 5 6 7) 10)))
              (test-error (ideque->list (ideque-take-right (ideque 1 2 3 4 5 6 7) 10)))
              (test-error (ideque-split-at (ideque 1 2 3 4 5 6 7) 10))

              (test '(3 2 1) (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2)))
              (test-error (ideque-ref (ideque 3 2 1) -1))
              (test-error (ideque-ref (ideque 3 2 1) 3))
              )

  (test-group "ideque/whole-ideque"
              (test 7 (ideque-length (ideque 1 2 3 4 5 6 7)))
              (test 0 (ideque-length (ideque)))
              (test '() (ideque->list (ideque-append)))
              (test '() (ideque->list (ideque-append (ideque) (ideque))))
              (test '(1 2 3 a b c d 5 6 7 8 9)
                    (ideque->list (ideque-append (ideque 1 2 3)
                                                 (ideque 'a 'b 'c 'd)
                                                 (ideque)
                                                 (ideque 5 6 7 8 9))))
              (test '() (ideque->list (ideque-reverse (ideque))))
              (test '(5 4 3 2 1) (ideque->list (ideque-reverse (ideque 1 2 3 4 5))))
              (test 0 (ideque-count odd? (ideque)))
              (test 3 (ideque-count odd? (ideque 1 2 3 4 5)))
              (test '((1 a) (2 b) (3 c))
                    (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e))))
              (test '((1 a x) (2 b y) (3 c z))
                    (ideque->list (ideque-zip (ideque 1 2 3 4 5)
                                              (ideque 'a 'b 'c 'd 'e)
                                              (ideque 'x 'y 'z))))
              (test '((1) (2) (3))
                    (ideque->list (ideque-zip (ideque 1 2 3))))
              (test '()
                    (ideque->list (ideque-zip (ideque 1 2 3) (ideque))))
            )

  (test-group "ideque/mapping"
              (test-assert (ideque-empty? (ideque-map list (ideque))))
              (test '(-1 -2 -3 -4 -5) (ideque->list (ideque-map - (ideque 1 2 3 4 5))))
              (test '(-1 -3 5 -8)
                    (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x)))
                                                     (ideque 1 3 'a -5 8))))
              (test '(5 4 3 2 1)
                    (let ((r '()))
                      (ideque-for-each (lambda (n) (set! r (cons n r)))
                                       (ideque 1 2 3 4 5))
                      r))
              (test '(1 2 3 4 5)
                    (let ((r '()))
                      (ideque-for-each-right (lambda (n) (set! r (cons n r)))
                                             (ideque 1 2 3 4 5))
                      r))
              (test '(5 4 3 2 1 . z)
                    (ideque-fold cons 'z (ideque 1 2 3 4 5)))
              (test '(1 2 3 4 5 . z)
                    (ideque-fold-right cons 'z (ideque 1 2 3 4 5)))
              (test '(a a b b c c)
                    (ideque->list (ideque-append-map (lambda (x) (list x x))
                                                     (ideque 'a 'b 'c))))
              )

  (test-group "ideque/filtering"
              (test '(1 3 5)
                    (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5))))
              (test '(2 4)
                    (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5))))
              (test '((1 3 5) (2 4))
                    (receive xs (ideque-partition odd? (ideque 1 2 3 4 5))
                             (map ideque->list xs)))
              )

  (test-group "ideque/searching"
              (test 3 (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
              (test 'boo (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
              (test #f (ideque-find number? (ideque 'a 'b 'c 'd)))
              (test 4 (ideque-find-right number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
              (test 'boo (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
              (test #f (ideque-find-right number? (ideque 'a 'b 'c 'd)))
              (test '(1 3 2)
                    (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(5 8 4 6 3 4 2)
                    (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(3 4 2)
                    (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(1 3 2 5 8 4 6)
                    (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '()
                    (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                                     (ideque 5 8 4 6 3 4 2 9))))
              (test '()
                    (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                                     (ideque 1 4 3 2 3 4 2 1))))
              (test '()
                    (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                           (ideque 5 8 4 6 3 4 2 9))))
              (test '()
                    (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 4 3 2 3 2))))
              (test '((1 3 2) (5 8 4 6 3 4 2))
                    (receive xs (ideque-span (lambda (n) (< n 5))
                                             (ideque 1 3 2 5 8 4 6 3 4 2))
                             (map ideque->list xs)))
              (test '((5 8) (4 6 3 4 2 9))
                    (receive xs (ideque-break (lambda (n) (< n 5))
                                              (ideque 5 8 4 6 3 4 2 9))
                             (map ideque->list xs)))
              (test 3 (ideque-any (lambda (x) (and (number? x) x))
                                  (ideque 'a 3 'b 'c 4 'd 'e)))
              (test 5 (ideque-any (lambda (x) (and (number? x) x))
                                  (ideque 'a 'b 'c 'd 'e 5)))
              (test #f (ideque-any (lambda (x) (and (number? x) x))
                                   (ideque 'a 'b 'c 'd 'e)))
              (test 9 (ideque-every (lambda (x) (and (number? x) x))
                                    (ideque 1 5 3 2 9)))
              (test #f (ideque-every (lambda (x) (and (number? x) x))
                                     (ideque 1 5 'a 2 9)))
              ;; check if we won't see further once we found the result
              (test 1 (ideque-any (lambda (x) (and (odd? x) x))
                                  (ideque 2 1 'a 'b 'c 'd)))
              (test #f (ideque-every (lambda (x) (and (odd? x) x))
                                     (ideque 1 2 'a 'b 'c 'd)))

              (test '(1 2 3) (generator->list (ideque->generator (ideque 1 2 3))))
              (test '() (generator->list (ideque->generator (ideque))))
              (test '(1 2 3) (ideque->list (generator->ideque (generator 1 2 3))))
              (test '() (ideque->list (generator->ideque (generator))))
              )

  (test-group "sequences"
              (test '(1 2 3) (for/list ([elem (ideque 1 2 3)]) elem))
              (test '(3 2 1) (for/list ([elem (in-ideque-backwards (ideque 1 2 3))]) elem))
              (test '(0 1 2 3) (ideque->list (for/ideque ((n (in-range 4))) n))))

  (test-group "equality"
              (test-true (equal? (ideque 1 2 3) (ideque 1 2 3)))
              (test-false (equal? (ideque 1 2 3) (ideque 1 2 #\c))))
  )
