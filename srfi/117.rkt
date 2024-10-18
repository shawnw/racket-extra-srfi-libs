#lang racket/base

;;; SRFI-117 implementation using mcons
(require racket/contract racket/list racket/match racket/struct "1m.rkt")
(module+ test (require "private/testwrappers.rkt"))

(provide
 (contract-out
  #:unprotected-submodule unsafe
  [list-queue? predicate/c]
  [make-list-queue (case-> (-> (or/c list? proper-mlist?) list-queue?) (-> proper-mlist? (or/c mpair? null?) list-queue?))]
  [list-queue (-> any/c ... list-queue?)]
  [list-queue-copy (-> list-queue? list-queue?)]
  [list-queue-unfold (->* ((-> any/c any/c) (-> any/c any/c) (any/c any/c) any/c) (list-queue?) list-queue?)]
  [list-queue-unfold-right (->* ((-> any/c any/c) (-> any/c any/c) (any/c any/c) any/c) (list-queue?) list-queue?)]
  [list-queue-empty? (-> list-queue? boolean?)]
  [list-queue-front (-> list-queue? any/c)]
  [list-queue-back (-> list-queue? any/c)]
  [list-queue-list (-> list-queue? proper-mlist?)]
  [list-queue-first-last (-> list-queue? (values (or/c mpair? null?) (or/c mpair? null?)))]
  [list-queue-add-front! (-> list-queue? any/c void?)]
  [list-queue-add-back! (-> list-queue? any/c void?)]
  [list-queue-remove-front! (-> list-queue? void?)]
  [list-queue-remove-back! (-> list-queue? void?)]
  [list-queue-remove-all! (-> list-queue? proper-mlist?)]
  [list-queue-set-list! (case-> (-> list-queue? (or/c list? proper-mlist?) void?) (-> list-queue? proper-mlist? (or/c mpair? null?) void?))]
  [list-queue-append (-> list-queue? ... list-queue?)]
  [list-queue-append! (-> list-queue? ... list-queue?)]
  [list-queue-concatenate (-> (listof list-queue?) list-queue?)]
  [list-queue-map (-> (-> any/c any/c) list-queue? list-queue?)]
  [list-queue-map! (-> (-> any/c any/c) list-queue? list-queue?)]
  [list-queue-for-each (-> (-> any/c any/c) list-queue? void?)]
  [in-list-queue (-> list-queue? sequence?)]
  ))

(struct lqueue (first last)
  #:mutable
  #:sealed
  #:property prop:sequence
  (lambda (s) (in-list-queue s))
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer (lambda (obj) 'list-queue) (lambda (obj) (in-list-queue obj))))])

(define (in-list-queue lq)
  (in-mlist (lqueue-first lq)))

(define list-queue? lqueue?)

(define (mpair-in-mlist? lst p)
  (let loop ([lst lst])
    (cond
      [(null? lst) #f]
      [(eq? lst p) #t]
      [else (loop (mcdr lst))])))

(define/match (list->mlist/last lst)
  [('()) (values '() '())]
  [((cons first rest))
   (define head (mcons first '()))
   (let loop ([last head]
              [lst rest])
     (match lst
       ['()
        (values head last)]
       [(cons kar kdr)
        (let ([new-last (mcons kar '())])
          (set-mcdr! last new-last)
          (loop new-last kdr))]))])

(define make-list-queue
  (case-lambda
    [(source)
     (cond
       [(null? source) (lqueue '() '())]
       [(pair? source)
        (let-values ([(first last) (list->mlist/last source)])
          (lqueue first last))]
       [(mpair? source)
        (lqueue source (last-mpair source))])]
    [(first last)
     (cond
       [(and (null? first) (null? last))
        (lqueue '() '())]
       [(null? first)
        (raise-arguments-error 'make-list-queue "null list with non-null last" "list" first "last" last)]
       [(null? last)
        (raise-arguments-error 'make-list-queue "non-null list with null last" "list" first "last" last)]
       #;[(not (eq? (mcdr last) '()))
        (raise-argument-error 'make-list-queue "(eq? (mcdr last) '())" last)]
       [else #;(mpair-in-mlist? first last)
        (lqueue first last)]
       #;[else
        (raise-arguments-error 'make-list-queue "last pair not present in list" "list" first "last" last)])]))

(define (list-queue . elems)
  (make-list-queue elems))

(define/match (list-queue-copy lq)
  [((lqueue '() '())) (lqueue '() '())]
  [((lqueue first last))
   (let ([new-mlist (mlist-copy first)])
     (lqueue new-mlist (last-mpair new-mlist)))])

(define (list-queue-unfold stop? mapper successor seed [queue (lqueue '() '())])
  (let loop ([seed seed]
             [elems '()])
    (cond
      [(stop? seed)
       (for ([elem (in-list elems)])
         (list-queue-add-front! queue elem))
       queue]
      [else
       (loop (successor seed) (cons (mapper seed) elems))])))

(define (list-queue-unfold-right stop? mapper successor seed [queue (lqueue '() '())])
  (let loop ([seed seed]
             [elems '()])
    (cond
      [(stop? seed)
       (for ([elem (in-list elems)])
         (list-queue-add-back! queue elem))
       queue]
      [else
       (loop (successor seed) (cons (mapper seed) elems))])))

(define/match (list-queue-empty? lq)
  [((lqueue '() '())) #t]
  [(_) #f])

(define/match (list-queue-front lq)
  [((lqueue '() '()))
   (raise-arguments-error 'list-queue-front "(not/c list-queue-empty?)" lq)]
  [((lqueue first _))
   (mcar first)])

(define/match (list-queue-back lq)
  [((lqueue '() '()))
   (raise-arguments-error 'list-queue-back "(not/c list-queue-empty?)" lq)]
  [((lqueue _ last))
   (mcar last)])

(define (list-queue-list lq)
  (lqueue-first lq))

(define (list-queue-first-last lq)
  (values (lqueue-first lq) (lqueue-last lq)))

(define (list-queue-add-front! lq elem)
  (match lq
    [(lqueue '() '())
     (let ([new-pair (mcons elem '())])
       (set-lqueue-first! lq new-pair)
       (set-lqueue-last! lq new-pair))]
    [(lqueue first _)
     (set-lqueue-first! lq (mcons elem first))]))

(define (list-queue-add-back! lq elem)
  (match lq
    [(lqueue '() '())
     (let ([new-pair (mcons elem '())])
       (set-lqueue-first! lq new-pair)
       (set-lqueue-last! lq new-pair))]
    [(lqueue _ last)
     (define new-last (mcons elem '()))
     (set-mcdr! (lqueue-last lq) new-last)
     (set-lqueue-last! lq new-last)]))

(define/match (list-queue-remove-front! lq)
  [((lqueue '() '()))
    (raise-argument-error 'list-queue-remove-front! "(not/c list-queue-empty?)" lq)]
  [((lqueue first _))
   (set-lqueue-first! lq (mdrop first 1))
   (when (null? (lqueue-first lq))
     (set-lqueue-last! lq '()))])

(define/match (list-queue-remove-back! lq)
   [((lqueue '() '()))
    (raise-argument-error 'list-queue-remove-back! "(not/c list-queue-empty?)" lq)]
  [((lqueue first _))
   (set-lqueue-first! lq (mdrop-right! first 1))
   (if (null? (lqueue-first lq))
       (set-lqueue-last! lq '())
       (set-lqueue-last! lq (last-mpair (lqueue-first lq))))])

(define (list-queue-remove-all! lq)
  (let ([old-queue (lqueue-first lq)])
    (set-lqueue-first! lq '())
    (set-lqueue-last! lq '())
    old-queue))

(define list-queue-set-list!
  (case-lambda
    [(lq new-list)
     (cond
       [(null? new-list) 
        (set-lqueue-first! lq '())
        (set-lqueue-last! lq '())]
       [(pair? new-list)
        (let-values ([(new-mlist new-last) (list->mlist/last new-list)])
          (set-lqueue-first! lq new-mlist)
          (set-lqueue-last! lq new-last))]
       [(mpair? new-list)
        (set-lqueue-first! lq new-list)
        (set-lqueue-last! lq (last-mpair new-list))])]
    [(lq new-mlist new-mlast)
     (cond
       [(and (null? new-mlist) (null? new-mlast))
        (set-lqueue-first! lq '())
        (set-lqueue-last! lq '())]
       [(null? new-mlist)
        (raise-arguments-error 'list-queue-set-list! "null list with non-null last" "list" new-mlist "last" new-mlast)]
       [(null? new-mlast)
        (raise-arguments-error 'list-queue-set-list! "non-null list with null last" "list" new-mlist "last" new-mlast)]
       [else #;(mpair-in-mlist? new-mlist new-mlast)
        (set-lqueue-first! lq new-mlist)
        (set-lqueue-last! lq new-mlast)]
       #;[else
        (raise-arguments-error 'make-list-queue "last pair not present in list" "list" new-mlist "last" new-mlast)])]))

(define (list-queue-append . lqs)
  (list-queue-concatenate lqs))

(define (list-queue-append! . lqs)
  (match (dropf lqs list-queue-empty?)
    ['()
     (lqueue '() '())]
    [(cons head rest)
     (for ([lq (in-list rest)]
           #:unless (list-queue-empty? lq))
       (set-mcdr! (lqueue-last head) (lqueue-first lq))
       (set-lqueue-last! head (lqueue-last lq)))
     head]))
     
(define (list-queue-concatenate lqs)
  (define new-lq (list-queue))
  (for* ([lq (in-list lqs)]
         [elem (in-mlist (lqueue-first lq))])
    (list-queue-add-back! new-lq elem))
  new-lq)

(define (list-queue-map proc lq)
  (match lq
    [(lqueue '() '()) (lqueue '() '())]
    [(lqueue (mcons first-elem rest) _)
     (define new-mlist (mcons (proc first-elem) '()))
     (lqueue new-mlist
             (for/fold ([last new-mlist])
                       ([elem (in-mlist rest)])
               (define new-last (mcons (proc elem) '()))
               (set-mcdr! last new-last)
               new-last))]))

(define (list-queue-map! proc lq)
  (mpair-for-each (lambda (pair) (set-mcar! pair (proc (mcar pair)))) (lqueue-first lq))
  lq)

(define (list-queue-for-each proc lq)
  (mfor-each proc (lqueue-first lq)))

(module+ test
  ;;;; SPDX-FileCopyrightText: 2017 Alex Shinn
  ;;;;
  ;;;; SPDX-License-Identifier: BSD-3-Clause

  (test-group "list-queues"

              (test-group "list-queues/simple"
                          (test (mlist 1 1 1) (list-queue-list (make-list-queue '(1 1 1))))
                          (define x (list-queue 1 2 3))
                          (test (mlist 1 2 3) (list-queue-list x))
                          (define x1 (mlist 1 2 3))
                          (define x2 (make-list-queue x1 (mcddr x1)))
                          (test 3 (list-queue-back x2))
                          (define y (list-queue 4 5))
                          (test-assert (list-queue? y))
                          (define z (list-queue-append x y))
                          (test (mlist 1 2 3 4 5) (list-queue-list z))
                          (define z2 (list-queue-append! x (list-queue-copy y)))
                          (test (mlist 1 2 3 4 5) (list-queue-list z2))
                          (test 1 (list-queue-front z))
                          (test 5 (list-queue-back z))
                          (list-queue-remove-front! y)
                          (test (mlist 5) (list-queue-list y))
                          (list-queue-remove-back! y)
                          (test-assert (list-queue-empty? y))
                          (test-error (list-queue-remove-front! y))
                          (test-error (list-queue-remove-back! y))
                          (test (mlist 1 2 3 4 5) (list-queue-list z))
                          (test (mlist 1 2 3 4 5) (list-queue-remove-all! z2))
                          (test-assert (list-queue-empty? z2))
                          (list-queue-remove-all! z)
                          (list-queue-add-front! z 1)
                          (list-queue-add-front! z 0)
                          (list-queue-add-back! z 2)
                          (list-queue-add-back! z 3)
                          (test (mlist 0 1 2 3) (list-queue-list z))
                          ) ; end list-queues/simple

              (test-group "list-queues/whole"
                          (define a (list-queue 1 2 3))
                          (define b (list-queue-copy a))
                          (test (mlist 1 2 3) (list-queue-list b))
                          (list-queue-add-front! b 0)
                          (test (mlist 1 2 3) (list-queue-list a))
                          (test 4 (mlength (list-queue-list b)))
                          (define c (list-queue-concatenate (list a b)))
                          (test (mlist 1 2 3 0 1 2 3) (list-queue-list c))
                          ) ; end list-queues/whole

              (test-group "list-queues/map"
                          (define r (list-queue 1 2 3))
                          (define s (list-queue-map (lambda (x) (* x 10)) r))
                          (test (mlist 10 20 30) (list-queue-list s))
                          (list-queue-map! (lambda (x) (+ x 1)) r)
                          (test (mlist 2 3 4) (list-queue-list r))
                          (define sum 0)
                          (list-queue-for-each (lambda (x) (set! sum (+ sum x))) s)
                          (test 60 sum)
                          ) ; end list-queues/map

              (test-group "list-queues/conversion"
                          (define n (list-queue 5 6))
                          (list-queue-set-list! n (list 1 2))
                          (test (mlist 1 2) (list-queue-list n))
                          (define d (mlist 1 2 3))
                          (define e (mcddr d))
                          (define f (make-list-queue d e))
                          (define-values (dx ex) (list-queue-first-last f))
                          (test-assert (eq? d dx))
                          (test-assert (eq? e ex))
                          (test (mlist 1 2 3) (list-queue-list f))
                          (list-queue-add-front! f 0)
                          (list-queue-add-back! f 4)
                          (test (mlist 0 1 2 3 4) (list-queue-list f))
                          (define g (make-list-queue d e))
                          (test (mlist 1 2 3 4) (list-queue-list g))
                          (define h (list-queue 5 6))
                          (list-queue-set-list! h d e)
                          (test (mlist 1 2 3 4) (list-queue-list h))
                          ); end list-queues/conversion

              (test-group "list-queues/unfold"
                          (define (double x) (* x 2))
                          (define (done? x) (> x 3))
                          (define (add1 x) (+ x 1))
                          (define x (list-queue-unfold done? double add1 0))
                          (test (mlist 0 2 4 6) (list-queue-list x))
                          (define y (list-queue-unfold-right done? double add1 0))
                          (test (mlist 6 4 2 0) (list-queue-list y))
                          (define x0 (list-queue 8))
                          (define x1 (list-queue-unfold done? double add1 0 x0))
                          (test (mlist 0 2 4 6 8) (list-queue-list x1))
                          (define y0 (list-queue 8))
                          (define y1 (list-queue-unfold-right done? double add1 0 y0))
                          (test (mlist 8 6 4 2 0) (list-queue-list y1))

                          ) ; end list-queues/unfold

              ) ; end list-queues

  )