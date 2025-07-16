#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Linus Björnstam
;;
;; You may use this code under either the license in the SRFI document or the
;; license below.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ported to Racket by Shawn Wagner 2024

(require racket/contract racket/dict racket/sequence racket/set racket/treelist racket/undefined
         "133.rkt" (only-in "158.rkt" generator?) "171/meta.rkt")
(module+ test (require "private/testwrappers.rkt" srfi/1))

(provide
 (contract-out
  #:unprotected-submodule unsafe
  [list-transduce (case->
                   (-> transducer/c reducer/c list? any/c)
                   (-> transducer/c reducer/c any/c list? any/c))]
  [vector-transduce (case->
                     (-> transducer/c reducer/c vector? any/c)
                     (-> transducer/c reducer/c any/c vector? any/c))]
  [string-transduce (case->
                     (-> transducer/c reducer/c string? any/c)
                     (-> transducer/c reducer/c any/c string? any/c))]
  [bytevector-u8-transduce (case->
                    (-> transducer/c reducer/c bytes? any/c)
                    (-> transducer/c reducer/c any/c bytes? any/c))]

  [bytes-transduce (case->
                    (-> transducer/c reducer/c bytes? any/c)
                    (-> transducer/c reducer/c any/c bytes? any/c))]
  [port-transduce (case->
                   (-> transducer/c reducer/c (-> any/c any/c) any/c)
                   (-> transducer/c reducer/c (-> any/c any/c) any/c any/c)
                   (-> transducer/c reducer/c any/c (-> any/c any/c) any/c any/c))]
  [generator-transduce (case->
                        (-> transducer/c reducer/c generator? any/c)
                        (-> transducer/c reducer/c any/c generator? any/c))]

  [rcons reducer/c]
  [reverse-rcons reducer/c]
  [rany (-> (-> any/c any/c) reducer/c)]
  [revery (-> (-> any/c any/c) reducer/c)]
  [rcount reducer/c]

  [tmap (-> (-> any/c any/c) transducer/c)]
  [tfilter (-> (-> any/c any/c) transducer/c)]
  [tremove (-> (-> any/c any/c) transducer/c)]
  [tfilter-map (-> (-> any/c any/c) transducer/c)]
  [treplace (-> (or/c dict? (-> any/c any/c)) transducer/c)]
  [tdrop (-> exact-nonnegative-integer? transducer/c)]
  [ttake (-> exact-nonnegative-integer? transducer/c)]
  [tdrop-while (-> (-> any/c any/c) transducer/c)]
  [ttake-while (->* ((-> any/c any/c)) ((-> any/c any/c any/c)) transducer/c)]
  [tconcatenate transducer/c]
  [tappend-map (-> (-> any/c list?) transducer/c)]
  [tflatten transducer/c]
  [tdelete-neighbor-duplicates (->* () ((-> any/c any/c any/c)) transducer/c)]
  [tdelete-duplicates (->* () ((-> any/c any/c any/c)) transducer/c)]
  [tsegment (-> exact-positive-integer? transducer/c)]
  [tpartition (-> (-> any/c any/c) transducer/c)]
  [tadd-between (-> any/c transducer/c)]
  [tenumerate (->* () (exact-integer?) transducer/c)]
  [tlog (->* () ((-> any/c any/c any/c)) transducer/c)]

  ;; extra functions
  [set-transduce (case->
                  (-> transducer/c reducer/c set? any/c)
                  (-> transducer/c reducer/c any/c set? any/c))]
  [treelist-transduce (case->
                  (-> transducer/c reducer/c treelist? any/c)
                  (-> transducer/c reducer/c any/c treelist? any/c))]
  [hash-transduce (case->
                   (-> transducer/c reducer/c hash? any/c)
                   (-> transducer/c reducer/c any/c hash? any/c))]
  [sequence-transduce (case->
                        (-> transducer/c reducer/c (sequence/c any/c) any/c)
                        (-> transducer/c reducer/c any/c (sequence/c any/c) any/c))]
  ))

(define nothing undefined)
(define (nothing? x) (eq? x undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reducing functions meant to be used at the end at the transducing
;; process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a transducer-friendly cons with the empty list as identity
(define rcons
  (case-lambda
    (() '())
    ((lst) (reverse lst))
    ((lst x) (cons x lst))))

(define reverse-rcons
  (case-lambda
    (() '())
    ((lst) lst)
    ((lst x) (cons x lst))))

;; Use this as the f in transduce to count the amount of elements passed through.
;; (transduce (tfilter odd?) tcount (list 1 2 3)) => 2
(define rcount
  (case-lambda
    (() 0)
    ((result) result)
    ((result input)
     (+ 1 result))))

;; These two take a predicate and returns reducing functions that behave
;; like any and every from srfi-1
(define (rany pred)
  (case-lambda
    (() #f)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if test
           (reduced test)
           #f)))))

(define (revery pred)
  (case-lambda
    (() #t)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if (and result test)
           test
           (reduced #f))))))

(define list-transduce
  (case-lambda
    ((xform f coll)
     (list-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (list-reduce xf init coll)))
       (xf result)))))

(define vector-transduce
  (case-lambda
    ((xform f coll)
     (vector-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (vector-reduce xf init coll)))
       (xf result)))))

(define string-transduce
  (case-lambda
    ((xform f coll)
     (string-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (string-reduce xf init coll)))
       (xf result)))))

(define bytes-transduce
  (case-lambda
    ((xform f coll)
     (bytes-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (bytes-reduce xf init coll)))
       (xf result)))))

(define bytevector-u8-transduce bytes-transduce)

(define port-transduce
  (case-lambda
    ((xform f by)
     (generator-transduce xform f by))
    ((xform f by port)
     (port-transduce xform f (f) by port))
    ((xform f init by port)
     (let* ((xf (xform f))
            (result (port-reduce xf init by port)))
       (xf result)))))

(define generator-transduce
  (case-lambda
    ((xform f gen)
     (generator-transduce xform f (f) gen))
    ((xform f init gen)
     (let* ((xf (xform f))
            (result (generator-reduce xf init gen)))
       (xf result)))))

(define set-transduce
  (case-lambda
    [(xform f set)
     (set-transduce xform f (f) set)]
    [(xform f init set)
     (let* ((xf (xform f))
            (result (set-reduce xf init set)))
       (xf result))]))

(define treelist-transduce
  (case-lambda
    [(xform f tl)
     (treelist-transduce xform f (f) tl)]
    [(xform f init tl)
     (let* ((xf (xform f))
            (result (treelist-reduce xf init tl)))
       (xf result))]))

(define hash-transduce
  (case-lambda
    [(xform f ht)
     (hash-transduce xform f (f) ht)]
    [(xform f init ht)
     (let* ([xf (xform f)]
            [result (hash-reduce xf init ht)])
       (xf result))]))

(define sequence-transduce
  (case-lambda
    [(xform f seq)
     (sequence-transduce xform f (f) seq)]
    [(xform f init seq)
     (let* ([xf (xform f)]
            [result (sequence-reduce xf init seq)])
       (xf result))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transducers!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmap f)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (reducer result (f input))))))

(define (tfilter pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (pred input)
           (reducer result input)
           result)))))

(define (tremove pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (not (pred input))
           (reducer result input)
           result)))))

(define (tfilter-map f)
  (compose (tmap f) (tfilter values)))

(define (make-replacer map)
  (cond
    ((dict? map)
     (lambda (x)
       (dict-ref map x x)))
    ((procedure? map) map)
    (else
     (error "Unsupported mapping in treplace" map))))

(define (treplace map)
  (tmap (make-replacer map)))

(define (tdrop n)
  (lambda (reducer)
    (let ((new-n (+ 1 n)))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (set! new-n (- new-n 1))
         (if (positive? new-n)
             result
             (reducer result input)))))))

(define (tdrop-while pred)
  (lambda (reducer)
    (let ((drop? #t))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (and (pred input) drop?)
             result
             (begin
               (set! drop? #f)
               (reducer result input))))))))

(define (ttake n)
  (lambda (reducer)
    ;; we need to reset new-n for every new transduction
    (let ((new-n n))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (let ((result (if (positive? new-n)
                           (reducer result input)
                           result)))
           (set! new-n (- new-n 1))
           (if (not (positive? new-n))
               (ensure-reduced result)
               result)))))))

(define (ttake-while pred [retf (lambda (result input) result)])
  (lambda (reducer)
    (let ((take? #t))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (and take? (pred input))
             (reducer result input)
             (begin
               (set! take? #f)
               (ensure-reduced (retf result input)))))))))

(define (tconcatenate reducer)
  (let ((preserving-reducer (preserving-reduced reducer)))
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (list-reduce preserving-reducer result input)))))

(define (tappend-map f)
  (compose1 (tmap f) tconcatenate))

;; Flattens everything and passes each value through the reducer
;; (list-transduce tflatten rcons (list 1 2 (list 3 4 '(5 6) 7 8))) => (1 2 3 4 5 6 7 8)
(define tflatten
  (lambda (reducer)
    (case-lambda
      (() '())
      ((result) (reducer result))
      ((result input)
       (if (list? input)
           (list-reduce (preserving-reduced (tflatten reducer)) result input)
           (reducer result input))))))

;; removes duplicate consecutive elements
(define (tdelete-neighbor-duplicates [equality-pred? equal?])
  (lambda (reducer)
    (let ((prev nothing))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (equality-pred? prev input)
             result
             (begin
               (set! prev input)
               (reducer result input))))))))

;; Deletes all duplicates that passes through.
(define (tdelete-duplicates [equality-pred? equal?])
  (lambda (reducer)
    (let ((already-seen (make-custom-hash equality-pred? equal-hash-code equal-secondary-hash-code)))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (dict-has-key? already-seen input)
             result
             (begin
               (dict-set! already-seen input #t)
               (reducer result input))))))))

;; Partitions the input into lists of N items. If the input stops it flushes whatever
;; it has collected, which may be shorter than n.
(define (tsegment n)
  (lambda (reducer)
    (let ((i 0)
          (collect (make-vector n)))
      (case-lambda
        (() (reducer))
        ((result)
         ;; if there is anything collected when we are asked to quit
         ;; we flush it to the remaining transducers
         (let ((result
                (if (zero? i)
                    result
                    (reducer result (vector->list collect 0 i)))))
           (set! i 0)
           ;; now finally, pass it downstreams
           (if (reduced? result)
               (reducer (unreduce result))
               (reducer result))))
        ((result input)
         (vector-set! collect i input)
         (set! i (+ i 1))
         ;; If we have collected enough input we can pass it on downstream
         (if (< i n)
             result
             (let ((next-input (vector->list collect 0 i)))
               (set! i 0)
               (reducer result next-input))))))))

(define (tpartition f)
  (lambda (reducer)
    (let* ((prev nothing)
           (collect '()))
      (case-lambda
        (() (reducer))
        ((result)
         (let ((result
                (if (null? collect)
                    result
                    (reducer result (reverse collect)))))
           (set! collect '())
           (if (reduced? result)
               (reducer (unreduce result))
               (reducer result))))
        ((result input)
         (let ((fout (f input)))
           (cond
             ((or (equal? fout prev) (nothing? prev)) ; collect
              (set! prev fout)
              (set! collect (cons input collect))
              result)
             (else ; flush what we collected already to the reducer
              (let ((next-input  (reverse collect)))
                (set! prev fout)
                (set! collect (list input))
                (reducer result next-input))))))))))

;; Interposes element between each value pushed through the transduction.
(define (tadd-between elem)
  (lambda (reducer)
    (let ((send-elem? #f))
      (case-lambda
        (() (reducer))
        ((result)
         (reducer result))
        ((result input)
         (if send-elem?
             (let ((result (reducer result elem)))
               (if (reduced? result)
                   result
                   (reducer result input)))
             (begin
               (set! send-elem? #t)
               (reducer result input))))))))

;; indexes every value passed through in a cons pair as in (index . value). By default starts at 0
(define (tenumerate [n 0])
  (lambda (reducer)
    (let ((n n))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (let ((input (cons n input)))
           (set! n (+ n 1))
           (reducer result input)))))))

(define (tlog [log-function (lambda (result input) (println input))])
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (log-function result input)
       (reducer result input)))))

(module+ test

  (define numeric-list (iota 5))
  (define numeric-vec (list->vector numeric-list))
  (define string "0123456789abcdef")
  (define list-of-chars (string->list string))
  (define replace-alist '((1 . s) (2 . c) (3 . h) (4 . e) (5 . m)))
  (define (replace-function val)
    (case val
      ((1) 's)
      ((2) 'c)
      ((3) 'h)
      ((4) 'e)
      ((5) 'm)
      (else val)))


  (test-begin "transducers")
  (test-equal '(1 2 3 4 5) (list-transduce (tmap add1) rcons numeric-list))
  (test-equal '(0 2 4) (list-transduce (tfilter even?) rcons numeric-list))
  (test-equal '(1 3 5) (list-transduce (compose (tfilter even?) (tmap add1)) rcons numeric-list))
  (test-equal '(1 3 5) (list-transduce (tfilter-map
                                        (lambda (x) (if (even? x) (+ x 1) #f))) rcons numeric-list))

  (test-equal (string-transduce (tmap char->integer) rcons string) (list-transduce (tmap char->integer) rcons list-of-chars))
  (test-equal 6 (string-transduce (tfilter char-alphabetic?) rcount string))
  (test-equal (list-transduce (tremove char-alphabetic?) rcount list-of-chars) (string-transduce (tremove char-alphabetic?) rcount string))
  (test-equal '(s c h e m e  r o c k s) (list-transduce (treplace replace-alist) rcons '(1 2 3 4 5 4 r o c k s) ))
  (test-equal '(s c h e m e  r o c k s) (list-transduce (treplace replace-function) rcons '(1 2 3 4 5 4 r o c k s) ))


  (test-equal 6 (list-transduce (ttake 4) + numeric-list))
  (test-equal 7 (list-transduce (tdrop 3) + numeric-list))

  (test-equal '(3 4) (list-transduce (tdrop-while (lambda (x) (< x 3))) rcons numeric-list))

  (test-equal '(0 1 2) (list-transduce (ttake-while (lambda (x) (< x 3))) rcons numeric-list))

  (test-equal '(0 1 2 3 4) (list-transduce tconcatenate rcons '((0 1) (2 3) (4))))

  (test-equal '(1 2 2 4 3 6) (list-transduce (tappend-map (lambda (x) (list x (* x 2)))) rcons '(1 2 3)))

  (test-equal '(1 2 1 2 3) (list-transduce (tdelete-neighbor-duplicates) rcons '(1 1 1 2 2 1 2 3 3)))

  (test-equal '(1 2 3 4) (list-transduce (tdelete-duplicates) rcons '(1 1 2 1 2 3 3 1 2 3 4 4)))

  (test-equal '(1 2 3 4 5 6 7 8 9) (list-transduce tflatten rcons '((1 2) 3 (4 (5 6) 7) 8 (9))))

  (test-equal '((1 1 1 1) (2 2 2 2) (3 3 3) (4 4 4 4)) (list-transduce (tpartition even?) rcons '(1 1 1 1 2 2 2 2 3 3 3 4 4 4 4)))

  (test-equal '((0 1) (2 3) (4)) (vector-transduce (tsegment 2) rcons numeric-vec))

  (test-equal '(0 and 1 and 2 and 3 and 4) (list-transduce (tadd-between 'and) rcons numeric-list))

  (test-equal '((-1 . 0) (0 . 1) (1 . 2) (2 . 3) (3 . 4)) (list-transduce (tenumerate (- 1)) rcons numeric-list))


  (test-equal 5 (set-transduce (tfilter integer?) rcount (list->set numeric-list)))
  (test-equal 5 (treelist-transduce (tfilter integer?) rcount (list->treelist numeric-list)))

  (test-end "transducers")



  (test-begin "x-transduce")
  (test-equal 15 (list-transduce (tmap add1) + numeric-list))
  (test-equal 15 (list-transduce (tmap add1) + 0 numeric-list))

  (test-equal 15 (vector-transduce (tmap add1) + numeric-vec))
  (test-equal 15 (vector-transduce (tmap add1) + 0 numeric-vec))

  ;; This should really close it's port. I know.
  (test-equal 15 (port-transduce (tmap add1) + read (open-input-string "0 1 2 3 4")))
  (test-equal 15 (port-transduce (tmap add1) + 0 read (open-input-string "0 1 2 3 4")))

  ;; Converts each numeric char to it's corresponding number (+ 1) and sums them.
  (test-equal 15 (string-transduce  (tmap (lambda (x) (- (char->integer x) 47))) + "01234"))
  (test-equal 15 (string-transduce  (tmap (lambda (x) (- (char->integer x) 47))) + 0 "01234"))

  (test-equal '(1 2 3) (parameterize ((current-input-port (open-input-string "1 2 3")))
                         (generator-transduce (tmap (lambda (x) x)) rcons read)))

  (test-equal '(1 2 3) (parameterize ((current-input-port (open-input-string "1 2 3")))
                         (generator-transduce (tmap (lambda (x) x)) rcons '() read)))

  (test-end "x-transduce")
  )