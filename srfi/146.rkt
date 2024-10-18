#lang racket/base

;;; Ordered mappings public interface

;;; For some reason, using srfi/146/main.rkt as the file name causes
;;; (require srfi/146) to fail. So, srfi/146.rkt it is.

(require racket/contract racket/dict racket/match data/order
         "128.rkt" "146/private/scapegoat.rkt" "146/private/common.rkt")
(module+ test (require "private/testwrappers.rkt"))

(provide
 comparator?
 mapping?
 mapping-key-comparator
 (all-from-out "146/private/common.rkt")
 (contract-out
  [in-ordered-dict (->* (ordered-dict?) (any/c) sequence?)]
  [in-ordered-dict-keys (->* (ordered-dict?) (any/c) sequence?)]
  [in-ordered-dict-values (->* (ordered-dict?) (any/c) sequence?)]

  [mapping (-> comparator? any/c ... mapping?)]
  [mapping/ordered (-> comparator? any/c ... mapping?)]
  [mapping-unfold (-> (-> any/c any/c) (-> any/c (values any/c any/c)) (-> any/c any/c) any/c comparator? mapping?)]
  [mapping-unfold/ordered (-> (-> any/c any/c) (-> any/c (values any/c any/c)) (-> any/c any/c) any/c comparator? mapping?)]
  [mapping-map (-> (-> any/c any/c (values any/c any/c)) comparator? ordered-dict? mapping?)]
  [mapping-map/monotone (-> (-> any/c any/c (values any/c any/c)) comparator? ordered-dict? mapping?)]
  [mapping-map/monotone! (-> (-> any/c any/c (values any/c any/c)) comparator? ordered-dict? mapping?)]
  [alist->mapping (-> comparator? (listof (cons/c any/c any/c)) mapping?)]
  [alist->mapping/ordered (-> comparator? (listof (cons/c any/c any/c)) mapping?)]
  [mapping=? (-> comparator? mapping? mapping? mapping? ... boolean?)]
  [mapping<? (-> comparator? mapping? mapping? mapping? ... boolean?)]
  [mapping<=? (-> comparator? mapping? mapping? mapping? ... boolean?)]
  [mapping>? (-> comparator? mapping? mapping? mapping? ... boolean?)]
  [mapping>=? (-> comparator? mapping? mapping? mapping? ... boolean?)]
  [make-mapping-comparator (-> comparator? comparator?)]
  [mapping-comparator comparator?]
  [mapping-range< (-> mapping? any/c mapping?)]
  [mapping-range<! (-> mapping? any/c mapping?)]
  [mapping-range<= (-> mapping? any/c mapping?)]
  [mapping-range<=! (-> mapping? any/c mapping?)]
  [mapping-split (-> mapping? any/c (values mapping? mapping? mapping? mapping? mapping?))]
  [mapping-split! (-> mapping? any/c (values mapping? mapping? mapping? mapping? mapping?))]
  [mapping-catenate (-> comparator? ordered-dict? any/c any/c ordered-dict? mapping?)]

  [mapping-adjoin (-> dict? any/c ... dict?)]
  [mapping-adjoin! (-> dict? any/c ... dict?)]
  [mapping-pop (->* (ordered-dict?) ((-> any)) any)]
  [mapping-pop! (->* (ordered-dict?) ((-> any)) any)]
  [mapping-find (-> (-> any/c any/c any/c) ordered-dict? (-> any) any)]
  [mapping-entries (-> ordered-dict? (values list? list?))]
  [mapping-fold (-> (-> any/c any/c any/c any/c) any/c ordered-dict? any/c)]
  [mapping-map->list (-> (-> any/c any/c any/c) ordered-dict? list?)]
  [mapping-partition (-> (-> any/c any/c any/c) ordered-dict? (values ordered-dict? ordered-dict?))]
  [mapping-partition! (-> (-> any/c any/c any/c) ordered-dict? (values ordered-dict? ordered-dict?))]

  [alist->mapping/ordered! (-> ordered-dict? (listof (cons/c any/c any/c)) ordered-dict?)]

  [mapping-min-key (-> ordered-dict? any/c)]
  [mapping-max-key (-> ordered-dict? any/c)]
  [mapping-min-value (-> ordered-dict? any/c)]
  [mapping-max-value (-> ordered-dict? any/c)]
  [mapping-min-entry (-> ordered-dict? (values any/c any/c))]
  [mapping-max-entry (-> ordered-dict? (values any/c any/c))]
  [mapping-key-predecessor (-> ordered-dict? any/c (-> any) any)]
  [mapping-key-successor (-> ordered-dict? any/c (-> any) any)]
  [mapping-range= (-> ordered-dict? any/c ordered-dict?)]
  [mapping-range=! (-> ordered-dict? any/c ordered-dict?)]
  [mapping-range> (-> ordered-dict? any/c ordered-dict?)]
  [mapping-range>! (-> ordered-dict? any/c ordered-dict?)]
  [mapping-range>= (-> ordered-dict? any/c ordered-dict?)]
  [mapping-range>=! (-> ordered-dict? any/c ordered-dict?)]
  [mapping-catenate! (-> ordered-dict? any/c any/c ordered-dict? ordered-dict?)]
  [mapping-fold/reverse (-> (-> any/c any/c any/c any/c) any/c ordered-dict? any/c)]
  ))

;;; Iterate starting with the least element, hopefully in ascending order.
(define (in-ordered-dict od [starting-pos (dict-iterate-least od)])
  (make-do-sequence
   (lambda ()
     (values
      (lambda (pos) (values (dict-iterate-key od pos) (dict-iterate-value od pos)))
      (lambda (pos) (dict-iterate-next od pos))
      starting-pos
      (lambda (pos) (not (eq? pos #f)))
      #f
      #f))))

(define (in-ordered-dict-keys od [starting-pos (dict-iterate-least od)])
  (make-do-sequence
   (lambda ()
     (values
      (lambda (pos) (dict-iterate-key od pos))
      (lambda (pos) (dict-iterate-next od pos))
      starting-pos
      (lambda (pos) (not (eq? pos #f)))
      #f
      #f))))

(define (in-ordered-dict-values od [starting-pos (dict-iterate-least od)])
  (make-do-sequence
   (lambda ()
     (values
      (lambda (pos) (dict-iterate-value od pos))
      (lambda (pos) (dict-iterate-next od pos))
      starting-pos
      (lambda (pos) (not (eq? pos #f)))
      #f
      #f))))

;;; internal routines

(define (%mapping-add-list name od keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error name "Needs 0 or more sets of key and value arguments"))
  (let loop ([od od]
             [keyvals keyvals])
    (match keyvals
      ((list) od)
      ((list* key val rest-keyvals)
       (if (dict-has-key? od key)
           (loop od rest-keyvals)
           (loop (dict-set od key val) rest-keyvals))))))

(define (%mapping-add-list! name od keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error name "Needs 0 or more sets of key and value arguments"))
  (let loop ([keyvals keyvals])
    (match keyvals
      ((list) od)
      ((list* key val rest-keyvals)
       (unless (dict-has-key? od key)
         (dict-set! od key val))
       (loop rest-keyvals)))))

;;; mapping specific routines. Use scapegoat-tree functions directly when provided for efficiency

(define (mapping cmp . keyvals)
  (unless (comparator-ordered? cmp)
    (raise-argument-error 'mapping "comparator-ordered?" cmp))
  (%mapping-add-list 'mapping (make-scapegoat-tree cmp) keyvals))

; TODO: Add optimized implementation
(define mapping/ordered (procedure-rename mapping 'mapping/ordered))

(define (mapping-unfold stop? mapper successor seed cmp)
  (unless (comparator-ordered? cmp)
    (raise-argument-error 'mapping-unfold "comparator-ordered?" cmp))
  (let ([st (make-scapegoat-tree cmp)])
    (let loop ([seed seed])
      (if (stop? seed)
          st
          (let-values ([(k v) (mapper seed)]
                       [(next-seed) (successor seed)])
            (if (scapegoat-tree-has-key? st k)
                (loop next-seed)
                (begin
                  (scapegoat-tree-set! st k v)
                  (loop next-seed))))))))

(define mapping-unfold/ordered (procedure-rename mapping-unfold 'mapping-unfold/ordered))

(define (mapping-map proc cmp st)
  (unless (comparator-ordered? cmp)
    (raise-argument-error 'mapping-map "comparator-ordered?" cmp))
  (let ([new-st (make-scapegoat-tree cmp)])
    (for ([(k v) (in-ordered-dict st)])
      (let-values ([(new-k new-v) (proc k v)])
        (scapegoat-tree-set! new-st new-k new-v)))
    new-st))

(define mapping-map/monotone (procedure-rename mapping-map 'mapping-map/monotone))
(define mapping-map/monotone! (procedure-rename mapping-map 'mapping-map/monotone!))

(define (alist->mapping cmp alist)
  (unless (comparator-ordered? cmp)
    (raise-argument-error 'alist->mapping "comparator-ordered?" cmp))
  (let ([st (make-scapegoat-tree cmp)])
    (for ([pair (in-list alist)]
          #:when (not (scapegoat-tree-has-key? st (car pair))))
      (scapegoat-tree-set! st (car pair) (cdr pair)))
    st))

; TODO: Add optimized implementation
(define alist->mapping/ordered (procedure-rename alist->mapping 'alist->mapping/ordered))
(define alist->mapping/ordered! (procedure-rename alist->mapping! 'alist->mapping/ordered!))

(define (%binary-mapping=? cmp st1 st2)
  (and
   (= (dict-count st1) (dict-count st2))
   (eq? (mapping-key-comparator st1) (mapping-key-comparator st2))
     (for/and ([(k1 v1) (in-ordered-dict st1)])
       (and (scapegoat-tree-has-key? st2 k1)
            (=? cmp v1 (scapegoat-tree-ref st2 k1))))))

(define (mapping=? cmp st1 st2 . rest)
  (and (%binary-mapping=? cmp st1 st2)
       (match rest
         ((list) #t)
         ((list st3) (%binary-mapping=? cmp st1 st3))
         ((list* st3 rest) (apply mapping=? cmp st1 st3 rest)))))

(define (%binary-mapping<? cmp st1 st2)
  (and
   (< (dict-count st2) (dict-count st1))
   (eq? (mapping-key-comparator st1) (mapping-key-comparator st2))
   (for/and ([(k2 v2) (in-ordered-dict st2)])
     (and (scapegoat-tree-has-key? st1 k2)
          (=? cmp v2 (scapegoat-tree-ref st1 k2))))))

(define (%mapping<? cmp mappings)
  (match mappings
    ((list st1 st2) (%binary-mapping<? cmp st1 st2))
    ((list* st1 st2 rest)
     (and (%binary-mapping<? cmp st1 st2)
          (%mapping<? cmp (cons st2 rest))))))

(define (mapping<? cmp . mappings)
  (%mapping<? cmp (reverse mappings)))

(define (mapping<=? cmp . mappings)
  (%mapping<=? cmp (reverse mappings)))

(define (%binary-mapping<=? cmp st1 st2)
  (and
   (<= (dict-count st2) (dict-count st1))
   (eq? (mapping-key-comparator st1) (mapping-key-comparator st2))
   (for/and ([(k2 v2) (in-ordered-dict st2)])
     (and (scapegoat-tree-has-key? st1 k2)
          (=? cmp v2 (scapegoat-tree-ref st1 k2))))))

(define (%mapping<=? cmp mappings)
  (match mappings
    ((list st1 st2) (%binary-mapping<=? cmp st1 st2))
    ((list* st1 st2 rest)
     (and (%binary-mapping<=? cmp st1 st2)
          (%mapping<=? cmp (cons st2 rest))))))

(define (mapping>? cmp . mappings)
  (%mapping>? cmp (reverse mappings)))

(define (%binary-mapping>? cmp st1 st2)
  (and
   (> (dict-count st2) (dict-count st1))
   (eq? (mapping-key-comparator st1) (mapping-key-comparator st2))
   (for/and ([(k1 v1) (in-ordered-dict st1)])
     (and (scapegoat-tree-has-key? st2 k1)
          (=? cmp v1 (scapegoat-tree-ref st2 k1))))))

(define (%mapping>? cmp mappings)
  (match mappings
    ((list st1 st2) (%binary-mapping>? cmp st1 st2))
    ((list* st1 st2 rest)
     (and (%binary-mapping>? cmp st1 st2)
          (%mapping>? cmp (cons st2 rest))))))

(define (mapping>=? cmp . mappings)
  (%mapping>=? cmp (reverse mappings)))

(define (%binary-mapping>=? cmp st1 st2)
  (and
   (>= (dict-count st2) (dict-count st1))
   (eq? (mapping-key-comparator st1) (mapping-key-comparator st2))
   (for/and ([(k1 v1) (in-ordered-dict st1)])
     (and (scapegoat-tree-has-key? st2 k1)
          (=? cmp v1 (scapegoat-tree-ref st2 k1))))))

(define (%mapping>=? cmp mappings)
  (match mappings
    ((list st1 st2) (%binary-mapping>=? cmp st1 st2))
    ((list* st1 st2 rest)
     (and (%binary-mapping>=? cmp st1 st2)
          (%mapping>=? cmp (cons st2 rest))))))

(define (mapping-range< od key)
  (let ([end-pos (dict-iterate-greatest/<? od key)])
    (if end-pos
        (let ([end-key (scapegoat-tree-iterate-key od end-pos)]
              [cmp (mapping-key-comparator od)])
          (for/fold ([new-od (scapegoat-tree-clear od)])
                    ([(k v) (in-ordered-dict od)]
                     #:break (>? cmp k end-key))
            (scapegoat-tree-set new-od k v)))
        (scapegoat-tree-clear od))))

(define (mapping-range<! od key)
  (let ([end-pos (dict-iterate-greatest/<? od key)])
    (if end-pos
        (let ([end-key (scapegoat-tree-iterate-key od end-pos)]
              [cmp (mapping-key-comparator od)]
              [new-od (scapegoat-tree-clear od)])
          (for ([(k v) (in-ordered-dict od)]
                #:break (>? cmp k end-key))
            (scapegoat-tree-set! new-od k v))
          new-od)
        (scapegoat-tree-clear od))))

(define (mapping-range<= od key)
  (let ([end-pos (dict-iterate-greatest/<=? od key)])
    (if end-pos
        (let ([end-key (scapegoat-tree-iterate-key od end-pos)]
              [cmp (mapping-key-comparator od)])
          (for/fold ([new-od (scapegoat-tree-clear od)])
                    ([(k v) (in-ordered-dict od)]
                     #:break (>? cmp k end-key))
            (scapegoat-tree-set new-od k v)))
        (scapegoat-tree-clear od))))

(define (mapping-range<=! od key)
  (let ([end-pos (dict-iterate-greatest/<=? od key)])
    (if end-pos
        (let ([end-key (scapegoat-tree-iterate-key od end-pos)]
              [cmp (mapping-key-comparator od)]
              [new-od (scapegoat-tree-clear od)])
          (for ([(k v) (in-ordered-dict od)]
                #:break (>? cmp k end-key))
            (scapegoat-tree-set! new-od k v))
          new-od)
        (scapegoat-tree-clear od))))

(define (mapping-split od key)
  (let ([empty-od (scapegoat-tree-clear od)]
        [cmp (mapping-key-comparator od)])
    (for/fold ([lt empty-od]
               [lte empty-od]
               [eq empty-od]
               [gte empty-od]
               [gt empty-od])
              ([(k v) (in-ordered-dict od)])
      (cond
        ((<? cmp k key)
         (values (scapegoat-tree-set lt k v) (scapegoat-tree-set lte k v) eq gte gt))
        ((=? cmp k key)
         (values lt (scapegoat-tree-set lte k v) (scapegoat-tree-set eq k v) (scapegoat-tree-set gte k v) gt))
        (else
         (values lt lte eq (scapegoat-tree-set gte k v) (scapegoat-tree-set gt k v)))))))

(define (mapping-split! od key)
  (let ([lt (scapegoat-tree-clear od)]
        [lte (scapegoat-tree-clear od)]
        [eq (scapegoat-tree-clear od)]
        [gte (scapegoat-tree-clear od)]
        [gt (scapegoat-tree-clear od)]
        [cmp (mapping-key-comparator od)])
    (for ([(k v) (in-ordered-dict od)])
      (cond
        ((<? cmp k key)
         (scapegoat-tree-set! lt k v)
         (scapegoat-tree-set! lte k v))
        ((=? cmp k key)
         (scapegoat-tree-set! lte k v)
         (scapegoat-tree-set! eq k v)
         (scapegoat-tree-set! gte k v))
        (else
         (scapegoat-tree-set! gte k v)
         (scapegoat-tree-set! gt k v))))
    (values lt lte eq gte gt)))

(define (mapping-catenate cmp od1 k v od2)
  (unless (comparator-ordered? cmp)
    (raise-argument-error 'mapping-catenate "comparator-ordered?" cmp))
  (let* ([new-od (make-scapegoat-tree cmp)]
         [new-od (mapping-union new-od od1)]
         [new-od (scapegoat-tree-set new-od k v)])
    (mapping-union new-od od2)))

#;(define (%mapping-lt? value-cmp a b)
  (and
   (<= (dict-count a) (dict-count b))
   (eq? (mapping-key-comparator a) (mapping-key-comparator b))
   (let ([key-cmp (mapping-key-comparator a)])
     (let loop ([a-iter (dict-iterate-least a)]
                [b-iter (dict-iterate-least b)])
       (cond
         ((and (eq? a-iter #f) b-iter) #f)
         ((and a-iter (eq? b-iter #f)) #t)
         ((and (eq? a-iter #f) (eq? b-iter #f)) #f)
         ((<? key-cmp (dict-iterate-key a a-iter) (dict-iterate-key b b-iter)) #t)
         ((=? key-cmp (dict-iterate-key a a-iter) (dict-iterate-key b b-iter))
          (cond
            ((<? value-cmp (dict-iterate-value a a-iter) (dict-iterate-value b b-iter)) #t)
            ((=? value-cmp (dict-iterate-value a a-iter) (dict-iterate-value b b-iter))
             (loop (dict-iterate-next a a-iter) (dict-iterate-next b b-iter)))
            (else #f)))
         (else #f))))))

(define (make-mapping-comparator cmp)
  (make-comparator mapping?
                   (lambda (a b) (%binary-mapping=? cmp a b))
                   #f ;(lambda (a b) (%mapping-lt? cmp a b))
                   equal-hash-code
                   #:secondary-hash equal-secondary-hash-code))

(define mapping-comparator (make-mapping-comparator default-comparator))
(comparator-register-default! mapping-comparator)

;;; generic ordered dict procedures

(define (mapping-adjoin od . keyvals)
  (%mapping-add-list 'mapping-adjoin od keyvals))

(define (mapping-adjoin! od . keyvals)
  (if (dict-implements? od 'dict-set!)
      (%mapping-add-list! 'mapping-adjoin! od keyvals)
      (%mapping-add-list 'mapping-adjoin! od keyvals)))

(define (mapping-pop od [failure (lambda () (raise-argument-error 'mapping-pop "(not/c mapping-empty?)" od))])
  (if (dict-empty? od)
      (failure)
      (let* ([pos (dict-iterate-least od)]
             [key (dict-iterate-key od pos)]
             [val (dict-iterate-value od pos)])
        (values (dict-remove od key) key val))))

(define (mapping-pop! od [failure (lambda () (raise-argument-error 'mapping-pop! "(not/c mapping-empty?)" od))])
  (if (dict-empty? od)
      (failure)
      (let* ([pos (dict-iterate-least od)]
             [key (dict-iterate-key od pos)]
             [val (dict-iterate-value od pos)])
        (if (dict-implements? od 'dict-remove!)
            (begin
              (dict-remove! od key)
              (values od key val))
            (values (dict-remove od key) key val)))))

(define (mapping-find pred? od failure)
  (let ([found (for/first ([(k v) (in-ordered-dict od)]
                           #:when (pred? k v))
                 (cons k v))])
    (if found
        (values (car found) (cdr found))
        (failure))))

(define (mapping-entries od)
  (for/lists (keys vals)
             ([(k v) (in-ordered-dict od)])
    (values k v)))

(define (mapping-fold kons knil od)
  (for/fold ([seed knil])
            ([(k v) (in-ordered-dict od)])
    (kons k v seed)))

(define (mapping-map->list proc od)
  (for/list ([(k v) (in-ordered-dict od)])
    (proc k v)))

(define (mapping-partition pred? od)
  (for/fold ([yes-od (dict-clear od)]
             [no-od (dict-clear od)])
            ([(k v) (in-ordered-dict od)])
    (if (pred? k v)
        (values (dict-set yes-od k v) no-od)
        (values yes-od (dict-set no-od k v)))))

(define (mapping-partition! pred? od)
  (if (dict-implements? od 'dict-set! 'dict-clear)
      (let ([yes-od (dict-clear od)]
            [no-od (dict-clear od)])
        (for ([(k v) (in-ordered-dict od)])
          (if (pred? k v)
              (dict-set! yes-od k v)
              (dict-set! no-od k v)))
        (values yes-od no-od))
      (mapping-partition pred? od)))

(define (mapping-min-key od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-min-key "(not/c dict-empty?)" od))
  (dict-iterate-key od (dict-iterate-least od)))

(define (mapping-max-key od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-max-key "(not/c dict-empty?)" od))
  (dict-iterate-key od (dict-iterate-greatest od)))

(define (mapping-min-value od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-min-value "(not/c dict-empty?)" od))
  (dict-iterate-value od (dict-iterate-least od)))

(define (mapping-max-value od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-max-value "(not/c dict-empty?)" od))
  (dict-iterate-value od (dict-iterate-greatest od)))

(define (mapping-min-entry od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-min-entry "(not/c dict-empty?)" od))
  (let ([pos (dict-iterate-least od)])
    (values (dict-iterate-key od pos) (dict-iterate-value od pos))))

(define (mapping-max-entry od)
  (when (dict-empty? od)
    (raise-argument-error 'mapping-max-entry "(not/c dict-empty?)" od))
  (let ([pos (dict-iterate-greatest od)])
    (values (dict-iterate-key od pos) (dict-iterate-value od pos))))

(define (mapping-key-predecessor od key failure)
  (let ([pred (dict-iterate-greatest/<? od key)])
    (if pred
        (dict-iterate-key od pred)
        (failure))))

(define (mapping-key-successor od key failure)
  (let ([pred (dict-iterate-least/>? od key)])
    (if pred
        (dict-iterate-key od pred)
        (failure))))

(define (mapping-range= od key)
  (let ([new-od (dict-clear od)])
    (if (dict-has-key? od key)
        (dict-set new-od key (dict-ref od key))
        new-od)))

(define (mapping-range=! od key)
  (if (dict-implements? od 'dict-set! 'dict-clear)
      (let ([new-od (dict-clear od)])
        (when (dict-has-key? od key)
          (dict-set! new-od key (dict-ref od key)))
        new-od)
      (mapping-range= od key)))

(define (mapping-range> od key)
  (for/fold ([new-od (dict-clear od)])
            ([(k v) (in-ordered-dict od (dict-iterate-least/>? od key))])
    (dict-set new-od k v)))

(define (mapping-range>! od key)
  (if (dict-implements? od 'dict-set! 'dict-clear)
      (let ([new-od (dict-clear od)])
        (for ([(k v) (in-ordered-dict od (dict-iterate-least/>? od key))])
          (dict-set new-od k v))
        new-od)
      (mapping-range> od key)))

(define (mapping-range>= od key)
  (for/fold ([new-od (dict-clear od)])
            ([(k v) (in-ordered-dict od (dict-iterate-least/>=? od key))])
    (dict-set new-od k v)))

(define (mapping-range>=! od key)
  (if (dict-implements? od 'dict-set! 'dict-clear)
      (let ([new-od (dict-clear od)])
        (for ([(k v) (in-ordered-dict od (dict-iterate-least/>=? od key))])
          (dict-set new-od k v))
        new-od)
      (mapping-range>= od key)))

(define (mapping-catenate! od k v od2)
  (if (dict-implements? od 'dict-set!)
      (begin
        (dict-set! od k v)
        (mapping-union! od od2))
      (mapping-union (dict-set od k v) od2)))

(define (mapping-fold/reverse kons knil od)
  (for/fold ([seed knil])
            ([key (in-list (reverse (mapping-keys od)))])
    (kons key (dict-ref od key) seed)))

(module+ test
  ;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).  All Rights
  ;; Reserved.

  ;; Permission is hereby granted, free of charge, to any person
  ;; obtaining a copy of this software and associated documentation
  ;; files (the "Software"), to deal in the Software without
  ;; restriction, including without limitation the rights to use, copy,
  ;; modify, merge, publish, distribute, sublicense, and/or sell copies
  ;; of the Software, and to permit persons to whom the Software is
  ;; furnished to do so, subject to the following conditions:

  ;; The above copyright notice and this permission notice shall be
  ;; included in all copies or substantial portions of the Software.

  ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  ;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  ;; SOFTWARE.

  (require srfi/1 srfi/8)

  (define comparator (make-default-comparator))

  (test-group "Predicates"
              (define mapping0 (mapping comparator))
              (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
              (define mapping2 (mapping comparator 'c 1 'd 2 'e 3))
              (define mapping3 (mapping comparator 'd 1 'e 2 'f 3))

              (test-assert "mapping?: a mapping"
                           (mapping? (mapping comparator)))

              (test-assert "mapping?: not a mapping"
                           (not (mapping? (list 1 2 3))))

              (test-assert "mapping-empty?: empty mapping"
                           (mapping-empty? mapping0))

              (test-assert "mapping-empty?: non-empty mapping"
                           (not (mapping-empty? mapping1)))

              (test-assert "mapping-contains?: containing"
                           (mapping-contains? mapping1 'b))

              (test-assert "mapping-contains?: not containing"
                           (not (mapping-contains? mapping1 '2)))

              (test-assert "mapping-disjoint?: disjoint"
                           (mapping-disjoint? mapping1 mapping3))

              (test-assert "mapping-disjoint?: not disjoint"
                           (not (mapping-disjoint? mapping1 mapping2))))

  (test-group "Accessors"
              (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))

              (test-equal "mapping-ref: key found"
                          2
                          (mapping-ref mapping1 'b))

              (test-equal "mapping-ref: key not found/with failure"
                          42
                          (mapping-ref mapping1 'd (lambda () 42)))

              (test-error "mapping-ref: key not found/without failure"
                          (mapping-ref mapping1 'd))

              (test-equal "mapping-ref: with success procedure"
                          (* 2 2)
                          (mapping-ref mapping1 'b (lambda () #f) (lambda (x) (* x x))))

              (test-equal "mapping-ref/default: key found"
                          3
                          (mapping-ref/default mapping1 'c 42))

              (test-equal "mapping-ref/default: key not found"
                          42
                          (mapping-ref/default mapping1 'd 42))

              (test-equal "mapping-key-comparator"
                          comparator
                          (mapping-key-comparator mapping1)))

  (test-group "Updaters"
              (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
              (define mapping2 (mapping-set mapping1 'c 4 'd 4 'd 5))
              (define mapping3 (mapping-update mapping1 'b (lambda (x) (* x x))))
              (define mapping4 (mapping-update/default mapping1 'd (lambda (x) (* x x)) 4))
              (define mapping5 (mapping-adjoin mapping1 'c 4 'd 4 'd 5))
              (define mapping0 (mapping comparator))

              (test-equal "mapping-adjoin: key already in mapping"
                          3
                          (mapping-ref mapping5 'c))

              (test-equal "mapping-adjoin: key set earlier"
                          4
                          (mapping-ref mapping5 'd))

              (test-equal "mapping-set: key already in mapping"
                          4
                          (mapping-ref mapping2 'c))

              (test-equal "mapping-set: key set earlier"
                          5
                          (mapping-ref mapping2 'd))

              (test-equal "mapping-replace: key not in mapping"
                          #f
                          (mapping-ref/default (mapping-replace mapping1 'd 4) 'd #f))

              (test-equal "mapping-replace: key in mapping"
                          6
                          (mapping-ref (mapping-replace mapping1 'c 6) 'c))

              (test-equal "mapping-delete"
                          42
                          (mapping-ref/default (mapping-delete mapping1 'b) 'b 42))

              (test-equal "mapping-delete-all"
                          42
                          (mapping-ref/default (mapping-delete-all mapping1 '(a b)) 'b 42))

              (test-equal "mapping-intern: key in mapping"
                          (list mapping1 2)
                          (receive result
                              (mapping-intern mapping1 'b (lambda () (error "should not have been invoked")))
                            result))

              (test-equal "mapping-intern: key not in mapping"
                          (list 42 42)
                          (receive (mapping value)
                              (mapping-intern mapping1 'd (lambda () 42))
                            (list value (mapping-ref mapping 'd))))

              (test-equal "mapping-update"
                          4
                          (mapping-ref mapping3 'b))

              (test-equal "mapping-update/default"
                          16
                          (mapping-ref mapping4 'd))

              (test-equal "mapping-pop: empty mapping"
                          'empty
                          (mapping-pop mapping0 (lambda () 'empty)))

              (test-equal "mapping-pop: non-empty mapping"
                          (list 2 'a 1)
                          (receive (mapping key value)
                              (mapping-pop mapping1)
                            (list (mapping-size mapping) key value)))

              (test-equal
               '("success updated"
                 "failure ignored"
                 ((0 . "zero") (1 . "one") (2 . "two [seen]") (3 . "three")
                               (4 . "four") (5 . "five")))
               (let ((m1 (mapping (make-default-comparator)
                                  1 "one"
                                  3 "three"
                                  0 "zero"
                                  4 "four"
                                  2 "two"
                                  5 "five")))
                 (define (f/ignore insert ignore)
                   (ignore "failure ignored"))
                 (define (s/update key val update remove)
                   (update key
                           (string-append val " [seen]")
                           "success updated"))
                 (let*-values (((m2 v2) (mapping-search m1 2 f/ignore s/update))
                               ((m3 v3) (mapping-search m2 42 f/ignore s/update)))
                   (list v2 v3 (mapping->alist m3))))))

  (test-group "The whole mapping"
              (define mapping0 (mapping comparator))
              (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))

              (test-equal "mapping-size: empty mapping"
                          0
                          (mapping-size mapping0))

              (test-equal "mapping-size: non-empty mapping"
                          3
                          (mapping-size mapping1))

              (test-equal "mapping-find: found in mapping"
                          (list 'b 2)
                          (receive result
                              (mapping-find (lambda (key value)
                                              (and (eq? key 'b)
                                                   (= value 2)))
                                            mapping1
                                            (lambda () (error "should not have been called")))
                            result))

              (test-equal "mapping-find: not found in mapping"
                          (list 42)
                          (receive result
                              (mapping-find (lambda (key value)
                                              (eq? key 'd))
                                            mapping1
                                            (lambda ()
                                              42))
                            result))

              (test-equal "mapping-count"
                          2
                          (mapping-count (lambda (key value)
                                           (>= value 2))
                                         mapping1))

              (test-assert "mapping-any?: found"
                           (mapping-any? (lambda (key value)
                                           (= value 3))
                                         mapping1))

              (test-assert "mapping-any?: not found"
                           (not (mapping-any? (lambda (key value)
                                                (= value 4))
                                              mapping1)))

              (test-assert "mapping-every?: true"
                           (mapping-every? (lambda (key value)
                                             (<= value 3))
                                           mapping1))

              (test-assert "mapping-every?: false"
                           (not (mapping-every? (lambda (key value)
                                                  (<= value 2))
                                                mapping1)))

              (test-equal "mapping-keys"
                          3
                          (length (mapping-keys mapping1)))

              (test-equal "mapping-values"
                          6
                          (fold + 0 (mapping-values mapping1)))

              (test-equal "mapping-entries"
                          (list 3 6)
                          (receive (keys values)
                              (mapping-entries mapping1)
                            (list (length keys) (fold + 0 values)))))

  (test-group "Mapping and folding"
              (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
              (define mapping2 (mapping-map (lambda (key value)
                                              (values (symbol->string key)
                                                      (* 10 value)))
                                            comparator
                                            mapping1))

              (test-equal "mapping-map"
                          20
                          (mapping-ref mapping2 "b"))

              (test-equal "mapping-for-each"
                          6
                          (let ((counter 0))
                            (mapping-for-each (lambda (key value)
                                                (set! counter (+ counter value)))
                                              mapping1)
                            counter))

              (test-equal "mapping-fold"
                          6
                          (mapping-fold (lambda (key value acc)
                                          (+ value acc))
                                        0
                                        mapping1))

              (test-equal "mapping-map->list"
                          (+ (* 1 1) (* 2 2) (* 3 3))
                          (fold + 0 (mapping-map->list (lambda (key value)
                                                         (* value value))
                                                       mapping1)))

              (test-equal "mapping-filter"
                          2
                          (mapping-size (mapping-filter (lambda (key value)
                                                          (<= value 2))
                                                        mapping1)))

              (test-equal "mapping-remove"
                          1
                          (mapping-size (mapping-remove (lambda (key value)
                                                          (<= value 2))
                                                        mapping1)))

              (test-equal "mapping-partition"
                          (list 1 2)
                          (receive result
                              (mapping-partition (lambda (key value)
                                                   (eq? 'b key))
                                                 mapping1)
                            (map mapping-size result)))

              (test-group "Copying and conversion"
                          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping2 (alist->mapping comparator '((a . 1) (b . 2) (c . 3))))
                          (define mapping3 (alist->mapping! (mapping-copy mapping1) '((d . 4) '(c . 5))))

                          (test-equal "mapping-copy: same size"
                                      3
                                      (mapping-size (mapping-copy mapping1)))

                          (test-equal "mapping-copy: same comparator"
                                      comparator
                                      (mapping-key-comparator (mapping-copy mapping1)))

                          (test-equal "mapping->alist"
                                      (cons 'b 2)
                                      (assq 'b (mapping->alist mapping1)))

                          (test-equal "alist->mapping"
                                      2
                                      (mapping-ref mapping2 'b)
                                      )

                          (test-equal "alist->mapping!: new key"
                                      4
                                      (mapping-ref mapping3 'd))

                          (test-equal "alist->mapping!: existing key"
                                      3
                                      (mapping-ref mapping3 'c)))

              (test-group "Submappings"
                          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping3 (mapping comparator 'a 1 'c 3))
                          (define mapping4 (mapping comparator 'a 1 'c 3 'd 4))
                          (define mapping5 (mapping comparator 'a 1 'b 2 'c 6))
                          (define mapping6 (mapping (make-comparator (comparator-type-test-predicate comparator)
                                                                     (comparator-equality-predicate comparator)
                                                                     (comparator-ordering-predicate comparator)
                                                                     (lambda (obj) 42))
                                                    'a 1 'b 2 'c 3))


                          (test-assert "mapping=?: equal mappings"
                                       (mapping=? comparator mapping1 mapping2))

                          (test-assert "mapping=?: unequal mappings"
                                       (not (mapping=? comparator mapping1 mapping4)))

                          (test-assert "mapping=?: different comparators"
                                       (not (mapping=? comparator mapping1 mapping6)))

                          (test-assert "mapping<?: proper subset"
                                       (mapping<? comparator mapping3 mapping1))

                          (test-assert "mapping<?: improper subset"
                                       (not (mapping<? comparator mapping3 mapping1 mapping2)))

                          (test-assert "mapping>?: proper superset"
                                       (mapping>? comparator mapping2 mapping3))

                          (test-assert "mapping>?: improper superset"
                                       (not (mapping>? comparator mapping1 mapping2 mapping3)))

                          (test-assert "mapping<=?: subset"
                                       (mapping<=? comparator mapping3 mapping2 mapping1))

                          (test-assert "mapping<=?: non-matching values"
                                       (not (mapping<=? comparator mapping3 mapping5)))

                          (test-assert "mapping<=?: not a subset"
                                       (not (mapping<=? comparator mapping2 mapping4)))

                          (test-assert "mapping>=?: superset"
                                       (mapping>=? comparator mapping4 mapping3))

                          (test-assert "mapping>=?: not a superset"
                                       (not (mapping>=? comparator mapping5 mapping3))))

              (test-group "Set theory operations"
                          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping2 (mapping comparator 'a 1 'b 2 'd 4))
                          (define mapping3 (mapping comparator 'a 1 'b 2))
                          (define mapping4 (mapping comparator 'a 1 'b 2 'c 4))
                          (define mapping5 (mapping comparator 'a 1 'c 3))
                          (define mapping6 (mapping comparator 'd 4 'e 5 'f 6))

                          (test-equal "mapping-union: new association"
                                      4
                                      (mapping-ref (mapping-union mapping1 mapping2) 'd))

                          (test-equal "mapping-union: existing association"
                                      3
                                      (mapping-ref (mapping-union mapping1 mapping4) 'c))

                          (test-equal "mapping-union: three mappings"
                                      6
                                      (mapping-size (mapping-union mapping1 mapping2 mapping6)))

                          (test-equal "mapping-intersection: existing association"
                                      3
                                      (mapping-ref (mapping-intersection mapping1 mapping4) 'c))

                          (test-equal "mapping-intersection: removed association"
                                      42
                                      (mapping-ref/default (mapping-intersection mapping1 mapping5) 'b 42))

                          (test-equal "mapping-difference"
                                      2
                                      (mapping-size (mapping-difference mapping2 mapping6)))

                          (test-equal "mapping-xor"
                                      4
                                      (mapping-size (mapping-xor mapping2 mapping6))))

              (test-group "Additional procedures for mappings with ordered keys"
                          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3 'd 4))
                          (define mapping3 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5))
                          (define mapping4 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6))
                          (define mapping5 (mapping comparator 'f 6 'g 7 'h 8))

                          (test-equal "mapping-min-key"
                                      '(a a a a)
                                      (map mapping-min-key (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-max-key"
                                      '(c d e f)
                                      (map mapping-max-key (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-min-value"
                                      '(1 1 1 1)
                                      (map mapping-min-value (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-max-value"
                                      '(3 4 5 6)
                                      (map mapping-max-value (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-key-predecessor"
                                      '(c d d d)
                                      (map (lambda (mapping)
                                             (mapping-key-predecessor mapping 'e (lambda () #f)))
                                           (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-key-successor"
                                      '(#f #f e e)
                                      (map (lambda (mapping)
                                             (mapping-key-successor mapping 'd (lambda () #f)))
                                           (list mapping1 mapping2 mapping3 mapping4)))

                          (test-equal "mapping-range=: contained"
                                      '(4)
                                      (mapping-values (mapping-range= mapping4 'd)))

                          (test-equal "mapping-range=: not contained"
                                      '()
                                      (mapping-values (mapping-range= mapping4 'z)))

                          (test-equal "mapping-range<"
                                      '(1 2 3)
                                      (mapping-values (mapping-range< mapping4 'd)))

                          (test-equal "mapping-range<="
                                      '(1 2 3 4)
                                      (mapping-values (mapping-range<= mapping4 'd)))

                          (test-equal "mapping-range>"
                                      '(5 6)
                                      (mapping-values (mapping-range> mapping4 'd)))

                          (test-equal "mapping-range>="
                                      '(4 5 6)
                                      (mapping-values (mapping-range>= mapping4 'd)))

                          (test-equal "mapping-split"
                                      '((1 2 3) (1 2 3 4) (4) (4 5 6) (5 6))
                                      (receive mappings
                                          (mapping-split mapping4 'd)
                                        (map mapping-values mappings)))

                          (test-equal "mapping-catenate"
                                      '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8))
                                      (mapping->alist (mapping-catenate comparator mapping2 'e 5 mapping5)))

                          (test-equal "mapping-map/monotone"
                                      '((1 . 1) (2 . 4) (3 . 9))
                                      (mapping->alist
                                       (mapping-map/monotone (lambda (key value)
                                                               (values value (* value value)))
                                                             comparator
                                                             mapping1)))

                          (test-equal "mapping-fold/reverse"
                                      '(1 2 3)
                                      (mapping-fold/reverse (lambda (key value acc)
                                                              (cons value acc))
                                                            '() mapping1)))

              #;(test-group "Comparators"
                          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3))
                          (define mapping3 (mapping comparator 'a 1 'b 2))
                          (define mapping4 (mapping comparator 'a 1 'b 2 'c 4))
                          (define mapping5 (mapping comparator 'a 1 'c 3))
                          (define mapping0 (mapping comparator mapping1 "a" mapping2 "b" mapping3 "c" mapping4 "d" mapping5 "e"))

                          (test-assert "mapping-comparator"
                                       (comparator? mapping-comparator))

                          (test-equal "mapping-keyed mapping"
                                      (list "a" "a" "c" "d" "e")
                                      (list (mapping-ref mapping0 mapping1)
                                            (mapping-ref mapping0 mapping2)
                                            (mapping-ref mapping0 mapping3)
                                            (mapping-ref mapping0 mapping4)
                                            (mapping-ref mapping0 mapping5)))

                          (test-group "Ordering comparators"
                                      (test-assert "=?: equal mappings"
                                                   (=? comparator mapping1 mapping2))

                                      (test-assert "=?: unequal mappings"
                                                   (not (=? comparator mapping1 mapping4)))

                                      (test-assert "<?: case 1"
                                                   (<? comparator mapping3 mapping4))

                                      (test-assert "<?: case 2"
                                                   (<? comparator mapping1 mapping4))

                                      (test-assert "<?: case 3"
                                                   (<? comparator mapping1 mapping5))))))
