#lang racket/base

;;; SRFI-146 hashmaps

(require racket/contract racket/dict racket/generic racket/hash-code racket/match racket/require
         "../128.rkt"
         (for-syntax racket/base)
         (filtered-in (lambda (name) (regexp-replace #rx"mapping" name "hashmap")) "private/common.rkt"))
(module+ test (require "../private/testwrappers.rkt"))

(provide
 comparator?
 (all-from-out "private/common.rkt")
 (contract-out
  [hashmap? predicate/c]
  [hashmap (-> comparator? any/c ... hashmap?)]
  [hashmap-unfold (-> (-> any/c any/c) (-> any/c (values any/c any/c)) (-> any/c any/c) any/c comparator? hashmap?)]
  [alist->hashmap (-> comparator? (listof (cons/c any/c any/c)) hashmap?)]
  [hashmap-map (-> (-> any/c any/c (values any/c any/c)) comparator? dict? hashmap?)]
  [hashmap=? (-> comparator? hashmap? hashmap? hashmap? ... boolean?)]
  [hashmap<? (-> comparator? hashmap? hashmap? hashmap? ... boolean?)]
  [hashmap<=? (-> comparator? hashmap? hashmap? hashmap? ... boolean?)]
  [hashmap>? (-> comparator? hashmap? hashmap? hashmap? ... boolean?)]
  [hashmap>=? (-> comparator? hashmap? hashmap? hashmap? ... boolean?)]
  [hashmap-key-comparator (-> hashmap? comparator?)]
  [make-hashmap-comparator (-> comparator? comparator?)]
  [hashmap-comparator comparator?]

  [hashmap-adjoin (-> dict? any/c ... dict?)]
  [hashmap-adjoin! (-> dict? any/c ... dict?)]
  [hashmap-pop (->* (dict?) ((-> any)) any)]
  [hashmap-pop! (->* (dict?) ((-> any)) any)]
  [hashmap-find (-> (-> any/c any/c any/c) dict? (-> any) any)]
  [hashmap-entries (-> dict? (values list? list?))]
  [hashmap-fold (-> (-> any/c any/c any/c any/c) any/c dict? any/c)]
  [hashmap-map->list (-> (-> any/c any/c any/c) dict? list?)]
  [hashmap-partition (-> (-> any/c any/c any/c) dict? (values dict? dict?))]
  [hashmap-partition! (-> (-> any/c any/c any/c) dict? (values dict? dict?))]

  ))

;;; Internal routines and struct definitions

(define (%apply-hash hm hash-func)
   (for/fold ([hash (hash-func (hashmap-key-comparator hm))])
             ([(k v) (in-dict (hashmap-table hm))])
     (hash-code-combine-unordered hash (hash-func k) (hash-func v))))

(struct hashmap (key-comparator [table #:mutable])
  #:name %hashmap
  #:constructor-name %make-hashmap
  #:methods gen:dict
  [(define/generic super-dict-ref dict-ref)
   (define (dict-ref hm key [failure (lambda () (raise-arguments-error 'dict-ref "key not present" "key" key))])
     (super-dict-ref (hashmap-table hm) key failure))
   (define/generic super-dict-set dict-set)
   (define (dict-set hm key val)
     (struct-copy %hashmap hm [table (super-dict-set (hashmap-table hm) key val)]))
   (define (dict-set! hm key val)
     (set-hashmap-table! hm (super-dict-set (hashmap-table hm) key val)))
   (define/generic super-dict-remove dict-remove)
   (define (dict-remove hm key)
     (struct-copy %hashmap hm [table (super-dict-remove (hashmap-table hm) key)]))
   (define (dict-remove! hm key)
     (set-hashmap-table! hm (super-dict-remove (hashmap-table hm) key)))
   (define/generic super-dict-iterate-first dict-iterate-first)
   (define (dict-iterate-first hm)
     (super-dict-iterate-first (hashmap-table hm)))
   (define/generic super-dict-iterate-next dict-iterate-next)
   (define (dict-iterate-next hm pos)
     (super-dict-iterate-next (hashmap-table hm) pos))
   (define/generic super-dict-iterate-key dict-iterate-key)
   (define (dict-iterate-key hm pos)
     (super-dict-iterate-key (hashmap-table hm) pos))
   (define/generic super-dict-iterate-value dict-iterate-value)
   (define (dict-iterate-value hm pos)
     (super-dict-iterate-value (hashmap-table hm) pos))
   (define/generic super-dict-has-key? dict-has-key?)
   (define (dict-has-key? hm key)
     (super-dict-has-key? (hashmap-table hm) key))
   (define/generic super-dict-empty? dict-empty?)
   (define (dict-empty? hm)
     (super-dict-empty? (hashmap-table hm)))
   (define/generic super-dict-count dict-count)
   (define (dict-count hm)
     (super-dict-count (hashmap-table hm)))
   (define (dict-clear hm)
     (make-hashmap (hashmap-key-comparator hm)))
   (define (dict-clear! hm)
     (set-hashmap-table! hm (make-new-table (hashmap-key-comparator hm))))
   (define/generic super-dict-copy dict-copy)
   (define (dict-copy hm)
     (let ([orig-table (hashmap-table hm)])
       (if (dict-implements? orig-table 'dict-copy)
           (struct-copy %hashmap hm (table (super-dict-copy orig-table)))
           (struct-copy %hashmap hm
                        (table (for/fold ([new-table (make-new-table (hashmap-key-comparator hm))])
                                         ([(k v) (in-dict orig-table)])
                                 (super-dict-set new-table k v)))))))
   ]

  #:methods gen:equal+hash
  [(define (equal-proc a b real-equal?)
     (let ([t1 (hashmap-table a)]
           [t2 (hashmap-table b)])
       (and
        (= (dict-count t1) (dict-count t2))
        (real-equal? (hashmap-key-comparator a) (hashmap-key-comparator a))
        (for/and ([(k1 v1) (in-dict t1)])
          (and (dict-has-key? t2 k1)
               (real-equal? v1 (dict-ref t2 k1)))))))
   (define hash-proc %apply-hash)
   (define hash2-proc %apply-hash)])

(define (make-new-table cmp)
  ;;; Special case comparators that are compatible with built-in hash table types
  (cond
    ((equal? cmp eq-comparator) (hasheq))
    ((equal? cmp eqv-comparator) (hasheqv))
    ((equal? cmp equal-comparator) (hash))
    ((equal? cmp equal-always-comparator) (hashalw))
    (else
     (make-immutable-custom-hash (comparator-equality-predicate cmp)
                                 (comparator-hash-function cmp)
                                 (comparator-secondary-hash-function cmp)
                                 #:key? (comparator-type-test-predicate cmp)))))

(define (make-hashmap cmp) ;(make-new-table cmp))
    (%make-hashmap cmp (make-new-table cmp)))

;;; If a key already exists, skip it
(define (%hashmap-add-list name table keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error name "Needs 0 or more sets of key and value arguments"))
  (let loop ([table table]
             [keyvals keyvals])
    (match keyvals
      ((list) table)
      ((list* key val rest-keyvals)
       (if (dict-has-key? table key)
           (loop table rest-keyvals)
           (loop (dict-set table key val) rest-keyvals))))))

(define (%hashmap-add-list! name table keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error name "Needs 0 or more sets of key and value arguments"))
  (let loop ([keyvals keyvals])
    (match keyvals
      ((list) table)
      ((list* key val rest-keyvals)
       (unless (dict-has-key? table key)
         (dict-set! table key val))
       (loop rest-keyvals)))))

;;; hashmap specific routines

(define (hashmap cmp . keyvals)
  (unless (comparator-hashable? cmp)
    (raise-argument-error 'hashmap "comparator-hashable?" cmp))
  (%hashmap-add-list 'hashmap (make-hashmap cmp) keyvals))

(define (hashmap-unfold stop? mapper successor seed cmp)
  (unless (comparator-hashable? cmp)
    (raise-argument-error 'hashmap-unfold "comparator-hashable?" cmp))
  (let ([table (make-hashmap cmp)])
    (let loop ([seed seed])
      (if (stop? seed)
          table
          (let-values ([(k v) (mapper seed)]
                       [(next-seed) (successor seed)])
            (if (dict-has-key? (hashmap-table table) k)
                (loop next-seed)
                (begin
                  (dict-set! table k v)
                  (loop next-seed))))))))

(define (hashmap-map proc cmp hm)
  (unless (comparator-hashable? cmp)
    (raise-argument-error 'hashmap-map "comparator-hashable?" cmp))
  (let ([new-hm (make-hashmap cmp)])
    (for ([(k v) (in-dict hm)])
      (let-values ([(new-k new-v) (proc k v)])
        (dict-set! new-hm new-k new-v)))
    new-hm))

(define (alist->hashmap cmp alist)
  (unless (comparator-hashable? cmp)
    (raise-argument-error 'alist->hashmap "comparator-hashable?" cmp))
  (let ([hm (make-hashmap cmp)])
    (for ([pair (in-list alist)]
          #:when (not (dict-has-key? (hashmap-table hm) (car pair))))
      (dict-set! hm (car pair) (cdr pair)))
    hm))

(define (%binary-hashmap=? cmp hm1 hm2)
  (let ([t1 (hashmap-table hm1)]
        [t2 (hashmap-table hm2)])
    (and
     (= (dict-count t1) (dict-count t2))
     (eq? (hashmap-key-comparator hm1) (hashmap-key-comparator hm2))
     (for/and ([(k1 v1) (in-dict t1)])
       (and (dict-has-key? t2 k1)
            (=? cmp v1 (dict-ref t2 k1)))))))

(define (hashmap=? cmp hm1 hm2 . rest)
  (and (%binary-hashmap=? cmp hm1 hm2)
       (match rest
         ((list) #t)
         ((list hm3) (%binary-hashmap=? cmp hm1 hm3))
         ((list* hm3 rest) (apply hashmap=? cmp hm1 hm3 rest)))))

(define (hashmap<? cmp . hashmaps)
  (%hashmap<? cmp (reverse hashmaps)))

(define (%binary-hashmap<? cmp hm1 hm2)
  (let ([t1 (hashmap-table hm1)]
        [t2 (hashmap-table hm2)])
    (and
     (< (dict-count t2) (dict-count t1))
     (eq? (hashmap-key-comparator hm1) (hashmap-key-comparator hm2))
     (for/and ([(k2 v2) (in-dict t2)])
       (and (dict-has-key? t1 k2)
            (=? cmp v2 (dict-ref t1 k2)))))))

(define (%hashmap<? cmp hashmaps)
  (match hashmaps
    ((list hm1 hm2) (%binary-hashmap<? cmp hm1 hm2))
    ((list* hm1 hm2 rest)
     (and (%binary-hashmap<? cmp hm1 hm2)
          (%hashmap<? cmp (cons hm2 rest))))))


(define (hashmap<=? cmp . hashmaps)
  (%hashmap<=? cmp (reverse hashmaps)))

(define (%binary-hashmap<=? cmp hm1 hm2)
  (let ([t1 (hashmap-table hm1)]
        [t2 (hashmap-table hm2)])
    (and
     (<= (dict-count t2) (dict-count t1))
     (eq? (hashmap-key-comparator hm1) (hashmap-key-comparator hm2))
     (for/and ([(k2 v2) (in-dict t2)])
       (and (dict-has-key? t1 k2)
            (=? cmp v2 (dict-ref t1 k2)))))))

(define (%hashmap<=? cmp hashmaps)
  (match hashmaps
    ((list hm1 hm2) (%binary-hashmap<=? cmp hm1 hm2))
    ((list* hm1 hm2 rest)
     (and (%binary-hashmap<=? cmp hm1 hm2)
          (%hashmap<=? cmp (cons hm2 rest))))))

(define (hashmap>? cmp . hashmaps)
  (%hashmap>? cmp (reverse hashmaps)))

(define (%binary-hashmap>? cmp hm1 hm2)
  (let ([t1 (hashmap-table hm1)]
        [t2 (hashmap-table hm2)])
    (and
     (> (dict-count t2) (dict-count t1))
     (eq? (hashmap-key-comparator hm1) (hashmap-key-comparator hm2))
     (for/and ([(k1 v1) (in-dict t1)])
       (and (dict-has-key? t2 k1)
            (=? cmp v1 (dict-ref t2 k1)))))))

(define (%hashmap>? cmp hashmaps)
  (match hashmaps
    ((list hm1 hm2) (%binary-hashmap>? cmp hm1 hm2))
    ((list* hm1 hm2 rest)
     (and (%binary-hashmap>? cmp hm1 hm2)
          (%hashmap>? cmp (cons hm2 rest))))))

(define (hashmap>=? cmp . hashmaps)
  (%hashmap>=? cmp (reverse hashmaps)))

(define (%binary-hashmap>=? cmp hm1 hm2)
  (let ([t1 (hashmap-table hm1)]
        [t2 (hashmap-table hm2)])
    (and
     (>= (dict-count t2) (dict-count t1))
     (eq? (hashmap-key-comparator hm1) (hashmap-key-comparator hm2))
     (for/and ([(k1 v1) (in-dict t1)])
       (and (dict-has-key? t2 k1)
            (=? cmp v1 (dict-ref t2 k1)))))))

(define (%hashmap>=? cmp hashmaps)
  (match hashmaps
    ((list hm1 hm2) (%binary-hashmap>=? cmp hm1 hm2))
    ((list* hm1 hm2 rest)
     (and (%binary-hashmap>=? cmp hm1 hm2)
          (%hashmap>=? cmp (cons hm2 rest))))))

;;; generic dict routines

(define (hashmap-adjoin hm . keyvals)
  (%hashmap-add-list 'hashmap-adjoin hm keyvals))

(define (hashmap-adjoin! hm . keyvals)
  (if (dict-implements? hm 'dict-set!)
      (%hashmap-add-list! 'hashmap-adjoin! hm keyvals)
      (%hashmap-add-list 'hashmap-adjoin! hm keyvals)))

(define (hashmap-pop hm [failure (lambda () (raise-argument-error 'hashmap-pop "(not/c hashmap-empty?)" hm))])
  (if (dict-empty? hm)
      (failure)
      (let* ([pos (dict-iterate-first hm)]
             [key (dict-iterate-key hm pos)]
             [val (dict-iterate-value hm pos)])
        (values (dict-remove hm key) key val))))

(define (hashmap-pop! hm [failure (lambda () (raise-argument-error 'hashmap-pop! "(not/c hashmap-empty?)" hm))])
  (if (dict-empty? hm)
      (failure)
      (let* ([pos (dict-iterate-first hm)]
             [key (dict-iterate-key hm pos)]
             [val (dict-iterate-value hm pos)])
        (if (dict-implements? hm 'dict-remove!)
            (begin
              (dict-remove! hm key)
              (values hm key val))
            (values (dict-remove hm key) key val)))))

(define (hashmap-find pred? hm failure)
  (let ([found (for/first ([(k v) (in-dict hm)]
                           #:when (pred? k v))
                 (cons k v))])
    (if found
        (values (car found) (cdr found))
        (failure))))


(define (hashmap-entries hm)
  (for/lists (keys vals)
             ([(k v) (in-dict hm)])
    (values k v)))

(define (hashmap-fold kons knil hm)
  (for/fold ([seed knil])
            ([(k v) (in-dict hm)])
    (kons k v seed)))

(define (hashmap-map->list proc hm)
  (for/list ([(k v) (in-dict hm)])
    (proc k v)))

(define (hashmap-partition pred? hm)
  (for/fold ([yes-hm (dict-clear hm)]
             [no-hm (dict-clear hm)])
            ([(k v) (in-dict hm)])
    (if (pred? k v)
        (values (dict-set yes-hm k v) no-hm)
        (values yes-hm (dict-set no-hm k v)))))

(define (hashmap-partition! pred? hm)
  (if (dict-implements? hm 'dict-set! 'dict-clear)
      (let ([yes-hm (dict-clear hm)]
            [no-hm (dict-clear hm)])
        (for ([(k v) (in-dict hm)])
          (if (pred? k v)
              (dict-set! yes-hm k v)
              (dict-set! no-hm k v)))
        (values yes-hm no-hm))
      (hashmap-partition pred? hm)))

(define (make-hashmap-comparator cmp)
  (make-comparator hashmap? (lambda (a b) (%binary-hashmap=? cmp a b)) #f equal-hash-code #:secondary-hash equal-secondary-hash-code))

(define hashmap-comparator (make-hashmap-comparator default-comparator))
(comparator-register-default! hashmap-comparator)

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
              (define hashmap0 (hashmap comparator))
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap comparator 'c 1 'd 2 'e 3))
              (define hashmap3 (hashmap comparator 'd 1 'e 2 'f 3))

              (test-assert "hashmap?: a hashmap"
                           (hashmap? (hashmap comparator)))

              (test-assert "hashmap?: not a hashmap"
                           (not (hashmap? (list 1 2 3))))

              (test-assert "hashmap-empty?: empty hashmap"
                           (hashmap-empty? hashmap0))

              (test-assert "hashmap-empty?: non-empty hashmap"
                           (not (hashmap-empty? hashmap1)))

              (test-assert "hashmap-contains?: containing"
                           (hashmap-contains? hashmap1 'b))

              (test-assert "hashmap-contains?: not containing"
                           (not (hashmap-contains? hashmap1 '2)))

              (test-assert "hashmap-disjoint?: disjoint"
                           (hashmap-disjoint? hashmap1 hashmap3))

              (test-assert "hashmap-disjoint?: not disjoint"
                           (not (hashmap-disjoint? hashmap1 hashmap2))))

  (test-group "Accessors"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))

              (test-equal "hashmap-ref: key found"
                          2
                          (hashmap-ref hashmap1 'b))

              (test-equal "hashmap-ref: key not found/with failure"
                          42
                          (hashmap-ref hashmap1 'd (lambda () 42)))

              (test-error "hashmap-ref: key not found/without failure"
                          (hashmap-ref hashmap1 'd))

              (test-equal "hashmap-ref: with success procedure"
                          (* 2 2)
                          (hashmap-ref hashmap1 'b (lambda () #f) (lambda (x) (* x x))))

              (test-equal "hashmap-ref/default: key found"
                          3
                          (hashmap-ref/default hashmap1 'c 42))

              (test-equal "hashmap-ref/default: key not found"
                          42
                          (hashmap-ref/default hashmap1 'd 42))

              (test-equal "hashmap-key-comparator"
                          comparator
                          (hashmap-key-comparator hashmap1)))

  (test-group "Updaters"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap-set hashmap1 'c 4 'd 4 'd 5))
              (define hashmap3 (hashmap-update hashmap1 'b (lambda (x) (* x x))))
              (define hashmap4 (hashmap-update/default hashmap1 'd (lambda (x) (* x x)) 4))
              (define hashmap5 (hashmap-adjoin hashmap1 'c 4 'd 4 'd 5))
              (define hashmap0 (hashmap comparator))

              (test-equal "hashmap-adjoin: key already in hashmap"
                          3
                          (hashmap-ref hashmap5 'c))

              (test-equal "hashmap-adjoin: key set earlier"
                          4
                          (hashmap-ref hashmap5 'd))

              (test-equal "hashmap-set: key already in hashmap"
                          4
                          (hashmap-ref hashmap2 'c))

              (test-equal "hashmap-set: key set earlier"
                          5
                          (hashmap-ref hashmap2 'd))

              (test-equal "hashmap-replace: key not in hashmap"
                          #f
                          (hashmap-ref/default (hashmap-replace hashmap1 'd 4) 'd #f))

              (test-equal "hashmap-replace: key in hashmap"
                          6
                          (hashmap-ref (hashmap-replace hashmap1 'c 6) 'c))

              (test-equal "hashmap-delete"
                          42
                          (hashmap-ref/default (hashmap-delete hashmap1 'b) 'b 42))

              (test-equal "hashmap-delete-all"
                          42
                          (hashmap-ref/default (hashmap-delete-all hashmap1 '(a b)) 'b 42))

              (test-equal "hashmap-intern: key in hashmap"
                          (list hashmap1 2)
                          (receive result
                              (hashmap-intern hashmap1 'b (lambda () (error "should not have been invoked")))
                            result))

              (test-equal "hashmap-intern: key not in hashmap"
                          (list 42 42)
                          (receive (hashmap value)
                              (hashmap-intern hashmap1 'd (lambda () 42))
                            (list value (hashmap-ref hashmap 'd))))

              (test-equal "hashmap-update"
                          4
                          (hashmap-ref hashmap3 'b))

              (test-equal "hashmap-update/default"
                          16
                          (hashmap-ref hashmap4 'd))

              (test-equal "hashmap-pop: empty hashmap"
                          'empty
                          (hashmap-pop hashmap0 (lambda () 'empty)))

              (test-assert "hashmap-pop: non-empty hashmap"
                           (member
                            (receive (hashmap key value)
                                (hashmap-pop hashmap1)
                              (list (hashmap-size hashmap) key value))
                            '((2 a 1) (2 b 2) (2 c 3)))))

  (test-group "The whole hashmap"
              (define hashmap0 (hashmap comparator))
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))

              (test-equal "hashmap-size: empty hashmap"
                          0
                          (hashmap-size hashmap0))

              (test-equal "hashmap-size: non-empty hashmap"
                          3
                          (hashmap-size hashmap1))

              (test-equal "hashmap-find: found in hashmap"
                          (list 'b 2)
                          (receive result
                              (hashmap-find (lambda (key value)
                                              (and (eq? key 'b)
                                                   (= value 2)))
                                            hashmap1
                                            (lambda () (error "should not have been called")))
                            result))

              (test-equal "hashmap-find: not found in hashmap"
                          (list 42)
                          (receive result
                              (hashmap-find (lambda (key value)
                                              (eq? key 'd))
                                            hashmap1
                                            (lambda ()
                                              42))
                            result))

              (test-equal "hashmap-count"
                          2
                          (hashmap-count (lambda (key value)
                                           (>= value 2))
                                         hashmap1))

              (test-assert "hashmap-any?: found"
                           (hashmap-any? (lambda (key value)
                                           (= value 3))
                                         hashmap1))

              (test-assert "hashmap-any?: not found"
                           (not (hashmap-any? (lambda (key value)
                                                (= value 4))
                                              hashmap1)))

              (test-assert "hashmap-every?: true"
                           (hashmap-every? (lambda (key value)
                                             (<= value 3))
                                           hashmap1))

              (test-assert "hashmap-every?: false"
                           (not (hashmap-every? (lambda (key value)
                                                  (<= value 2))
                                                hashmap1)))

              (test-equal "hashmap-keys"
                          3
                          (length (hashmap-keys hashmap1)))

              (test-equal "hashmap-values"
                          6
                          (fold + 0 (hashmap-values hashmap1)))

              (test-equal "hashmap-entries"
                          (list 3 6)
                          (receive (keys values)
                              (hashmap-entries hashmap1)
                            (list (length keys) (fold + 0 values)))))

  (test-group "Hashmap and folding"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap-map (lambda (key value)
                                              (values (symbol->string key)
                                                      (* 10 value)))
                                            comparator
                                            hashmap1))

              (test-equal "hashmap-map"
                          20
                          (hashmap-ref hashmap2 "b"))

              (test-equal "hashmap-for-each"
                          6
                          (let ((counter 0))
                            (hashmap-for-each (lambda (key value)
                                                (set! counter (+ counter value)))
                                              hashmap1)
                            counter))

              (test-equal "hashmap-fold"
                          6
                          (hashmap-fold (lambda (key value acc)
                                          (+ value acc))
                                        0
                                        hashmap1))

              (test-equal "hashmap-map->list"
                          (+ (* 1 1) (* 2 2) (* 3 3))
                          (fold + 0 (hashmap-map->list (lambda (key value)
                                                         (* value value))
                                                       hashmap1)))

              (test-equal "hashmap-filter"
                          2
                          (hashmap-size (hashmap-filter (lambda (key value)
                                                          (<= value 2))
                                                        hashmap1)))

              (test-equal "hashmap-remove"
                          1
                          (hashmap-size (hashmap-remove (lambda (key value)
                                                          (<= value 2))
                                                        hashmap1)))

              (test-equal "hashmap-partition"
                          (list 1 2)
                          (receive result
                              (hashmap-partition (lambda (key value)
                                                   (eq? 'b key))
                                                 hashmap1)
                            (map hashmap-size result))))

  (test-group "Copying and conversion"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (alist->hashmap comparator '((a . 1) (b . 2) (c . 3))))
              (define hashmap3 (alist->hashmap! (hashmap-copy hashmap1) '((d . 4) '(c . 5))))

              (test-equal "hashmap-copy: same size"
                          3
                          (hashmap-size (hashmap-copy hashmap1)))

              (test-equal "hashmap-copy: same comparator"
                          comparator
                          (hashmap-key-comparator (hashmap-copy hashmap1)))

              (test-equal "hashmap->alist"
                          (cons 'b 2)
                          (assq 'b (hashmap->alist hashmap1)))

              (test-equal "alist->hashmap"
                          2
                          (hashmap-ref hashmap2 'b)
                          )

              (test-equal "alist->hashmap!: new key"
                          4
                          (hashmap-ref hashmap3 'd))

              (test-equal "alist->hashmap!: existing key"
                          3
                          (hashmap-ref hashmap3 'c)))

  (test-group "Subhashmaps"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap3 (hashmap comparator 'a 1 'c 3))
              (define hashmap4 (hashmap comparator 'a 1 'c 3 'd 4))
              (define hashmap5 (hashmap comparator 'a 1 'b 2 'c 6))
              (define hashmap6 (hashmap (make-comparator (comparator-type-test-predicate comparator)
                                                         (comparator-equality-predicate comparator)
                                                         (comparator-ordering-predicate comparator)
                                                         (comparator-hash-function comparator))
                                        'a 1 'b 2 'c 3))


              (test-assert "hashmap=?: equal hashmaps"
                           (hashmap=? comparator hashmap1 hashmap2))

              (test-assert "hashmap=?: unequal hashmaps"
                           (not (hashmap=? comparator hashmap1 hashmap4)))

              (test-assert "hashmap=?: different comparators"
                           (not (hashmap=? comparator hashmap1 hashmap6)))

              (test-assert "hashmap<?: proper subset"
                           (hashmap<? comparator hashmap3 hashmap1))

              (test-assert "hashmap<?: improper subset"
                           (not (hashmap<? comparator hashmap3 hashmap1 hashmap2)))

              (test-assert "hashmap>?: proper superset"
                           (hashmap>? comparator hashmap2 hashmap3))

              (test-assert "hashmap>?: improper superset"
                           (not (hashmap>? comparator hashmap1 hashmap2 hashmap3)))

              (test-assert "hashmap<=?: subset"
                           (hashmap<=? comparator hashmap3 hashmap2 hashmap1))

              (test-assert "hashmap<=?: non-matching values"
                           (not (hashmap<=? comparator hashmap3 hashmap5)))

              (test-assert "hashmap<=?: not a subset"
                           (not (hashmap<=? comparator hashmap2 hashmap4)))

              (test-assert "hashmap>=?: superset"
                           (hashmap>=? comparator hashmap4 hashmap3))

              (test-assert "hashmap>=?: not a superset"
                           (not (hashmap>=? comparator hashmap5 hashmap3))))

  (test-group "Set theory operations"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap comparator 'a 1 'b 2 'd 4))
              (define hashmap3 (hashmap comparator 'a 1 'b 2))
              (define hashmap4 (hashmap comparator 'a 1 'b 2 'c 4))
              (define hashmap5 (hashmap comparator 'a 1 'c 3))
              (define hashmap6 (hashmap comparator 'd 4 'e 5 'f 6))

              (test-equal "hashmap-union: new association"
                          4
                          (hashmap-ref (hashmap-union hashmap1 hashmap2) 'd))

              (test-equal "hashmap-union: existing association"
                          3
                          (hashmap-ref (hashmap-union hashmap1 hashmap4) 'c))

              (test-equal "hashmap-union: three hashmaps"
                          6
                          (hashmap-size (hashmap-union hashmap1 hashmap2 hashmap6)))

              (test-equal "hashmap-intersection: existing association"
                          3
                          (hashmap-ref (hashmap-intersection hashmap1 hashmap4) 'c))

              (test-equal "hashmap-intersection: removed association"
                          42
                          (hashmap-ref/default (hashmap-intersection hashmap1 hashmap5) 'b 42))

              (test-equal "hashmap-difference"
                          2
                          (hashmap-size (hashmap-difference hashmap2 hashmap6)))

              (test-equal "hashmap-xor"
                          4
                          (hashmap-size (hashmap-xor hashmap2 hashmap6))))

  (test-group "Comparators"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap3 (hashmap comparator 'a 1 'b 2))
              (define hashmap4 (hashmap comparator 'a 1 'b 2 'c 4))
              (define hashmap5 (hashmap comparator 'a 1 'c 3))
              (define hashmap0 (hashmap (make-equal-comparator)
                                        hashmap1 "a"
                                        hashmap2 "b"
                                        hashmap3 "c"
                                        hashmap4 "d"
                                        hashmap5 "e"))

              (test-assert "hashmap-comparator"
                           (comparator? hashmap-comparator))

              (test-equal "hashmap-keyed hashmap"
                          (list "a" "a" "c" "d" "e")
                          (list (hashmap-ref hashmap0 hashmap1)
                                (hashmap-ref hashmap0 hashmap2)
                                (hashmap-ref hashmap0 hashmap3)
                                (hashmap-ref hashmap0 hashmap4)
                                (hashmap-ref hashmap0 hashmap5)
                                )))

  (test-group "Ordering comparators"
              (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap2 (hashmap comparator 'a 1 'b 2 'c 3))
              (define hashmap4 (hashmap comparator 'a 1 'b 2 'c 4))

              (test-assert "=?: equal hashmaps"
                           (=? comparator hashmap1 hashmap2))

              (test-assert "=?: unequal hashmaps"
                           (not (=? comparator hashmap1 hashmap4)))))
