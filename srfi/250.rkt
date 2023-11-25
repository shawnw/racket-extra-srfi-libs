#lang racket/base

(require racket/contract racket/dict racket/function racket/list racket/match racket/sequence (only-in racket/stream in-stream)
         srfi/41 "128.rkt" "146/hash.rkt")


(provide
 (contract-out
  [hash-table? predicate/c]
  [hash-table (->i ([c comparator?]) #:rest [key+values list?] #:pre (key+values) (even? (length key+values)) [result hash-table?])]
  [make-hash-table (->* (comparator?) (exact-nonnegative-integer?) hash-table?)]
  [hash-table-unfold (->* ((-> any/c any/c) (-> any/c (values any/c any/c)) (-> any/c any/c) any/c comparator?) (exact-nonnegative-integer?) hash-table?)]
  [alist->hash-table (->* ((listof (cons/c any/c any/c)) comparator?) (exact-nonnegative-integer?) hash-table?)]
  [hash-table-empty? (-> hash-table? boolean?)]
  [hash-table-contains? (-> hash-table? any/c boolean?)]
  [hash-table=? (-> comparator? hash-table? hash-table? boolean?)]
  [hash-table-mutable? (-> hash-table? boolean?)]
  [hash-table-ref (->* (hash-table? any/c) ((-> any/c) (-> any/c any/c)) any/c)]
  [hash-table-ref/default (-> hash-table? any/c any/c any/c)]
  [hash-table-comparator (-> hash-table? comparator?)]
  [hash-table-set! (->i ([ht hash-table?]) #:rest [key+values list?] #:pre (key+values) (even? (length key+values)) [result void?])]
  [hash-table-intern! (-> hash-table? any/c (-> any/c) any/c)]
  [hash-table-delete! (-> hash-table? any/c ... exact-nonnegative-integer?)]
  [hash-table-update! (->* (hash-table? any/c (-> any/c any/c)) ((-> any/c) (-> any/c any/c)) void?)]
  [hash-table-update/default! (-> hash-table? any/c (-> any/c any/c) any/c void?)]
  [hash-table-pop! (-> hash-table? (values any/c any/c))]
  [hash-table-clear! (-> hash-table? void?)]
  [hash-table-size (-> hash-table? exact-nonnegative-integer?)]
  [hash-table-keys (-> hash-table? list?)]
  [hash-table-key-vector (-> hash-table? vector?)]
  [hash-table-values (-> hash-table? list?)]
  [hash-table-value-vector (-> hash-table? vector?)]
  [hash-table-entries (-> hash-table? (values list? list?))]
  [hash-table-entry-vectors (-> hash-table? (values vector? vector?))]
  [hash-table-find (-> (-> any/c any/c any/c) hash-table? (-> any/c) any/c)]
  [hash-table-count (-> (-> any/c any/c any/c) hash-table? exact-nonnegative-integer?)]
  [hash-table-map (-> (-> any/c any/c) comparator? hash-table? hash-table?)]
  [hash-table-for-each (-> (-> any/c any/c any/c) hash-table? void?)]
  [hash-table-map! (-> (-> any/c any/c any/c) hash-table? void?)]
  [hash-table-map->list (-> (-> any/c any/c any/c) hash-table? list?)]
  [hash-table-fold (-> (-> any/c any/c any/c any/c) any/c hash-table? any/c)]
  [hash-table-prune! (-> (-> any/c any/c any/c) hash-table? void?)]
  [hash-table-copy (-> hash-table? hash-table?)]
  [hash-table-empty-copy (-> hash-table? hash-table-empty?)]
  [hash-table->alist (-> hash-table? (listof (cons/c any/c any/c)))]
  [hash-table-union! (-> hash-table? hash-table? hash-table?)]
  [hash-table-intersection! (-> hash-table? hash-table? hash-table?)]
  [hash-table-difference! (-> hash-table? hash-table? hash-table?)]
  [hash-table-xor! (-> hash-table? hash-table? hash-table?)]
  ))

(struct element (key [value #:mutable] [deleted? #:mutable])
  #:transparent
  #:extra-constructor-name make-element)

(define (raise-no-such-key-error)
  (raise (make-exn:fail "no such key" (current-continuation-marks))))

(struct ordered-hash (table order)
  #:mutable

  #:methods gen:dict
  [(define (dict-ref ht key [failure raise-no-such-key-error])
     (hash-table-ref ht key failure))
   (define (dict-set! ht key v)
     (hash-table-set! ht key v))
   (define (dict-remove! ht key)
     (hash-table-delete! ht key))
   (define (dict-iterate-first ht)
     (if (hash-table-empty? ht)
         #f
         (stream-filter element-present? (ordered-hash-order ht))))
   (define (dict-iterate-next ht pos)
     (if (stream-null? pos)
         #f
         (let ([next-pos (stream-cdr pos)])
           (cond
             [(stream-null? next-pos) #f]
             [(eq? (stream-car next-pos) stream-null) #f]
             [else next-pos]))))
   (define (dict-iterate-key ht pos)
     (element-key (weak-box-value (stream-car pos))))
   (define (dict-iterate-value ht pos)
     (element-value (weak-box-value (stream-car pos))))
   (define (dict-hash-key? ht k)
     (hash-table-contains? ht k))
   (define (dict-set*! ht . key+values)
     (hash-table-set-from-list! ht key+values))
   (define (dict-ref! ht k to-set)
     (if (procedure? to-set)
         (hash-table-intern! ht k to-set)
         (hash-table-intern! ht k (thunk to-set))))
   (define (dict-empty? ht)
     (hash-table-empty? ht))
   (define (dict-count ht)
     (hash-table-count ht))
   (define (dict-copy ht)
     (hash-table-copy ht))
   (define (dict-clear! ht)
     (hash-table-clear! ht))
   (define (dict-keys ht)
     (hash-table-keys ht))
   (define (dict-values ht)
     (hash-table-values ht))
   (define (dict->list ht)
     (hash-table->alist ht))
   ])

(define hash-table? ordered-hash?)

(define (make-hash-table comparator [k 0])
  (ordered-hash (hashmap comparator) stream-null))

(define (elements->hash-table comparator elems)
  (ordered-hash
   (hashmap-unfold null? (lambda (elem-list) (let ([elem (car elem-list)]) (values (element-key elem) elem))) cdr elems comparator)
   (list->stream (map make-weak-box elems))))

(define (hash-table comparator . key+vals)
  (define elems (for/list ([k+v (in-slice 2 (in-list key+vals))])
                  (match-let ([(list k v) k+v])
                    (make-element k v #f))))
  (elements->hash-table comparator elems))

(define (hash-table-unfold stop? mapper successor seed comparator [k 0])
  (define elems
    (let loop ([seed seed]
               [elems '()])
      (if (stop? seed)
          (reverse elems)
          (let-values ([(k v) (mapper seed)])
            (loop (successor seed)
                  (cons (make-element k v #f) elems))))))
  (elements->hash-table comparator elems))

(define (alist->hash-table alist comparator [k 0])
  (elements->hash-table comparator
                        (map (lambda (elem) (make-element (car elem) (cdr elem) #f)) alist)))

(define (hash-table-contains? ht key)
  (hashmap-contains? (ordered-hash-table ht) key))

(define (hash-table-empty? ht)
  (hashmap-empty? (ordered-hash-table ht)))

(define (hash-table=? value-comparator ht1 ht2)
  (hashmap=? (make-wrapper-comparator element? element-value value-comparator) ht1 ht2))

(define (hash-table-mutable? ht) #t)

(define (try-hash-table-ref ht key)
  (hashmap-ref (ordered-hash-table ht) key (thunk (values #f #f)) (lambda (elem) (values elem #t))))

(define (hash-table-ref ht key [failure raise-no-such-key-error] [success identity])
  (hashmap-ref (ordered-hash-table ht)
               key
               failure
               (lambda (elem) (success (element-value elem)))))

(define (hash-table-ref/default ht key def)
  (hashmap-ref (ordered-hash-table ht) key (thunk def) element-value))

(define (hash-table-comparator ht)
  (hashmap-key-comparator (ordered-hash-table ht)))

(define (hash-table-set-from-list! ht key+values)
  (match key+values
    ['() (void)]
    [(list k v) ; Adding a single element
     (define elem (hashmap-ref/default (ordered-hash-table ht) k #f))
     (cond
       [elem
        (set-element-value! elem v)]
       [else
        (let ([elem (make-element k v #f)])
          (set-ordered-hash-table! ht (hashmap-set! (ordered-hash-table ht) k elem))
          (set-ordered-hash-order! ht (stream-append (ordered-hash-order ht) (stream-cons (make-weak-box elem) stream-null))))])]
    [_ ; adding multiple elements
     (let loop ([key+values key+values]
                [elements '()])
       (cond
         [(null? key+values)
          (unless (null? elements)
            (set-ordered-hash-order! ht (stream-append (ordered-hash-order ht) (list->stream (reverse elements)))))]
         [else
          (define k (first key+values))
          (define v (second key+values))
          (define elem (hashmap-ref/default (ordered-hash-table ht) k #f))
          (cond
            [elem
             (set-element-value! elem v)
             (loop (cddr key+values) elements)]
            [else
             (let ([elem (make-element k v #f)])
               (set-ordered-hash-table! ht (hashmap-set! (ordered-hash-table ht) k elem))
               (loop (cddr key+values) (cons (make-weak-box elem) elements)))])]))]))

(define (hash-table-set! ht . key+values)
  (hash-table-set-from-list! ht key+values))

(define (hash-table-intern! ht key failure)
  (define-values (elem found?) (try-hash-table-ref ht key))
  (cond
    [found? (element-value elem)]
    [else
     (define val (failure))
     (hash-table-set! ht key val)
     val]))

(define (hash-table-delete! ht . keys)
  (define deleted 0)
  (for ([key (in-list keys)]
        #:do [(define-values (elem found?) (try-hash-table-ref ht key))]
        #:when found?)
    (set-element-deleted?! elem #t)
    (set-ordered-hash-table! ht (hashmap-delete! (ordered-hash-table ht) key))
    (set! deleted (add1 deleted)))
  deleted)

(define (hash-table-update! ht key updater [failure raise-no-such-key-error] [success identity])
  (hash-table-set! ht key (updater (hash-table-ref ht key failure success))))

(define (hash-table-update/default! ht key updater default)
   (hash-table-set! ht key (updater (hash-table-ref/default ht key default))))

(define (element-present? maybe-elem)
  (define elem (weak-box-value maybe-elem))
  (and elem (not (element-deleted? elem))))

(define (drop-leading-deleted! ht)
  (set-ordered-hash-order! ht (stream-drop-while (negate element-present?) (ordered-hash-order ht))))

(define (present-elements ht)
  (stream-filter element-present? (ordered-hash-order ht)))

(define (prune-order-stream! ht)
  (set-ordered-hash-order! ht (stream-filter element-present? (ordered-hash-order ht))))

(define (hash-table-pop! ht)
  (when (hash-table-empty? ht)
    (raise-argument-error 'hash-table-pop! "(not/c hash-table-empty?)" ht))
  (drop-leading-deleted! ht)
  (let ([first-elem (weak-box-value (stream-car (ordered-hash-order ht)))])
    (set-ordered-hash-table! ht (hashmap-delete! (ordered-hash-table ht) (element-key first-elem)))
    (set-element-deleted?! first-elem #t)
    (values (element-key first-elem) (element-value first-elem))))

(define (hash-table-clear! ht)
  (set-ordered-hash-table! ht (hashmap (hash-table-comparator ht)))
  (set-ordered-hash-order! ht stream-null))

(define (hash-table-size ht)
  (hashmap-size (ordered-hash-table ht)))

(define (hash-table-keys ht)
  (for/list ([elem (in-stream (present-elements ht))])
    (element-key (weak-box-value elem))))

(define (hash-table-key-vector ht)
  (for/vector #:length (hash-table-size ht)
    ([elem (in-stream (present-elements ht))])
    (element-key (weak-box-value elem))))

(define (hash-table-values ht)
  (for/list ([elem (in-stream (present-elements ht))])
    (element-value (weak-box-value elem))))

(define (hash-table-value-vector ht)
  (for/vector #:length (hash-table-size ht)
    ([elem (in-stream (present-elements ht))])
    (element-value (weak-box-value elem))))

(define (hash-table-entries ht)
  (for/lists (keys entries)
             ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (values (element-key elem) (element-value elem))))

(define (hash-table-entry-vectors ht)
  (define-values (keys entries) (hash-table-entries ht))
  (values (list->vector keys) (list->vector entries)))

(define (hash-table-find proc ht failure)
  (prune-order-stream! ht)
  (let loop ([order (ordered-hash-order ht)])
    (if (stream-null? order)
        (failure)
        (let* ([elem (weak-box-value (stream-car order))]
               [result (proc (element-key elem) (element-value elem))])
          (if result
              result
              (loop (stream-cdr order)))))))

(define (hash-table-count pred? ht)
  (inexact->exact
   (for/sum ([e (in-stream (present-elements ht))])
     (define elem (weak-box-value e))
     (if (pred? (element-key elem) (element-value elem))
         1
         0))))

(define (hash-table-map proc comparator ht)
  (define new-ht (make-hash-table comparator))
  (for ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (hash-table-set! new-ht (element-key elem) (proc (element-value elem))))
  new-ht)

(define (hash-table-for-each proc ht)
  (for ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (proc (element-key elem) (element-value elem))))

(define (hash-table-map! proc ht)
  (for ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (set-element-value! elem (proc (element-key elem) (element-value elem)))))

(define (hash-table-map->list proc ht)
  (for/list ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (proc (element-key elem) (element-value elem))))

(define (hash-table-fold proc seed ht)
  (for/fold ([val seed])
            ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (proc (element-key elem) (element-value elem) val)))

(define (hash-table-prune! proc ht)
  (for ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (when (proc (element-key elem) (element-value elem))
      (set-ordered-hash-table! ht (hashmap-delete! (ordered-hash-table ht) (element-key elem)))
      (set-element-deleted?! elem #t)))
  (prune-order-stream! ht))

(define (hash-table-copy ht [mutable? #t])
  (define new-ht (hash-table-empty-copy ht))
  (for ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (hash-table-set! new-ht (element-key elem) (element-value elem)))
  new-ht)

(define (hash-table-empty-copy ht)
  (ordered-hash (hashmap (hashmap-comparator ht)) stream-null))

(define (hash-table->alist ht)
  (for/list ([e (in-stream (present-elements ht))])
    (define elem (weak-box-value e))
    (cons (element-key elem) (element-value elem))))

(define (hash-table-union! ht1 ht2)
  (for ([e (in-stream (present-elements ht2))])
    (define elem (weak-box-value e))
    (unless (hash-table-contains? ht1 (element-key elem))
      (hash-table-set! ht1 (element-key elem) (element-value elem))))
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (for ([e (in-stream (present-elements ht1))])
    (define elem (weak-box-value e))
    (unless (hash-table-contains? ht2 (element-key elem))
      (hash-table-delete! ht1 (element-key elem))))
  ht1)

(define (hash-table-difference! ht1 ht2)
  (for ([e (in-stream (present-elements ht1))])
    (define elem (weak-box-value e))
    (when (hash-table-contains? ht2 (element-key elem))
      (hash-table-delete! ht1 (element-key elem))))
  ht1)

(define (hash-table-xor! ht1 ht2)
  (hash-table-union! (hash-table-difference! ht1 ht2) ht2))

