#lang racket/base

(require racket/contract racket/dict racket/function racket/match racket/sequence "128.rkt" "146/hash.rkt")

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
  [hash-table-prune! (-> (-> any/c any/c any/c) hash-table? exact-nonnegative-integer?)]
  [hash-table-copy (-> hash-table? hash-table?)]
  [hash-table-empty-copy (-> hash-table? hash-table-empty?)]
  [hash-table->alist (-> hash-table? (listof (cons/c any/c any/c)))]
  [hash-table-union! (-> hash-table? hash-table? hash-table?)]
  [hash-table-intersection! (-> hash-table? hash-table? hash-table?)]
  [hash-table-difference! (-> hash-table? hash-table? hash-table?)]
  [hash-table-xor! (-> hash-table? hash-table? hash-table?)]
  ))

(struct element (key [value #:mutable] [prev #:mutable] [next #:mutable])
  #:extra-constructor-name make-element
  )

(define (in-elements ht)
  (make-do-sequence
   (thunk
    (values
     identity
     element-next
     (ordered-hash-head ht)
     element?
     #f
     #f))))

(define (element-append! end new-elem)
  (set-element-next! end new-elem)
  (set-element-prev! new-elem end))

(define (raise-no-such-key-error)
  (raise (make-exn:fail "no such key" (current-continuation-marks))))

(struct ordered-hash (table head tail)
  #:mutable

  #:methods gen:dict
  [(define (dict-ref ht key [failure raise-no-such-key-error])
     (hash-table-ref ht key failure))
   (define (dict-set! ht key v)
     (hash-table-set! ht key v))
   (define (dict-remove! ht key)
     (hash-table-delete! ht key))
   (define (dict-iterate-first ht)
     (ordered-hash-head ht))
   (define (dict-iterate-next ht pos)
     (element-next pos))
   (define (dict-iterate-key ht pos)
     (element-key pos))
   (define (dict-iterate-value ht pos)
     (element-value pos))
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
  (ordered-hash (hashmap comparator) #f #f))

(define (elements->hash-table comparator elems)
  (define ht
    (ordered-hash
     (hashmap-unfold null? (lambda (elem-list) (let ([elem (car elem-list)]) (values (element-key elem) elem))) cdr elems comparator)
     #f #f))
  (unless (null? elems)
    (set-ordered-hash-head! ht (car elems))
    (let loop ([prev (car elems)]
               [elems (cdr elems)])
      (match elems
        ['()
         (set-ordered-hash-tail! ht prev)]
        [(list* curr rest)
         (set-element-prev! curr prev)
         (set-element-next! prev curr)
         (loop curr rest)])))
  ht)

(define (hash-table comparator . key+vals)
  (define elems (for/list ([k+v (in-slice 2 (in-list key+vals))])
                  (match-define (list k v) k+v)
                  (make-element k v #f #f)))
  (elements->hash-table comparator elems))

(define (hash-table-unfold stop? mapper successor seed comparator [k 0])
  (define elems
    (let loop ([seed seed]
               [elems '()])
      (if (stop? seed)
          (reverse elems)
          (let-values ([(k v) (mapper seed)])
            (loop (successor seed)
                  (cons (make-element k v #f #f) elems))))))
  (elements->hash-table comparator elems))

(define (alist->hash-table alist comparator [k 0])
  (elements->hash-table comparator
                        (map (lambda (elem) (make-element (car elem) (cdr elem) #f #f)) alist)))

(define (hash-table-contains? ht key)
  (hashmap-contains? (ordered-hash-table ht) key))

(define (hash-table-empty? ht)
  (hashmap-empty? (ordered-hash-table ht)))

(define (hash-table=? value-comparator ht1 ht2)
  (hashmap=? (make-wrapper-comparator element? element-value value-comparator) (ordered-hash-table ht1) (ordered-hash-table ht2)))

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
     (define-values (elem found?) (try-hash-table-ref ht k))
     (cond
       [found?
        (set-element-value! elem v)]
       [else
        (define elem (make-element k v #f #f))
        (set-ordered-hash-table! ht (hashmap-set! (ordered-hash-table ht) k elem))
        (cond
          [(ordered-hash-tail ht)
           (element-append! (ordered-hash-tail ht) elem)
           (set-ordered-hash-tail! ht elem)]
          [else
           (set-ordered-hash-head! ht elem)
           (set-ordered-hash-tail! ht elem)])])]
     [_ ; adding multiple elements
     (let loop ([key+values key+values])
       (match key+values
         ['() (void)]
         [(list* k v remaining-key+values)
          (define-values (elem found?) (try-hash-table-ref ht k))
          (cond
            [found?
             (set-element-value! elem v)]
            [else
             (define elem (make-element k v #f #f))
             (set-ordered-hash-table! ht (hashmap-set! (ordered-hash-table ht) k elem))
             (cond
               [(ordered-hash-tail ht)
                (element-append! (ordered-hash-tail ht) elem)
                (set-ordered-hash-tail! ht elem)]
               [else
                (set-ordered-hash-head! ht elem)
                (set-ordered-hash-tail! ht elem)])])
          (loop remaining-key+values)]))]))

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
  (for/fold ([deleted 0])
            ([key (in-list keys)]
             #:do [(define-values (elem found?) (try-hash-table-ref ht key))]
             #:when found?)
    (match-define (struct element (_ _ prev next)) elem)
    (when (eq? (ordered-hash-head ht) elem)
      (set-ordered-hash-head! ht next))
    (when (eq? (ordered-hash-tail ht) elem)
      (set-ordered-hash-tail! ht prev))
    (when prev
      (set-element-next! prev next))
    (when next
      (set-element-prev! next prev))
    (set-ordered-hash-table! ht (hashmap-delete! (ordered-hash-table ht) key))
    (add1 deleted)))

(define (hash-table-update! ht key updater [failure raise-no-such-key-error] [success identity])
  (define-values (elem found?) (try-hash-table-ref ht key))
  (if found?
      (set-element-value! elem (updater (success (element-value elem))))
      (hash-table-set! ht key (updater (failure)))))

(define (hash-table-update/default! ht key updater default)
  (define-values (elem found?) (try-hash-table-ref ht key))
  (if found?
      (set-element-value! elem (updater (element-value elem)))
      (hash-table-set! ht key (updater default))))

(define (hash-table-pop! ht)
  (when (hash-table-empty? ht)
    (raise-argument-error 'hash-table-pop! "(not/c hash-table-empty?)" ht))
  (define elem (ordered-hash-head ht))
  (match-define (struct element (k v prev next)) elem)
  (set-ordered-hash-table! ht (hashmap-delete! (ordered-hash-table ht) k))
  (set-ordered-hash-head! ht next)
  (when next
    (set-element-prev! next #f))
  (when (eq? (ordered-hash-tail ht) elem)
    (set-ordered-hash-tail! ht #f))
  (values k v))

(define (hash-table-clear! ht)
  (set-ordered-hash-table! ht (hashmap (hash-table-comparator ht)))
  (set-ordered-hash-head! #f)
  (set-ordered-hash-tail! #f))

(define (hash-table-size ht)
  (hashmap-size (ordered-hash-table ht)))

(define (hash-table-keys ht)
  (for/list ([elem (in-elements ht)])
    (element-key elem)))

(define (hash-table-key-vector ht)
  (for/vector #:length (hash-table-size ht)
    ([elem (in-elements ht)])
    (element-key elem)))

(define (hash-table-values ht)
  (for/list ([elem (in-elements ht)])
    (element-value elem)))

(define (hash-table-value-vector ht)
  (for/vector #:length (hash-table-size ht)
    ([elem (in-elements ht)])
    (element-value elem)))

(define (hash-table-entries ht)
  (for/lists (keys entries)
             ([elem (in-elements ht)])
    (values (element-key elem) (element-value elem))))

(define (hash-table-entry-vectors ht)
  (define len (hash-table-size ht))
  (define key-vec (make-vector len))
  (define value-vec (make-vector len))
  (for ([elem (in-elements ht)]
        [i (in-naturals)])
    (vector-set! key-vec i (element-key elem))
    (vector-set! value-vec i (element-value elem)))
  (values key-vec value-vec))

(define (hash-table-find proc ht failure)
  (define result
    (for/first ([elem (in-elements ht)])
      (proc (element-key elem) (element-value elem))))
  (if result
      result
      (failure)))

(define (hash-table-count pred? ht)
  (if (hash-table-empty? ht)
      0
      (for/sum ([elem (in-elements ht)])
        (if (pred? (element-key elem) (element-value elem))
            1
            0))))

(define (hash-table-map proc comparator ht)
  (define new-ht (make-hash-table comparator))
  (for ([elem (in-elements ht)])
    (hash-table-set! new-ht (element-key elem) (proc (element-value elem))))
  new-ht)

(define (hash-table-for-each proc ht)
  (for ([elem (in-elements ht)])
    (proc (element-key elem) (element-value elem))))

(define (hash-table-map! proc ht)
  (for ([elem (in-elements ht)])
    (set-element-value! elem (proc (element-key elem) (element-value elem)))))

(define (hash-table-map->list proc ht)
  (for/list ([elem (in-elements ht)])
    (proc (element-key elem) (element-value elem))))

(define (hash-table-fold proc seed ht)
  (for/fold ([val seed])
            ([elem (in-elements ht)])
    (proc (element-key elem) (element-value elem) val)))

(define (hash-table-prune! proc ht)
  (let loop ([elem (ordered-hash-head ht)]
             [deleted 0])
    (match elem
      [#f deleted]
      [(struct element (k v prev next))
       (cond
         [(proc k v)
          (when (eq? (ordered-hash-head ht) elem)
            (set-ordered-hash-head! ht next))
          (when (eq? (ordered-hash-tail ht) elem)
            (set-ordered-hash-tail! ht prev))
          (when prev
            (set-element-next! prev next))
          (when next
            (set-element-prev! next prev))
          (loop next (add1 deleted))]
         [else (loop next deleted)])])))

(define (hash-table-copy ht [mutable? #t])
  (define new-ht (hash-table-empty-copy ht))
  (for ([elem (in-elements ht)])
    (hash-table-set! new-ht (element-key elem) (element-value elem)))
  new-ht)

(define (hash-table-empty-copy ht)
  (ordered-hash (hashmap (hashmap-comparator ht)) #f #f))

(define (hash-table->alist ht)
  (for/list ([elem (in-elements ht)])
    (cons (element-key elem) (element-value elem))))

(define (hash-table-union! ht1 ht2)
  (for ([elem (in-elements ht2)])
    (unless (hash-table-contains? ht1 (element-key elem))
      (hash-table-set! ht1 (element-key elem) (element-value elem))))
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (for ([elem (in-elements ht1)])
    (unless (hash-table-contains? ht2 (element-key elem))
      (hash-table-delete! ht1 (element-key elem))))
  ht1)

(define (hash-table-difference! ht1 ht2)
  (for ([elem (in-elements ht1)])
    (when (hash-table-contains? ht2 (element-key elem))
      (hash-table-delete! ht1 (element-key elem))))
  ht1)

(define (hash-table-xor! ht1 ht2)
  (hash-table-union! (hash-table-difference! ht1 ht2) ht2))

