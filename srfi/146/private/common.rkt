#lang racket/base

;;; functions used by both srfi/146 and srfi/146/hash

(require racket/contract racket/dict racket/list)

(provide
 mapping-contains?
 mapping-empty?
 mapping-size
 mapping-keys
 mapping-values
 mapping-copy
 mapping->alist
 (contract-out
  [mapping-disjoint? (-> dict? dict? boolean?)]
  [mapping-ref (->* (dict? any/c) ((-> any) (-> any/c any)) any)]
  [mapping-ref/default (-> dict? any/c any/c any/c)]
  [mapping-set (-> dict? any/c ... dict?)]
  [mapping-set! (-> dict? any/c ... dict?)]
  [mapping-replace (-> dict? any/c any/c dict?)]
  [mapping-replace! (-> dict? any/c any/c dict?)]
  [mapping-delete (-> dict? any/c ... dict?)]
  [mapping-delete! (-> dict? any/c ... dict?)]
  [mapping-delete-all (-> dict? list? dict?)]
  [mapping-delete-all! (-> dict? list? dict?)]
  [mapping-intern (-> dict? any/c (-> any/c) (values dict? any/c))]
  [mapping-intern! (-> dict? any/c (-> any/c) (values dict? any/c))]
  [mapping-update (->* (dict? any/c (-> any/c any/c)) ((-> any/c) (-> any/c any/c)) dict?)]
  [mapping-update! (->* (dict? any/c (-> any/c any/c)) ((-> any/c) (-> any/c any/c)) dict?)]
  [mapping-update/default (-> dict? any/c (-> any/c any/c) any/c dict?)]
  [mapping-update!/default (-> dict? any/c (-> any/c any/c) any/c dict?)]
  [mapping-count (-> (-> any/c any/c any/c) dict? exact-nonnegative-integer?)]
  [mapping-any? (-> (-> any/c any/c any/c) dict? boolean?)]
  [mapping-every? (-> (-> any/c any/c any/c) dict? boolean?)]
  [mapping-for-each (-> (-> any/c any/c any) dict? void?)]
  [alist->mapping! (-> dict? (listof (cons/c any/c any/c)) dict?)]
  [mapping-search (-> dict? any/c procedure? procedure? (values dict? any/c))]
  [mapping-search! (-> dict? any/c procedure? procedure? (values dict? any/c))]
  [mapping-filter (-> (-> any/c any/c any/c) dict? dict?)]
  [mapping-filter! (-> (-> any/c any/c any/c) dict? dict?)]
  [mapping-remove (-> (-> any/c any/c any/c) dict? dict?)]
  [mapping-remove! (-> (-> any/c any/c any/c) dict? dict?)]

  [mapping-union (-> dict? dict? ... dict?)]
  [mapping-union! (-> dict? dict? ... dict?)]
  [mapping-intersection (-> dict? dict? ... dict?)]
  [mapping-intersection! (-> dict? dict? ... dict?)]
  [mapping-difference (-> dict? dict? ... dict?)]
  [mapping-difference! (-> dict? dict? ... dict?)]
  [mapping-xor (-> dict? dict? dict?)]
  [mapping-xor! (-> dict? dict? dict?)]

  ))

(define mapping-contains? (procedure-rename dict-has-key? 'mapping-contains?))
(define mapping-empty? (procedure-rename dict-empty? 'mapping-empty?))
(define mapping-size (procedure-rename dict-count 'mapping-size))
(define mapping-keys (procedure-rename dict-keys 'mapping-keys))
(define mapping-values (procedure-rename dict-values 'mapping-values))
(define mapping-copy (procedure-rename dict-copy 'mapping-copy))
(define mapping->alist (procedure-rename dict->list 'mapping->alist))

(define (mapping-disjoint? a b)
  (not (for/or ([k (in-dict-keys a)])
         (dict-has-key? b k))))

(define (mapping-ref hm k
                     [failure (lambda () (raise-arguments-error 'mapping-ref "key not found" "key" k))]
                     [success values])
  (if (dict-has-key? hm k)
      (success (dict-ref hm k))
      (failure)))

(define (mapping-ref/default hm k default)
  ;; Wrap default in a lambda so it doesn't get called if it's a procedure
  (dict-ref hm k (lambda () default)))

(define (mapping-set hm . keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error 'mapping-set "Needs 0 or more sets of key and value arguments"))
  (if (= (length keyvals) 2)
      (dict-set hm (first keyvals) (second keyvals))
      (apply dict-set* hm keyvals)))

(define (mapping-set! hm . keyvals)
  (unless (even? (length keyvals))
    (raise-arguments-error 'mapping-set! "Needs 0 or more sets of key and value arguments"))
  (if (dict-implements? hm 'dict-set!)
      (if (= (length keyvals) 2)
          (begin
            (dict-set! hm (first keyvals) (second keyvals))
            hm)
          (begin
            (apply dict-set*! hm keyvals)
            hm))
      (if (= (length keyvals 2))
          (dict-set hm (first keyvals) (second keyvals))
          (apply dict-set* hm keyvals))))

(define (mapping-replace hm key val)
  (if (dict-has-key? hm key)
      (dict-set hm key val)
      hm))

(define (mapping-replace! hm key val)
  (if (dict-has-key? hm key)
      (if (dict-implements? hm 'dict-set!)
          (begin
            (dict-set! hm key val)
            hm)
          (dict-set hm key val))
      hm))

(define (mapping-delete hm . keys)
  (mapping-delete-all hm keys))

(define (mapping-delete! hm . keys)
  (mapping-delete-all! hm keys))

(define (mapping-delete-all hm keys)
  (for/fold ([table hm])
            ([key (in-list keys)])
    (dict-remove table key)))

(define (mapping-delete-all! hm keys)
  (if (dict-implements? hm 'dict-remove!)
      (begin
        (for ([key (in-list keys)])
          (dict-remove! hm key))
        hm)
      (mapping-delete-all hm keys)))

(define (mapping-intern hm key failure)
  (if (dict-has-key? hm key)
      (values hm (dict-ref hm key))
      (let ([new-val (failure)])
        (values (dict-set hm key new-val) new-val))))

(define (mapping-intern! hm key failure)
  (if (dict-has-key? hm key)
      (values hm (dict-ref hm key))
      (let ([new-val (failure)])
        (if (dict-implements? hm 'dict-set!)
            (begin
              (dict-set! hm key new-val)
              (values hm new-val))
            (values (dict-set hm key new-val) new-val)))))

(define (mapping-update hm key updater
                        [failure (lambda () (raise-arguments-error 'mapping-update "key not found" "key" key))]
                        [success values])
  (mapping-set hm key (updater (mapping-ref hm key failure success))))

(define (mapping-update! hm key updater
                         [failure (lambda () (raise-arguments-error 'mapping-update! "key not found" "key" key))]
                         [success values])
  (if (dict-implements? hm 'dict-set!)
      (mapping-set! hm key (updater (mapping-ref hm key failure success)))
      (mapping-update hm key updater failure success)))

(define (mapping-update/default hm key updater default)
  (mapping-set hm key (updater (mapping-ref/default hm key default))))

(define (mapping-update!/default hm key updater default)
  (if (dict-implements? hm 'dict-set!)
      (mapping-set! hm key (updater (mapping-ref/default hm key default)))
      (mapping-update/default hm key updater default)))

(define (mapping-count pred? hm)
  (for/sum ([(k v) (in-dict hm)]
            #:when (pred? k v))
    1))

(define (mapping-any? pred? hm)
  (for/first ([(k v) (in-dict hm)]
              #:when (pred? k v))
    #t))

(define (mapping-every? pred? hm)
  (if (for/and ([(k v) (in-dict hm)])
        (pred? k v))
      #t
      #f))

(define (alist->hashmap! hm alist)
  (if (dict-implements? hm 'dict-set!)
      (begin
        (for ([pair (in-list alist)]
              #:when (not (dict-has-key? hm (car pair))))
          (dict-set! hm (car pair) (cdr pair)))
        hm)
      (for/fold ([hm hm])
                ([pair (in-list alist)]
                 #:when (not (dict-has-key? hm (car pair))))
        (dict-set hm (car pair) (cdr pair)))))

(define (mapping-for-each proc d)
  (dict-for-each d proc))

(define (mapping-union od1 . rest)
  (for*/fold ([od od1])
             ([odX (in-list rest)]
              [(k v) (in-dict odX)]
              #:when (not (dict-has-key? od k)))
    (dict-set od k v)))

(define (mapping-union! od1 . rest)
  (if (dict-implements? od1 'dict-set!)
      (begin
        (for* ([odX (in-list rest)]
               [(k v) (in-dict odX)]
               #:when (not (dict-has-key? od1 k)))
          (dict-set! od1 k v))
        od1)
      (apply mapping-union od1 rest)))

(define (mapping-intersection od1 . rest)
  (for/fold ([od (dict-clear od1)])
            ([(k v) (in-dict od1)]
             #:when (andmap (lambda (odX) (dict-has-key? odX k)) rest))
    (dict-set od k v)))

(define (mapping-intersection! od1 . rest)
  (if (dict-implements? od1 'dict-set! 'dict-clear)
      (let ([od (dict-clear od1)])
        (for ([(k v) (in-dict od1)]
              #:when (andmap (lambda (odX) (dict-has-key? odX k)) rest))
          (dict-set! od k v))
        od)
      (apply mapping-intersection od1 rest)))

(define (mapping-difference od1 . rest)
  (for/fold ([od (dict-clear od1)])
            ([(k v) (in-dict od1)]
             #:when (not (ormap (lambda (odX) (dict-has-key? odX k)) rest)))
      (dict-set od k v)))

(define (mapping-difference! od1 . rest)
  (if (dict-implements? od1 'dict-remove!)
      (begin
        (for ([k (in-dict-keys od1)]
              #:when (not (ormap (lambda (odX) (dict-has-key? odX k)) rest)))
          (dict-remove! od1 k))
        od1)
      (apply mapping-difference od1 rest)))

(define (mapping-xor od1 od2)
  (mapping-union
   (for/fold ([od (dict-clear od1)])
             ([(k v) (in-dict od1)]
              #:when (not (dict-has-key? od2 k)))
     (dict-set od k v))
   (for/fold ([od (dict-clear od2)])
             ([(k v) (in-dict od2)]
              #:when (not (dict-has-key? od1 k)))
     (dict-set od k v))))

(define (mapping-xor! od1 od2)
  (if (dict-implements? od1 'dict-set! 'dict-clear)
      (let ([od (dict-clear od1)])
        (for ([(k v) (in-dict od1)]
              #:when (not (dict-has-key? od2 k)))
          (dict-set! od k v))
        (for ([(k v) (in-dict od2)]
              #:when (not (dict-has-key? od1 k)))
          (dict-set! od k v))
        od)
      (mapping-xor od1 od2)))

(define (alist->mapping! d alist)
  (if (dict-implements? d 'dict-set!)
      (begin
        (for ([pair (in-list alist)]
              #:when (not (dict-has-key? d (car pair))))
          (dict-set! d (car pair) (cdr pair)))
        d)
      (for/fold ([d d])
                ([pair (in-list alist)]
                 #:when (not (dict-has-key? d (car pair))))
        (dict-set d (car pair) (cdr pair)))))

(define (mapping-search d key failure success)
  (let ([insert (lambda (value obj)
                  (values (dict-set d key value) obj))]
        [ignore (lambda (obj)
                  (values d obj))]
        [update (lambda (new-key new-value obj)
                  (values (dict-set (dict-remove d key) new-key new-value) obj))]
        [remove (lambda (obj)
                  (values (dict-remove d key) obj))])
    (if (dict-has-key? d key)
        (success key (dict-ref d key) update remove)
        (failure insert ignore))))

(define (mapping-search! d key failure success)
  (if (dict-implements? d 'dict-set! 'dict-remove!)
      (let ([insert (lambda (value obj)
                      (dict-set! d key value)
                      (values d obj))]
            [ignore (lambda (obj)
                      (values d obj))]
            [update (lambda (new-key new-value obj)
                      (dict-remove! d key)
                      (dict-set! d new-key new-value)
                      (values d obj))]
            [remove (lambda (obj)
                      (dict-remove! d key)
                      (values d obj))])
        (if (dict-has-key? d key)
            (success key (dict-ref d key) update remove)
            (failure insert ignore)))
      (mapping-search d key failure success)))

(define (mapping-filter pred? d)
  (for/fold ([new-d (dict-clear d)])
            ([(k v) (in-dict d)]
             #:when (pred? k v))
    (dict-set new-d k v)))

(define (mapping-filter! pred? d)
  (if (dict-implements? d 'dict-set! 'dict-clear)
      (let ([new-d (dict-clear d)])
        (for ([(k v) (in-dict d)]
              #:when (pred? k v))
          (dict-set! new-d k v))
        new-d)
      (mapping-filter pred? d)))

(define (mapping-remove pred? d)
  (mapping-filter (lambda (k v) (not (pred? k v))) d))

(define (mapping-remove! pred? d)
  (mapping-filter! (lambda (k v) (not (pred? k v))) d))
