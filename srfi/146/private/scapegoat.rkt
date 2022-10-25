#lang racket/base
;;; A trimmed down version of Shawn's scapegoat-tree package ported to
;;; use SRFI-128 comparators and SRFI-158 generators instead of
;;; dict/order order objects and racket/generator generators.

(require racket/contract racket/dict racket/stream data/order
         "../../128.rkt" "../../158.rkt" "../../190.rkt")

(provide
 scapegoat-tree-has-key? scapegoat-tree-ref
 scapegoat-tree-set scapegoat-tree-set!
 scapegoat-tree-clear
 scapegoat-tree-iterate-key
 (contract-out
  [mapping? predicate/c]
  [make-scapegoat-tree (-> comparator? mapping?)]
  [mapping-key-comparator (-> mapping? comparator?)]
  ))

(define scapegoat-tree-height-factor (make-parameter 2/3))
(define scapegoat-tree-rebalance-on-copy (make-parameter #t))

(define (%apply-hash st hash-func)
  (for/fold ([hash (+
                    (scapegoat-tree-count st)
                    (hash-func (scapegoat-tree-key-comparator st)))])
            ([(k v) (in-dict st)])
    (bitwise-xor hash (+ (hash-func k) (hash-func v)))))

(struct node (key val left right))
(struct scapegoat-tree ((count #:mutable) (max-count #:mutable) key-comparator (root #:mutable))
  #:methods gen:dict
  [(define (dict-ref st key [default (lambda () (raise-arguments-error 'dict-ref "key not found" "key" key))]) (scapegoat-tree-ref st key default))
   (define (dict-has-key? st key) (scapegoat-tree-has-key? st key))
   (define (dict-empty? st) (scapegoat-tree-empty? st))
   (define (dict-count st) (scapegoat-tree-count st))
   (define (dict-set st key v) (scapegoat-tree-set st key v))
   (define (dict-set! st key v) (scapegoat-tree-set! st key v))
   (define (dict-remove st key) (scapegoat-tree-delete st key))
   (define (dict-remove! st key) (scapegoat-tree-delete! st key))
   (define (dict-iterate-first st) (scapegoat-tree-iterate-first st))
   (define (dict-iterate-next st it) (scapegoat-tree-iterate-next st it))
   (define (dict-iterate-key st it) (scapegoat-tree-iterate-key st it))
   (define (dict-iterate-value st it) (scapegoat-tree-iterate-value st it))
   (define (dict-copy st) (scapegoat-tree-copy st))
   (define (dict-clear st) (scapegoat-tree-clear st))
   (define (dict-clear! st) (scapegoat-tree-clear! st))]

  #:methods gen:ordered-dict
  [(define (dict-iterate-least st) (scapegoat-tree-iterate-first st))
   (define (dict-iterate-greatest st) (scapegoat-tree-maximum-iterator st))
   (define (dict-iterate-least/>? st key) (scapegoat-tree-least-iterator st key >?))
   (define (dict-iterate-least/>=? st key) (scapegoat-tree-least-iterator st key >=?))
   (define (dict-iterate-greatest/<? st key) (scapegoat-tree-great-iterator st key <?))
   (define (dict-iterate-greatest/<=? st key) (scapegoat-tree-great-iterator st key <=?))]

  #:methods gen:equal+hash
  [(define (equal-proc a b real-equal?)
     (and
      (= (scapegoat-tree-count a) (scapegoat-tree-count b))
      (eq? (scapegoat-tree-key-comparator a) (scapegoat-tree-key-comparator b))
      (for/and ([(k v) (in-dict a)])
        (and (dict-has-key? b k)
             (real-equal? v (dict-ref b k))))))
   (define hash-proc %apply-hash)
   (define hash2-proc %apply-hash)])

(define mapping? (procedure-rename scapegoat-tree? 'mapping?))
(define mapping-key-comparator (procedure-rename scapegoat-tree-key-comparator 'mapping-key-comparator))

(define (make-scapegoat-tree cmp)
  (scapegoat-tree 0 0 cmp #f))

(define (scapegoat-tree-empty? st) (= (scapegoat-tree-count st) 0))

(define (node-ref cmp node key default)
  (cond
    ((node? node)
     (cond
       ((=? cmp key (node-key node)) (node-val node)) ; Match
       ((<? cmp key (node-key node)) (node-ref cmp (node-left node) key default))
       (else (node-ref cmp (node-right node) key default))))
    ((procedure? default) (default))
    (else default)))

(define-syntax-rule (check-key-contract function st key)
  (unless (comparator-test-type (scapegoat-tree-key-comparator st) key)
    (raise-argument-error function "key fails type check" key)))

(define (scapegoat-tree-ref st key [default (lambda () (raise-arguments-error 'scapegoat-tree-ref "key not found" "key" key))])
  (node-ref (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) key default))

(define %key-not-found (box #f))

(define (scapegoat-tree-has-key? st key)
  (not (eq? (scapegoat-tree-ref st key %key-not-found) %key-not-found)))

(define (size node)
  (if (node? node)
      (+ (size (node-left node)) (size (node-right node)) 1)
      0))

(define (rebalance cmp root size)
  (letrec ([arr (make-vector size)]
           [pack (lambda (node i)
                   (if (node? node)
                       (let ([i (pack (node-left node) i)])
                         (vector-set! arr i node)
                         (pack (node-right node) (add1 i)))
                       i))]
           [build (lambda (i ns)
                    (if (= ns 0)
                        #f
                        (let* ([m (quotient ns 2)]
                               [p (+ i m)])
                          (struct-copy node (vector-ref arr p) [left (build i m)] [right (build (add1 p) (- ns m 1))]))))])
    (pack root 0)
    (build 0 size)))

(define (scapegoat-tree-copy st)
  (if (scapegoat-tree-rebalance-on-copy)
      (struct-copy scapegoat-tree st [max-count (scapegoat-tree-count st)] [root (rebalance (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) (scapegoat-tree-count st))])
      (struct-copy scapegoat-tree st)))

(define (scapegoat-tree-clear st)
  (struct-copy scapegoat-tree st [count 0] [max-count 0] [root #f]))

(define (scapegoat-tree-clear! st)
  (set-scapegoat-tree-count! st 0)
  (set-scapegoat-tree-max-count! st 0)
  (set-scapegoat-tree-root! #f))

(define (scapegoat? left-size right-size)
  (let ([node-factor (* (scapegoat-tree-height-factor) (+ left-size right-size 1))])
    (or (> left-size node-factor)
        (> right-size node-factor))))

(define (node-insert q cmp root key v)
  (letrec ([helper
            (lambda (nod depth)
              (if (node? nod)
                  (cond
                    ((=? cmp key (node-key nod)) (values (struct-copy node nod [val v]) depth 0 #f #t)) ; Existing key; overwrite.
                    ((<? cmp key (node-key nod))
                     (let*-values ([(left-child child-depth left-size inserted? rebalanced?) (helper (node-left nod) (add1 depth))]
                                   [(right-size) (if rebalanced? 0 (size (node-right nod)))]
                                   [(size) (+ left-size right-size 1)])
                       (cond
                         (rebalanced? (values (struct-copy node nod [left left-child]) child-depth 0 inserted? rebalanced?))
                         ((scapegoat? left-size right-size) ; Scapegoat; rebalance
                          (values (rebalance cmp (struct-copy node nod [left left-child]) size) child-depth size inserted? #t))
                         (else (values (struct-copy node nod [left left-child]) child-depth size inserted? rebalanced?)))))
                    (else
                     (let*-values ([(right-child child-depth right-size inserted? rebalanced?) (helper (node-right nod) (add1 depth))]
                                   [(left-size) (if rebalanced? 0 (size (node-left nod)))]
                                   [(size) (+ left-size right-size 1)])
                       (cond
                         (rebalanced? (values (struct-copy node nod [right right-child]) child-depth 0 inserted? rebalanced?))
                         ((scapegoat? left-size right-size) ; Scapegoat; rebalance
                          (values (rebalance cmp (struct-copy node nod [right right-child]) size) child-depth size inserted? #t))
                         (else
                          (values (struct-copy node nod [right right-child]) child-depth size inserted? rebalanced?))))))
                  (values (node key v #f #f) depth 1 #t #f)))])
    (let-values ([(new-root depth size inserted? rebalanced?) (helper root 0)])
      (values new-root inserted?))))

(define (scapegoat-tree-set st key v)
  (check-key-contract 'scapegoat-tree-set st key)
  (let-values ([(new-root inserted?) (node-insert (scapegoat-tree-max-count st) (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) key v)])
    (struct-copy scapegoat-tree st [root new-root] [count (+ (scapegoat-tree-count st) (if inserted? 1 0))] [max-count (+ (scapegoat-tree-max-count st) (if inserted? 1 0))])))

(define (scapegoat-tree-set! st key v)
  (check-key-contract 'scapegoat-tree-set! st key)
  (let-values ([(new-root inserted?) (node-insert (scapegoat-tree-max-count st) (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) key v)])
    (set-scapegoat-tree-root! st new-root)
    (when inserted?
      (set-scapegoat-tree-count! st (add1 (scapegoat-tree-count st)))
      (set-scapegoat-tree-max-count! st (add1 (scapegoat-tree-max-count st))))))

(define (need-rebalance-after-delete? st)
  (<= (sub1 (scapegoat-tree-count st)) (* (scapegoat-tree-height-factor) (scapegoat-tree-max-count st))))

(define (node-remove-min root)
  (if (node? (node-left root))
      (let-values ([(min-node new-left) (node-remove-min (node-left root))])
        (values min-node (struct-copy node root [left new-left])))
      (values (struct-copy node root [left #f] [right #f]) (node-right root))))

(define (node-remove cmp root key)
  (if (node? root)
      (cond
        ((=? cmp key (node-key root)) ; the node to remove
         (cond
           ((and (eq? (node-left root) #f)
                 (eq? (node-right root) #f))
            (values #f #t)) ;; Leaf
           ((and (eq? (node-left root) #f)
                 (not (eq? (node-right root) #f)))
            (values (node-right root) #t)) ;; Only right child
           ((and (not (eq? (node-left root) #f))
                 (eq? (node-right root) #f))
            (values (node-left root) #t)) ;; Only left child
           (else ;; Two children
            (let-values ([(min-of-right new-right) (node-remove-min (node-right root))])
              (values (struct-copy node min-of-right [left (node-left root)] [right new-right]) #t)))))
        ((<? cmp key (node-key root))
         (let-values ([(new-left removed?) (node-remove cmp (node-left root) key)])
           (if removed?
               (values (struct-copy node root [left new-left]) #t)
               (values root #f))))
        (else
         (let-values ([(new-right removed?) (node-remove cmp (node-right root) key)])
           (if removed?
               (values (struct-copy node root [right new-right]) #t)
               (values root #f)))))
      (values root #f)))

(define (scapegoat-tree-delete st key)
  (let-values ([(new-root removed?) (node-remove (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) key)]
               [(new-n) (sub1 (scapegoat-tree-count st))])
    (cond
      ((and removed? (need-rebalance-after-delete? st))
       (struct-copy scapegoat-tree st
                    [root (rebalance (scapegoat-tree-key-comparator st) new-root new-n)]
                    [max-count new-n]
                    [count new-n]))
      (removed? (struct-copy scapegoat-tree st [root new-root] [count new-n]))
      (else st))))

(define (scapegoat-tree-delete! st key)
  (let-values ([(new-root removed?) (node-remove (scapegoat-tree-key-comparator st) (scapegoat-tree-root st) key)]
               [(new-n) (sub1 (scapegoat-tree-count st))])
    (cond
      ((and removed? (need-rebalance-after-delete? st))
       (set-scapegoat-tree-root! st (rebalance (scapegoat-tree-key-comparator st) new-root new-n))
       (set-scapegoat-tree-max-count! st new-n)
       (set-scapegoat-tree-count! st new-n))
      (removed?
       (set-scapegoat-tree-root! st new-root)
       (set-scapegoat-tree-count! st new-n))
      (else (null)))))

(struct scapegoat-tree-iterator (st elems))

(define (scapegoat-tree-iterate-first st)
  (if (scapegoat-tree-empty? st)
      #f
      (let ([g (coroutine-generator
                (let loop ([nod (scapegoat-tree-root st)])
                  (when (node? nod)
                    (loop (node-left nod))
                    (yield nod)
                    (loop (node-right nod)))))])
        (scapegoat-tree-iterator st (generator->stream g)))))

(define-syntax-rule (check-iterator function st st-i)
  (unless (eq? st (scapegoat-tree-iterator-st st-i))
    (raise-argument-error function "Use of scapegoat tree iterator with different tree" st-i)))

(define (scapegoat-tree-iterate-next st st-i)
  (check-iterator 'scapegoat-tree-iterate-next st st-i)
  (if (stream-empty? (scapegoat-tree-iterator-elems st-i))
      #f
      (let ([new-elems (stream-rest (scapegoat-tree-iterator-elems st-i))])
        (if (stream-empty? new-elems)
            #f
            (struct-copy scapegoat-tree-iterator st-i [elems new-elems])))))

(define (scapegoat-tree-maximum-iterator st)
  (if (scapegoat-tree-empty? st)
      #f
      (let loop ([nod (scapegoat-tree-root st)])
        (if (node? (node-right nod))
            (loop (node-right nod))
            (scapegoat-tree-iterator st (stream nod))))))

(define (scapegoat-tree-least-iterator st key order)
  (if (scapegoat-tree-empty? st)
      #f
      (let* ([cmp (scapegoat-tree-key-comparator st)]
             [g (coroutine-generator
                 (let loop ([nod (scapegoat-tree-root st)])
                   (when (node? nod)
                     (when (order cmp (node-key nod) key)
                       (loop (node-left nod))
                       (yield nod))
                     (loop (node-right nod)))))]
             [least (g)])
        (if (eof-object? least)
            #f
            (scapegoat-tree-iterator st (stream-cons least (generator->stream g)))))))

(define (scapegoat-tree-great-iterator st key order)
  (if (scapegoat-tree-empty? st)
      #f
      (let* ([cmp (scapegoat-tree-key-comparator st)]
             [g (coroutine-generator
                 (let loop ([nod (scapegoat-tree-root st)])
                   (cond
                     ((eq? nod #f) (void))
                     ((order cmp (node-key nod) key)
                      (cond
                        ((and (node? (node-right nod))
                              (order cmp (node-key (node-right nod)) key))
                         (loop (node-right nod)))
                        (else
                         (yield nod)
                         (when (node? (node-right nod))
                           (loop (node-right nod))))))
                     (else
                      (let loop2 ([nod nod])
                        (when (node? nod)
                          (when (node? (node-left nod))
                            (loop (node-left nod)))
                          (yield nod)
                          (loop2 (node-right nod))))))))]
             [great (g)])
        (if (eof-object? great)
            #f
            (scapegoat-tree-iterator st (stream-cons great (generator->stream g)))))))

(define (scapegoat-tree-iterate-key st st-i)
  (check-iterator 'scapegoat-tree-iterate-key st st-i)
  (node-key (stream-first (scapegoat-tree-iterator-elems st-i))))

(define (scapegoat-tree-iterate-value st st-i)
  (check-iterator 'scapegoat-tree-iterate-value st st-i)
  (node-val (stream-first (scapegoat-tree-iterator-elems st-i))))
