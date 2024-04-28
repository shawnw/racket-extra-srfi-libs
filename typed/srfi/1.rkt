#lang typed/racket/base/shallow

;;; SRFI-1 functions imported to Typed Racket

(module+ test (require typed/rackunit))

(require
  (only-in racket/list
           make-list
           first second third fourth fifth sixth seventh eighth ninth tenth
           take drop take-right drop-right split-at
           last last-pair
           partition
           remove-duplicates
           ))

(define-type (Circular-Listof a) (Pairof a (Circular-Listof a)))

; Proper or circular lists. Seems to get turned into just (Listof a) by TR.
(define-type (CListof a) (U (Listof a) (Circular-Listof a)))

(require/typed/provide
 srfi/1
 [xcons (All (a d) (d a -> (Pairof a d)))]
 [list-copy (All (a) (Listof a) -> (Listof a))]
 [circular-list (All (a) (a a * -> (Circular-Listof a)))]
 [iota (case->
        (->* (Integer) (Integer Integer) (Listof Integer))
        (->* (Real) (Real Real) (Listof Real)))]
 [circular-list? (Any -> Boolean) #;(Any -> Boolean : (Circular-Listof Any))]
 [dotted-list? (Any -> Boolean)]
 [null-list? (All (a) ((CListof a) -> Boolean))]
 [not-pair? (Any -> Boolean)]
 [list= (All (a) (a a -> Any) (Listof a) * -> Boolean)]
 [car+cdr (All (a b) (Pairof a b) -> (Values a b))]
 [length+ (All (a) (CListof a) -> (Option Nonnegative-Integer))]
 [concatenate (All (a) (Listof (Listof a)) -> (Listof a))]
 [append-reverse (All (a) (Listof a) (Listof a) -> (Listof a))]
 [zip (All (a b ...) (CListof a) (CListof b) ... b -> (Listof (List a b ... b)))]
 [unzip1 (All (a b ...) (Listof (List a b ... b)) -> (Listof a))]
 [unzip2 (All (a b c ...) (Listof (List a b c ... c)) -> (Values (Listof a) (Listof b)))]
 [unzip3 (All (a b c d ...) (Listof (List a b c d ... d)) -> (Values (Listof a) (Listof b) (Listof c)))]
 [unzip4 (All (a b c d e ...) (Listof (List a b c d e ... e)) -> (Values (Listof a) (Listof b) (Listof c) (Listof d)))]
 [unzip5 (All (a b c d e f ...) (Listof (List a b c d e f ... f)) -> (Values (Listof a) (Listof b) (Listof c) (Listof d) (Listof e)))]
 [count (All (a b ...) (a b ... b -> Any) (CListof a) (CListof b) ... b -> Nonnegative-Integer)]
 ;Doesn't compile, sigh
 ;[fold (All (a c b ...) (a b ... b c -> c) c (CListof a) (CListof b) ... b -> c)]
 [fold (All (a b c d)
            (case->
             (-> (a b -> b) b (CListof a) b)
             (-> (a b c -> c) c (CListof a) (CListof b) c)
             (-> (a b c d -> d) d (CListof a) (CListof b) (CListof c) d)))] ; Like foldl, doesn't support unlimited lists
 [fold-right (All (a b c d)
                  (case->
                   (-> (a b -> b) b (CListof a) b)
                   (-> (a b c -> c) c (CListof a) (CListof b) c)
                   (-> (a b c d -> d) d (CListof a) (CListof b) (CListof c) d)))] ; Like foldr, doesn't support unlimited lists
 [pair-fold (All (a b c d)
                 (case->
                  (-> ((Listof a) b -> b) b (Listof a) b)
                  (-> ((Listof a) (Listof b) c -> c) c (Listof a) (Listof b) c)
                  (-> ((Listof a) (Listof b) (Listof c) d -> d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldl, doesn't support unlimited lists
 [pair-fold-right (All (a b c d)
                       (case->
                        (-> ((Listof a) b -> b) b (Listof a) b)
                        (-> ((Listof a) (Listof b) c -> c) c (Listof a) (Listof b) c)
                        (-> ((Listof a) (Listof b) (Listof c) d -> d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldr, doesn't support unlimited lists
 [reduce (All (a b)
              (case->
               (-> (a a -> a) b Null b)
               (-> (a a -> a) b (Pairof a (Listof a)) a)
               (-> (a a -> a) b (Listof a) (U a b))))]
 [reduce-right (All (a b)
                    (case->
                     (-> (a a -> a) b Null b)
                     (-> (a a -> a) b (Pairof a (Listof a)) a)
                     (-> (a a -> a) b (Listof a) (U a b))))]
 [unfold (All (a b) (->* ((a -> Any) (a -> b) (a -> a) a) ((-> a (Listof b))) (Listof b)))]
 [unfold-right (All (a b) (->* ((a -> Any) (a -> b) (a -> a) a) ((Listof b)) (Listof b)))]
 [map (All (a b c ...) (a c ... c -> b) (CListof a) (CListof c) ... c -> (Listof b))]
 [for-each (All (a b ...) (a b ... b -> Any) (CListof a) (CListof b) ... b -> Void)]
 [append-map (All (a b c ...) (a c ... c -> (Listof b)) (CListof a) (CListof c) ... c -> (Listof b))]
 [map-in-order (All (a b c ...) (a c ... c -> b) (CListof a) (CListof c) ... c -> (Listof b))]
 [pair-for-each (All (a b ...) ((Listof a) (Listof b) ... b -> Any) (Listof a) (Listof b) ... b -> Void)]
 [filter-map (All (a b c ...) (a c ... c -> (Option b)) (CListof a) (CListof c) ... c -> (Listof b))]
 [filter (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [remove (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [take-while (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [drop-while (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [span (All (a) (a -> Any) (CListof a) -> (Values (Listof a) (CListof a)))]
 [break (All (a) (a -> Any) (CListof a) -> (Values (Listof a) (CListof a)))]
 [any (All (a c b ...) (a b ... b -> (Option c)) (CListof a) (CListof b) ... b -> (Option c))]
 [every (All (a c b ...) (a b ... b -> (Option c)) (CListof a) (CListof b) ... b -> (Option c))]
 [list-index (All (a b ...) (a b ... b -> Any) (CListof a) (CListof b) ... b -> (Option Nonnegative-Integer))]
 [delete (All (a) (->* (a (Listof a)) ((a a -> Any)) (Listof a)))]
 [alist-cons (All (a b) a b (Listof (Pairof a b)) -> (Listof (Pairof a b)))]
 [alist-copy (All (a b) (Listof (Pairof a b)) -> (Listof (Pairof a b)))]
 [alist-delete (All (a b) (->* (a (Listof (Pairof a b))) ((a a -> Any)) (Listof (Pairof a b))))]
 [lset<= (All (a) (a a -> Any) (Listof a) * -> Boolean)]
 [lset= (All (a) (a a -> Any) (Listof a) * -> Boolean)]
 [lset-adjoin (All (a) (a a -> Any) (Listof a) a * -> (Listof a))]
 [lset-union (All (a) (a a -> Any) (Listof a) * -> (Listof a))]
 [lset-intersection (All (a) (a a -> Any) (Listof a) (Listof a) * -> (Listof a))]
 [lset-difference (All (a) (a a -> Any) (Listof a) (Listof a) * -> (Listof a))]
 [lset-xor (All (a) (a a -> Any) (Listof a) * -> (Listof a))]
 [lset-diff+intersection (All (a) (a a -> Any) (Listof a) (Listof a) * -> (Values (Listof a) (Listof a)))]

 )
(provide
 Circular-Listof
 make-list
 first second third fourth fifth sixth seventh eighth ninth tenth
 take drop take-right drop-right split-at
 last last-pair
 partition
 (rename-out
  [list* cons*]
  [build-list list-tabulate]
  [list? proper-list?]
  [take take!]
  [drop-right drop-right!]
  [split-at split-at!]
  [append append!]
  [concatenate concatenate!]
  [reverse reverse!]
  [append-reverse append-reverse!]
  [append-map append-map!]
  [map map!]
  [filter filter!]
  [partition partition!]
  [remove remove!]
  [findf find]
  [memf find-pair]
  [take-while take-while!]
  [span span!]
  [break break!]
  [delete delete!]
  [remove-duplicates delete-duplicates]
  [remove-duplicates delete-duplicates!]
  [alist-delete alist-delete!]
  [lset-union lset-union!]
  [lset-intersection lset-intersection!]
  [lset-difference lset-difference!]
  [lset-xor lset-xor!]
  [lset-diff+intersection lset-diff+intersection!]
  ))

(module+ test
  (require/typed rackunit
                 [check-within (->* (Any Any Number) ((U String False)) Void)])

  (define-syntax test-values
    (syntax-rules (values)
      ((test-values name expr (values expected ...))
       (test-equal? name
                    (call-with-values (lambda () expr) list)
                    (list expected ...)))))

  (test-equal? "cons* 1" (list* 1 2 3 4) '(1 2 3 . 4))
  (test-equal? "cons* 2" (list* 1) 1)
  (test-equal? "make-list" (make-list 4 'c) '(c c c c))
  (test-equal? "list-tabulate" ((inst build-list Integer) 4 values) '(0 1 2 3))
  (test-equal? "iota 1" (iota 5) '(0 1 2 3 4))
  (test-case "iota 2" (check-within (iota 5 0 -0.1) '(0 -0.1 -0.2 -0.3 -0.4) 0.0000001))
  (test-true "list= 1" (list= eq?))
  (test-true "list= 2" (list= eq? '(a)))
  (test-false "list= 3" (list= eq? '(a b) '(a)))
  (test-false "list= 4" (list= eq? '(a b) '(a b) '(a)))
  (test-true "list= 5" (list= eq? '(a b) '(a b)))
  (define tens '(1 2 3 4 5 6 7 8 9 10 11 12))
  (test-equal? "first" (first tens) 1)
  (test-equal? "second" (second tens) 2)
  (test-equal? "third" (third tens) 3)
  (test-equal? "fourth" (fourth tens) 4)
  (test-equal? "fifth" (fifth tens) 5)
  (test-equal? "sixth" (sixth tens) 6)
  (test-equal? "seventh" (seventh tens) 7)
  (test-equal? "eighth" (eighth tens) 8)
  (test-equal? "ninth" (ninth tens) 9)
  (test-equal? "tenth" (tenth tens) 10)
  (test-equal? "take 1" (take (list 'a 'b 'c 'd 'e) 2) (list 'a 'b))
  ;(test-equal? "take 2" ((inst take (U Integer Symbol)) (list* 1 2 3 'd) 2) (list 1 2))
  ;(test-equal? "take 3" (take (list* 1 2 3 'd) 3) (list 1 2 3))
  (test-equal? "drop 1" (drop (list 'a 'b 'c 'd 'e) 2) (list 'c 'd 'e))
  ;(test-equal? "drop 2" (drop (list* 1 2 3 'd) 2) (cons 3 'd))
  ;(test-equal? "drop 3" (drop (list* 1 2 3 'd) 3) 'd)
  (test-equal? "take-right 1" (take-right (list 'a 'b 'c 'd 'e) 2) (list 'd 'e))
  ;(test-equal? "take-right 2" (take-right (cons* 1 2 3 'd) 2) (cons* 2 3 'd))
  ;(test-equal? "take-right 3" (take-right (cons* 1 2 3 'd) 0) 'd)
  (test-equal? "drop-right 1" (drop-right (list 'a 'b 'c 'd 'e) 2) (list 'a 'b 'c))
  ;(test-equal? "drop-right 2" (drop-right (cons* 1 2 3 'd) 2) (list 1))
  ;(test-equal? "drop-right 3" (drop-right (cons* 1 2 3 'd) 0) (list 1 2 3))
  (test-values "split-at 1" (split-at (list 'a 'b 'c 'd 'e 'f 'g 'h) 3)
               (values (list 'a 'b 'c) (list 'd 'e 'f 'g 'h)))
  (test-equal? "last 1" (last (list 'a 'b 'c)) 'c)
  (test-equal? "last-pair 1" (last-pair (list 'a 'b 'c)) (cons 'c '()))
  (test-equal? "length+ 1" (length+ '(1 2 3 4)) 4)
  (test-equal? "length+ 2" (length+ (circular-list 1 2 3 4)) #f)
  (test-equal? "append-reverse 1" (append-reverse '(c b a) '(d)) '(a b c d))
  ;(test-equal? "append-reverse 2" (append-reverse '(c b a) 'd) '(a b c . d))
  (test-values "unzip2" (unzip2 (list (list 1 'one) (list 2 'two) (list 3 'three)))
               (values '(1 2 3) '(one two three)))
  (test-equal? "count 1" (count even? '(3 1 4 1 5 9 2 5 6)) 3)
  ;(test-equal? "count 2" (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)) 3)
  ;(test-equal? "count 3" (count < '(3 1 4 1) (circular-list 1 10)) 2)
  (test-equal? "filter-map 1"
               (filter-map (lambda ([x : (U Number Symbol)]) (and (number? x) (* x x))) '(a 1 b 3 c 7))
               '(1 9 49))
  (test-equal? "filter 1" (filter even? '(0 7 8 8 43 -4)) '(0 8 8 -4))
  (test-values "partition 1" (partition symbol? (list 'one 2 3 'four 'five 6))
               (values '(one four five) '(2 3 6)))
  (test-equal? "find 1" (findf even? '(3 1 4 1 5 9)) 4)
  (test-equal? "find-tail 1" (memf even? '(3 1 37 -8 -5 0 0)) '(-8 -5 0 0))
  (test-equal? "find-tail 2" (memf even? '(3 1 37 -5)) #f)
  (test-equal? "take-while 1" (take-while even? '(2 18 3 10 22 9)) '(2 18))
  (test-equal? "drop-while 1" (drop-while even? '(2 18 3 10 22 9)) '(3 10 22 9))
  (test-values "span 1" (span even? '(2 18 3 10 22 9))
               (values '(2 18) '(3 10 22 9)))
  (test-values "break 1" (break even? '(3 1 4 1 5 9))
               (values '(3 1) '(4 1 5 9)))
  (test-true "any 1" (any integer? (list 'a 3 'b 2.7)))
  (test-false "any 2" (any integer? (list 'a 3.1 'b 2.7)))
  (test-true "any 3" (any < '(3 1 4 1 5) '(2 7 1 8 2)))
  (test-true "every 1" (every integer? '(1 2 3 4)))
  (test-false "every 2" (every integer? '(1 2 3.14 4)))
  (test-true "every 3" (every < '(1 2 3 4) '(2 3 4 5)))
  (test-false "every 4" (every < '(1 2 3 4) '(2 3 4 1)))
  (test-equal? "list-index 1" (list-index even? '(3 1 4 1 5 9)) 2)
  (test-equal? "list-index 2" (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) 1)
  (test-false "list-index 3" (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))
  (test-equal? "delete-duplicates 1" (remove-duplicates '(a b a c a b c z)) '(a b c z))
  (test-equal? "delete-duplicates 2" (remove-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                                                          (lambda ([x : (Pair Symbol Integer)] [y : (Pair Symbol Integer)]) (eq? (car x) (car y))))
               '((a . 3) (b . 7) (c . 1)))

  (test-true "lset<= 1" (lset<= eq? (list 'a) (list 'a 'b 'a) (list 'a 'b 'c 'c)))
  (test-true "lset<= 2" (lset<= eq?))
  (test-true "lset<= 3" (lset<= eq? (list 'a)))

  (test-true "lset= 1" (lset= eq? (list 'b 'e 'a) (list 'a 'e 'b) (list 'e 'e 'b 'a)))
  (test-true "lset= 2" (lset= eq?))
  (test-true "lset= 3" (lset= eq? '(a)))

  (test-equal? "lset-adjoin 1" (lset-adjoin eq? (list 'a 'b 'c 'd 'c 'e) 'a 'e 'i 'o 'u)
               (list 'u 'o 'i 'a 'b 'c 'd 'c 'e))

  (test-equal? "mlset-union 1" (lset-union eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))
               (list 'u 'o 'i 'a 'b 'c 'd 'e))
  (test-equal? "lset-union 2" (lset-union eq? (list 'a 'a 'c) (list 'x 'a 'x)) (list 'x 'a 'a 'c))
  (test-equal? "lset-union 3" (lset-union eq?) '())
  (test-equal? "lset-union 4" (lset-union eq? (list 'a 'b 'c)) (list 'a 'b 'c))

  (test-equal? "lset-intersection 1" (lset-intersection eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))
               (list 'a 'e))
  (test-equal? "lset-intersection 2" (lset-intersection eq? (list 'a 'x 'y 'a) (list 'x 'a 'x 'z))
               (list 'a 'x 'a))
  (test-equal? "lset-intersection 3" (lset-intersection eq? (list 'a 'b 'c)) (list 'a 'b 'c))

  (test-equal? "lset-difference 1" (lset-difference eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))
               (list 'b 'c 'd))
  (test-equal? "lset-difference 2" (lset-difference eq? (list 'a 'b 'c)) (list 'a 'b 'c))

  (test-equal? "lset-xor 1" (lset-xor eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))
               '(u o i b c d) #;(list 'd 'c 'b 'i 'o 'u))
  (test-equal? "lset-xor 2" (lset-xor eq?) '())
  (test-equal? "lset-xor 3" (lset-xor eq? (list 'a 'b 'c 'd 'e)) (list 'a 'b 'c 'd 'e))
  )
