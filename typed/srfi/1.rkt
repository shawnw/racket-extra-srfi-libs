#lang typed/racket/base/shallow

;;; SRFI-1 functions imported to Typed Racket

(require
  (only-in racket/list
           first second third fourth fifth sixth seventh eighth ninth tenth
           take drop take-right drop-right split-at
           last last-pair
           partition
           remove-duplicates
           ))
(require/typed/provide
 srfi/1
 [xcons (All (a d) (d a -> (Pairof a d)))]
 [list-copy (All (a) (Listof a) -> (Listof a))]
 [iota (case->
        (->* (Integer) (Integer Integer) (Listof Integer))
        (->* (Real) (Real Real) (Listof Real)))]
 [circular-list? (Any -> Boolean)]
 [dotted-list? (Any -> Boolean)]
 [null-list? (Any -> Boolean)]
 [not-pair? (Any -> Boolean)]
 [list= (All (a) (a a -> Any) (Listof a) * -> Boolean)]
 [car+cdr (All (a b) (Pairof a b) -> (Values a b))]
 [length+ (Any -> (Option Nonnegative-Integer))]
 [concatenate (All (a) (Listof (Listof a)) -> (Listof a))]
 [append-reverse (All (a) (Listof a) (Listof a) -> (Listof a))]
 [zip (All (a b ...) (Listof a) (Listof b) ... b -> (Listof (List a b ... b)))]
 [unzip1 (All (a b ...) (Listof (List a b ... b)) -> (Listof a))]
 [unzip2 (All (a b c ...) (Listof (List a b c ... c)) -> (Values (Listof a) (Listof b)))]
 [unzip3 (All (a b c d ...) (Listof (List a b c d ... d)) -> (Values (Listof a) (Listof b) (Listof c)))]
 [unzip4 (All (a b c d e ...) (Listof (List a b c d e ... e)) -> (Values (Listof a) (Listof b) (Listof c) (Listof d)))]
 [unzip5 (All (a b c d e f ...) (Listof (List a b c d e f ... f)) -> (Values (Listof a) (Listof b) (Listof c) (Listof d) (Listof e)))]
 [count (All (a b ...) (a b ... b -> Any) (Listof a) (Listof b) ... b -> Nonnegative-Integer)]
 [fold (All (a b c d)
            (case->
             (-> (-> a b b) b (Listof a) b)
             (-> (-> a b c c) c (Listof a) (Listof b) c)
             (-> (-> a b c d d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldl, doesn't support unlimited lists
 [fold-right (All (a b c d)
                  (case->
                   (-> (-> a b b) b (Listof a) b)
                   (-> (-> a b c c) c (Listof a) (Listof b) c)
                   (-> (-> a b c d d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldr, doesn't support unlimited lists
 [pair-fold (All (a b c d)
                 (case->
                  (-> (-> (Listof a) b b) b (Listof a) b)
                  (-> (-> (Listof a) (Listof b) c c) c (Listof a) (Listof b) c)
                  (-> (-> (Listof a) (Listof b) (Listof c) d d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldl, doesn't support unlimited lists
 [pair-fold-right (All (a b c d)
                       (case->
                        (-> (-> (Listof a) b b) b (Listof a) b)
                        (-> (-> (Listof a) (Listof b) c c) c (Listof a) (Listof b) c)
                        (-> (-> (Listof a) (Listof b) (Listof c) d d) d (Listof a) (Listof b) (Listof c) d)))] ; Like foldr, doesn't support unlimited lists
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
 [map (All (a b c ...) (a c ... c -> b) (Listof a) (Listof c) ... c -> (Listof b))]
 [for-each (All (a b ...) (a b ... b -> Any) (Listof a) (Listof b) ... b -> Void)]
 [append-map (All (a b c ...) (a c ... c -> (Listof b)) (Listof a) (Listof c) ... c -> (Listof b))]
 [map-in-order (All (a b c ...) (a c ... c -> b) (Listof a) (Listof c) ... c -> (Listof b))]
 [pair-for-each (All (a b ...) ((Listof a) (Listof b) ... b -> Any) (Listof a) (Listof b) ... b -> Void)]
 [filter-map (All (a b c ...) (a c ... c -> b) (Listof a) (Listof c) ... c -> (Listof b))]
 [filter (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [remove (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [take-while (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [drop-while (All (a) (a -> Any) (Listof a) -> (Listof a))]
 [span (All (a) (a -> Any) (Listof a) -> (Values (Listof a) (Listof a)))]
 [break (All (a) (a -> Any) (Listof a) -> (Values (Listof a) (Listof a)))]
 [any (All (a c b ...) (a b ... b -> c) (Listof a) (Listof b) ... b -> (Option c))]
 [every (All (a c b ...) (a b ... b -> c) (Listof a) (Listof b) ... b -> (Option c))]
 [list-index (All (a b ...) (a b ... b -> Any) (Listof a) (Listof b) ... b -> (Option Nonnegative-Integer))]
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
