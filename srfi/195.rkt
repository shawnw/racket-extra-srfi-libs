#lang racket/base

; SRFI-195 multi-valued boxes


(require racket/contract racket/match racket/struct racket/unsafe/ops
         (for-syntax racket/base syntax/parse)
         (only-in racket/base
                  [box? rkt:box?]
                  [box rkt:box]
                  [box-immutable rkt:box-immutable]
                  [unbox rkt:unbox]
                  [set-box! rkt:set-box!]))
(module+ test (require rackunit))

(provide
 mvbox
 (contract-out
  [box? predicate/c]
  [box (-> any/c ... box?)]
  [box-immutable (-> any/c ... box?)]
  [unbox (-> box? any)]
  [set-box! (-> (and/c box? (not/c immutable?)) any/c ... void?)]
  [box-arity (-> box? exact-nonnegative-integer?)]
  [unbox-value (-> box? exact-nonnegative-integer? any/c)]
  [set-box-value! (-> (and/c box? (not/c immutable?)) exact-nonnegative-integer? any/c void?)]
  ))

(define (mvbox-hash-proc mvb real-hash)
  (real-hash (mvbox-vals mvb)))

(struct mvbox (vals)
  #:sealed
  #:name %mvbox
  #:constructor-name make-mvbox
  #:methods gen:equal+hash
  [(define (equal-proc a b real-equal?)
     (real-equal? (mvbox-vals a) (mvbox-vals b)))
   (define hash-proc mvbox-hash-proc)
   (define hash2-proc mvbox-hash-proc)]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (mvb) 'box)
      (lambda (mvb) (vector->list (mvbox-vals mvb)))))])

(define (box? b)
  (or (rkt:box? b) (mvbox? b)))

(define (box . vals)
  (if (= (length vals) 1)
      (rkt:box (unsafe-car vals))
      (make-mvbox (list->vector vals))))

(define (box-immutable . vals)
  (if (= (length vals) 1)
      (rkt:box-immutable (unsafe-car vals))
      (make-mvbox (unsafe-vector*->immutable-vector! (list->vector vals)))))


(define (unbox b)
  (if (rkt:box? b)
      (unsafe-unbox b)
      (vector->values (mvbox-vals b))))

(define (set-box! b . vals)
  (cond
    ((rkt:box? b)
     (cond
       ((immutable? b)
        (raise-argument-error 'set-box! "(and/c box? (not/c immutable?))" b))
       ((= (length vals) 1)
        (unsafe-set-box! b (unsafe-car vals)))
       (else
        (raise-arguments-error 'set-box! "Box and arguments must have the same arity"
                               "box arity" 1
                               "argument arity" (length vals)))))
    ((immutable? (mvbox-vals b))
     (raise-argument-error 'set-box! "(and/c box? (not/c immutable?))" b))
    (else
     (let ([v (mvbox-vals b)])
       (unless (= (unsafe-vector-length v) (length vals))
         (raise-arguments-error 'set-box! "Box and arguments must have the same arity"
                                 "box arity" (unsafe-vector-length v)
                                 "argument arity" (length vals)))
       (for ([elem (in-list vals)]
             [i (in-naturals)])
         (unsafe-vector-set! v i elem))))))

(define (box-arity b)
  (if (rkt:box? b)
      1
      (unsafe-vector-length (mvbox-vals b))))

(define (unbox-value b i)
  (if (rkt:box? b)
      (if (= i 0)
          (unsafe-unbox b)
          (raise-range-error 'unbox-value "box" "" i b 0 0))
      (if (and (>= i 0) (< i (unsafe-vector-length (mvbox-vals b))))
          (unsafe-vector-ref (mvbox-vals b) i)
          (raise-range-error 'unbox-value "box" "" i b 0 (- (unsafe-vector-length (mvbox-vals b)))))))

(define (set-box-value! b i v)
  (cond
    ((rkt:box? b)
     (cond
       ((immutable? b)
        (raise-argument-error 'set-box-value! "(and/c box? (not/c immutable?))" b))
       ((= i 0)
        (unsafe-set-box! b v))
       (else
        (raise-range-error 'set-box-value! "box" "" i b 0 0))))
    ((immutable? (mvbox-vals b))
     (raise-argument-error 'set-box-value! "(and/c box? (not/c immutable?))" b))
    ((and (> i 0) (< i (unsafe-vector-length (mvbox-vals b))))
     (unsafe-vector-set! (mvbox-vals b) i v))
    (else
     (raise-range-error 'set-box-value! "box" "" i b 0 (- (unsafe-vector-length (mvbox-vals b)) 1)))))

(define-match-expander mvbox
  (lambda (stx)
    (syntax-parse stx
      [(_ pat:expr) ; Single-value boxes should only be normal Racket boxes but let's be safe
       #'(or (box pat)
             (? mvbox? (app mvbox-vals (vector pat))))]
      [(_ pat:expr ...) ; Multi-value boxes
       #'(? mvbox? (app mvbox-vals (vector pat ...)))]))
  (lambda (stx)
    (syntax-parse stx
      [(_ pat:expr ...) #'(box pat ...)])))

(module+ test
  (define b1 (box #\a))
  (define b3 (box 1 2 3))

  (define-syntax-rule (returned-values->list expr)
    (call-with-values
     (lambda () expr)
     list))

  (test-pred "box? 1" box? b1)
  (test-pred "box? 3" box? b3)

  (test-true "equal? box 3" (equal? (box 1 2) (box 1 2)))
  (test-false "not equal? box 3" (equal? (box 1 2) (box 1 3)))

  (test-equal? "box-arity 1" (box-arity b1) 1)
  (test-equal? "box-arity 3" (box-arity b3) 3)

  (test-equal? "unbox 1" (unbox b1) #\a)
  (test-equal? "unbox 3" (returned-values->list (unbox b3)) '(1 2 3))
  (test-equal? "unbox-value 1" (unbox-value b1 0) #\a)
  (test-equal? "unbox-value 3.1" (unbox-value b3 0) 1)
  (test-equal? "unbox-value 3.2" (unbox-value b3 1) 2)
  (test-equal? "unbox-value 3.3" (unbox-value b3 2) 3)
  (test-exn "unbox-value out of range 1" exn:fail:contract? (lambda () (unbox-value b1 2)))
  (test-exn "unbox-value out of range 3" exn:fail:contract? (lambda () (unbox-value b3 3)))

  (test-exn "set-box! wrong arity 1" exn:fail:contract? (lambda () (set-box! b1 #t #f)))
  (test-exn "set-box! wrong arity 3" exn:fail:contract? (lambda () (set-box! b3 #t #f)))

  (set-box! b1 #t)
  (test-equal? "set-box! 1" (unbox-value b1 0) #t)
  (set-box! b3 'a 'b 'c)
  (test-equal? "set-box! 3" (returned-values->list (unbox b3)) '(a b c))

  (test-exn "set-box-value! out of range 1" exn:fail:contract? (lambda () (set-box-value! b1 2 #f)))
  (test-exn "set-box-value! out of range 3" exn:fail:contract? (lambda () (set-box-value! b3 5 #f)))
  (set-box-value! b1 0 #\a)
  (test-equal? "set-box-value! 1" (unbox-value b1 0) #\a)
  (set-box-value! b3 2 #t)
  (test-equal? "set-box-value! 3" (returned-values->list (unbox b3)) '(a b #t))

  (test-equal? "match mvbox 1"
               (match b1 [(mvbox a b c) (list a b c)] [(mvbox a) (list a)]) '(#\a))
  (test-equal? "match mvbox 3"
               (match b3 [(mvbox a) (list a)] [(mvbox a b c) (list a b c)]) '(a b #t))
  )
