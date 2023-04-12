#lang racket/base

;;; SRFI-210 Procedures and Syntax for Multiple Values
;;; Mix of original (Mostly syntax forms) and reference code (Mostly functions)

(require racket/contract racket/list syntax/parse/define
         (only-in racket/base [vector->values vector-values])
         (rename-in "195.rkt" [unbox box-values])
         (for-syntax racket/base syntax/parse syntax/parse/lib/function-header))
(module+ test (require racket/format rackunit))

(provide
 ; Syntax forms
 apply/mv call/mv list/mv vector/mv box/mv value/mv coarity with-values
 ; Aleady in Racket: set!-values
 ; Renamed procedures from other modules
 vector-values box-values identity

 (contract-out
  [value (-> exact-nonnegative-integer? any/c ... any/c)]
  [list-values (-> list? any)]
  [compose-left (-> procedure? ... procedure?)]
  [compose-right (-> procedure? ... procedure?)]  
  [map-values (-> procedure? procedure?)]
  [bind/list (-> list? procedure? ... any)]
  [bind/box (-> box? procedure? ... any)]
  [bind (-> any/c procedure? ... any)]

  [bind/vector (-> vector? procedure? ... any)]
  ))

(define-syntax (apply/mv stx)
  (syntax-parse stx
    [(_ operator:expr operandN:expr ... producer:expr (~and (~seq (~seq k:keyword v:expr) ...) (~seq k+v ...)))
     (let ([operandN-names (generate-temporaries #'(operandN ...))])
       #`(let (#,@(map (lambda (id expr)
                         #`(#,id #,expr))
                       operandN-names
                       (syntax->list #'(operandN ...))))
           (call-with-values
            (lambda () producer)
            (lambda prod-values (apply operator #,@operandN-names prod-values k+v ...)))))]))

(define-syntax (call/mv stx)
  (syntax-parse stx
    [(_ consumer:expr producer:expr ... (~and (~seq (~seq k:keyword v:expr) ...) (~seq k+v ...)))
     (let ([producer-names (generate-temporaries #'(producer ...))])
       #`(let (#,@(map (lambda (id expr)
                         #`(#,id (call-with-values (lambda () #,expr) list)))
                       producer-names
                       (syntax->list #'(producer ...))))
           (apply consumer (append #,@producer-names) k+v ...)))]))

(define-syntax-parse-rule (list/mv element:expr ... producer:expr)
  (apply/mv list element ... producer))

(define-syntax-parse-rule (vector/mv element:expr ... producer:expr)
  (apply/mv vector element ... producer))

(define-syntax-parse-rule (box/mv element:expr ... producer:expr)
  (apply/mv box element ... producer))

(define-syntax-parse-rule (value/mv index:expr operand:expr ... producer:expr)
  (let ([i index])
    (unless (exact-nonnegative-integer? i)
      (raise-argument-error 'value/mv "exact-nonnegative-integer?" (quote index)))
    (apply/mv value index operand ... producer)))

(define-syntax-parse-rule (coarity producer:expr)
  (call-with-values
   (lambda () producer)
   (lambda vals (length vals))))

(define-syntax-parse-rule (with-values producer:expr consumer:expr (~and (~seq (~seq k:keyword v:expr) ...) (~seq k+v ...)))
  (call-with-values
   (lambda () producer)
   (lambda args (apply consumer args k+v ...))))

(define-syntax-parse-rule (case-receive producer:expr (fmls:formals body:expr ...+) ...+)
  (call-with-values
   (lambda () producer)
   (case-lambda (fmls body ...) ...)))

(define-syntax-parse-rule (bind/mv producer:expr transducer:expr ...)
  (bind/list (list/mv producer) transducer ...))

(define (list-values lst)
  (apply values lst))

(define (value i . objs)
  (list-ref objs i))

(define identity (procedure-rename values 'identity))

(define compose-left
  (case-lambda
    (() identity)
    ((transducer . transducers)
     (let f ((transducer transducer) (transducers transducers))
       (if (null? transducers)
           transducer
           (let ((composition (f (car transducers) (cdr transducers))))
             (lambda args
               (apply/mv composition (apply transducer args)))))))))

(define compose-right
  (case-lambda
    (() identity)
    ((transducer . transducers)
     (let f ((transducer transducer) (transducers transducers))
       (if (null? transducers)
           transducer
           (let ((composition (f (car transducers) (cdr transducers))))
             (lambda args
               (apply/mv transducer (apply composition args)))))))))

(define (map-values proc)
  (lambda args (list-values (map proc args))))

(define bind/list
  (case-lambda
    [(lst) (list-values lst)]
    [(lst transducer) (apply transducer lst)]
    [(lst transducer . transducers)
     (apply bind/list (list/mv (apply transducer lst)) transducers)]))

(define (vector-apply func vec)
  (apply func (vector->list vec)))

(define bind/vector
  (case-lambda
    [(vec) (vector-values vec)]
    [(vec transducer) (vector-apply transducer vec)]
    [(vec transducer . transducers)
     (apply bind/vector (vector/mv (vector-apply transducer vec)) transducers)]))

(define (bind/box bx . transducers)
  (apply bind/list (list/mv (box-values bx)) transducers))

(define (bind obj . transducers)
      (apply bind/list (list obj) transducers))

(module+ test

  (define-syntax-rule (test-values-equal? name expr val ...)
    (test-equal? name (call-with-values
                       (lambda () expr)
                       list)
                 (list val ...)))
  
  (test-equal? "apply/mv simple" (apply/mv string #\a (values #\b #\c)) "abc")
  (test-equal? "apply/mv keywords" (apply/mv ~a #\a (values #\b #\c) #:width 20 #:separator ",") "a,b,c               ")
  (test-equal? "call/mv simple" (call/mv string (values #\a #\b) (values #\c #\d)) "abcd")
  (test-equal? "call/mv keywords" (call/mv ~a (values #\a #\b) (values #\c #\d) #:separator ",") "a,b,c,d")
  (test-equal? "list/mv" (list/mv 'a (values 'b 'c)) '(a b c))
  (test-equal? "veotor/mv" (vector/mv 'a (values 'b 'c)) #(a b c))
  (test-equal? "box/mv" (box/mv 'a (values 'b 'c)) (box 'a 'b 'c))
  (test-equal? "value/mv" (value/mv 1 'a (values 'b 'c)) 'b)
  (test-equal? "coarity" (coarity (values 'a 'b 'c)) 3)
  (test-equal? "with-values simple" (with-values (values 4 5) (lambda (a b) b)) 5)
  (test-equal? "with-values keywords" (with-values (values 4 5) ~a #:separator ",") "4,5")
  (test-equal? "case-receive"
              (case-receive (values 'a 'b)
                            ((x) #f)
                            ((x . y) (list x y)))
              '(a (b)))
  (test-values-equal? "bind/mv"  (bind/mv (values 1 2 3)
                                          (map-values (lambda (x) (* 2 x)))
                                          (map-values (lambda (x) (+ 1 x))))
                      3 5 7)
  (test-values-equal? "list-values" (list-values '(a b c)) 'a 'b 'c)
  (test-values-equal? "vector-values" (vector-values #(a b c)) 'a 'b 'c)
  (test-values-equal? "box-values" (box-values (box 'a 'b 'c)) 'a 'b 'c)
  (test-equal? "value" (value 1 'a 'b 'c) 'b)
  (test-values-equal? "compose-left"
                      (let ((f (map-values (lambda (x) (* 2 x))))
                            (g (map-values (lambda (x) (+ x 1)))))
                        ((compose-left f g) 1 2 3))
                      3 5 7)
  (test-values-equal? "compose-right"
                      (let ((f (map-values (lambda (x) (* 2 x))))
                            (g (map-values (lambda (x) (+ x 1)))))
                        ((compose-right f g) 1 2 3))
                      4 6 8)
  (test-values-equal? "map-values" ((map-values odd?) 1 2 3) #t #f #t)
  (test-values-equal? "bind/list" (bind/list '(1 2 3) (map-values (lambda (x) (* 3 x)))) 3 6 9)
  (test-values-equal? "bind/list multiple transducers" (bind/list '(1 2 3)
                                             (map-values (lambda (x) (* 3 x)))
                                             (map-values (lambda (x) (/ x 2))))
                      3/2 3 9/2)

  (test-values-equal? "bind/vector" (bind/vector #(1 2 3) (map-values (lambda (x) (* 3 x)))) 3 6 9)
  (test-values-equal? "bind/vector multiple transducers" (bind/vector #(1 2 3)
                                                 (map-values (lambda (x) (* 3 x)))
                                                 (map-values (lambda (x) (/ x 2))))
                      3/2 3 9/2)

  (test-values-equal? "bind/box" (bind/box (box 1 2 3) (map-values (lambda (x) (* 3 x)))) 3 6 9)

  )