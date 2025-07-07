#lang racket/base

;;; SRFI-259 Tagged procedures with type safety


(require (for-syntax racket/base racket/syntax syntax/parse))
(module+ test (require rackunit))
(provide define-procedure-tag)


(define (make-struct-type* #:name name #:init-field-count init-field-cnt #:super-type [super-type #f] #:auto-field-count [auto-field-cnt 0]
                           #:auto-v [auto-v #f] #:props [props '()] #:inspector [inspector (current-inspector)] #:proc-spec [proc-spec #f]
                           #:immutables [immutables '()] #:guard [guard #f] #:constructor-name [constructor-name #f])
  (make-struct-type name super-type init-field-cnt auto-field-cnt auto-v props inspector proc-spec immutables guard constructor-name))

(struct base-tagged-procedure ())

(define (check-procedure-type pred? pred-name)
  (define (check obj)
    (cond
      [(pred? obj) #t]
      [(base-tagged-procedure? obj)
       (check (procedure-extract-target obj))]
      [else #f]))
  (procedure-rename check pred-name))

(define (find-tag-for-type accessor name pred? pred-name)
  (define (find the-obj)
    (let loop ([obj the-obj])
      (cond
        [(pred? obj) (accessor obj 0)]
        [(base-tagged-procedure? obj)
         (loop (procedure-extract-target obj))]
        [else
         (raise-argument-error name (symbol->string pred-name) the-obj)])))
  (procedure-rename find name))

(define-syntax (define-procedure-tag stx)
  (syntax-parse stx
    [(_ protocol-name:id constructor-name:id predicate-name:id accessor-name:id)
     #'(define-values (constructor-name predicate-name accessor-name)
         (letrec-values ([(typedesc constructor predicate? accessor mutator)
                          (make-struct-type* #:name 'protocol-name #:super-type struct:base-tagged-procedure
                                             #:init-field-count 2 #:immutables '(0 1) #:proc-spec 1
                                             #:constructor-name 'constructor-name
                                             #:guard (lambda (tag proc name)
                                                       (if (procedure? proc)
                                                           (values tag proc)
                                                           (raise-argument-error 'constructor-name "procedure?" proc)))
                                             #:props (list (cons prop:sealed #t)
                                                           (cons prop:object-name
                                                                 (lambda (obj) (string->symbol (format "~A/~A" 'protocol-name (or (object-name (accessor obj 1)) 'unknown)))))
                                                           (cons prop:custom-write
                                                                 (lambda (obj out mode) (fprintf out "#<tagged-procedure:~A tag: ~S>" (object-name obj) (accessor obj 0))))))])
           (values constructor (check-procedure-type predicate? 'predicate-name) (find-tag-for-type accessor 'accessor-name predicate? 'predicate-name))))]
    [(_ constructor-name:id predicate-name:id accessor-name:id)
     #`(define-procedure-tag #,(generate-temporary 'tpp) constructor-name predicate-name accessor-name)]))

(module+ test
  (define-procedure-tag make-a-tagged a-tagged? a-tag)
  (define (greet whom) (list 'hello whom))
  (define greet-a (make-a-tagged 12 greet))

  (check-true (a-tagged? greet-a))
  (check-false (a-tagged? greet))
  (check-false (a-tagged? 'a))

  (check-eqv? (a-tag greet-a) 12)
  (check-exn exn:fail? (lambda () (a-tag greet)))
  (check-exn exn:fail? (lambda () (a-tag 'a)))

  (check-equal? (greet 'world) '(hello world))
  (check-equal? (greet-a 'world) '(hello world))

  (define greet-a* (make-a-tagged 12 greet))
  (check-false (eqv? greet-a greet-a*))
  (check-false (eq? greet-a greet-a*))
  (check-equal? (greet-a* 'world) '(hello world))

  (define-procedure-tag make-b-tagged b-tagged? b-tag)
  (define greet-b (make-b-tagged 34 greet))

  (check-false (a-tagged? greet-b))
  (check-true (b-tagged? greet-b))

  (check-eqv? (b-tag greet-b) 34)


  (define greet-ab (make-b-tagged 56 greet-a))

  (check-true (a-tagged? greet-ab))
  (check-true (b-tagged? greet-ab))
  (check-eqv? (a-tag greet-ab) 12)
  (check-eqv? (b-tag greet-ab) 56)

  (define greet-ab* (make-a-tagged 1234 greet-ab))
  (check-true (a-tagged? greet-ab*))
  (check-true (b-tagged? greet-ab*))
  (check-eqv? (a-tag greet-ab*) 1234)
  (check-eqv? (b-tag greet-ab*) 56)
  (check-eqv? (a-tag greet-ab) 12)

  (define (make-procedure-tag)
  (define-procedure-tag make is-a? ref)
  (values make is-a? ref))

  (define-values (make-c-tagged c-tagged? c-tag) (make-procedure-tag))
  (define-values (make-d-tagged d-tagged? d-tag) (make-procedure-tag))
  (define greet-c (make-c-tagged 'alpha greet))
  (define greet-d (make-d-tagged 'beta greet))

  (check-true (c-tagged? greet-c))
  (check-false (d-tagged? greet-c))
  (check-false (c-tagged? greet-d))
  (check-true (d-tagged? greet-d))

  (check-eq? (c-tag greet-c) 'alpha)
  (check-eq? (d-tag greet-d) 'beta)
  )
