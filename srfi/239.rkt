#lang racket/base

;;; SRFI 239: Destructuring Lists
;; Custom implementation; standard test cases adapated to rackunit

(require racket/unsafe/ops (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(provide list-case mlist-case exn:fail:list-case? _)

(struct exn:fail:list-case exn:fail ()
  #:extra-constructor-name make-exn:fail:list-case
  #:transparent)

(define-syntax (list-case stx)
  (syntax-parse stx
    [(list-case list-expression:expr
             (~alt (~optional [() null-body:expr ...+]
                              #:name "null case"
                              #:defaults ([(null-body 1) (list #'(raise (make-exn:fail:list-case "list-case: no matching null case" (current-continuation-marks))))]))
                   (~optional (~and [(kar:id . kdr:id) pair-body:expr ...+] ~!
                                    (~fail #:when (and (not (free-identifier=? #'kar #'_)) (not (free-identifier=? #'kdr #'_)) (check-duplicate-identifier (list #'kar #'kdr)))
                                           "duplicate identifiers in cons case"))
                              #:name "cons case"
                              #:defaults ([kar #'_] [kdr #'_] [(pair-body 1) (list #'(raise (make-exn:fail:list-case "list-case: no matching cons case" (current-continuation-marks))))]))
                   (~optional [var:id alt-body:expr ...+]
                              #:name "atom case"
                              #:defaults ([var #'_] [(alt-body 1) (list #'(raise (make-exn:fail:list-case "list-case: no matching atom case" (current-continuation-marks))))]))) ...)
  #`(let ([l-e list-expression])
      (if (pair? l-e)
          #,(cond
              [(and (free-identifier=? #'kar #'_) (free-identifier=? #'kdr #'_))
               #'(let () pair-body ...)]
              [(free-identifier=? #'kar #'_)
               #'(let ([kdr (unsafe-cdr l-e)]) pair-body ...)]
              [(free-identifier=? #'kdr #'_)
               #'(let ([kar (unsafe-car l-e)]) pair-body ...)]
              [else
               #'(let ([kar (unsafe-car l-e)] [kdr (unsafe-cdr l-e)]) pair-body ...)])
          (if (null? l-e)
              (let () null-body ...)
              #,(if (free-identifier=? #'var #'_) #'(let () alt-body ...) #'(let ([var l-e]) alt-body ...)))))]))

(define-syntax (mlist-case stx)
  (syntax-parse stx
    [(list-case list-expression:expr
             (~alt (~optional [() null-body:expr ...+]
                              #:name "null case"
                              #:defaults ([(null-body 1) (list #'(raise (make-exn:fail:list-case "mlist-case: no matching null case" (current-continuation-marks))))]))
                   (~optional (~and [(kar:id . kdr:id) pair-body:expr ...+] ~!
                                    (~fail #:when (and (not (free-identifier=? #'kar #'_)) (not (free-identifier=? #'kdr #'_)) (check-duplicate-identifier (list #'kar #'kdr)))
                                           "duplicate identifiers in mcons case"))
                              #:name "mcons case"
                              #:defaults ([kar #'_] [kdr #'_] [(pair-body 1) (list #'(raise (make-exn:fail:list-case "mlist-case: no matching mcons case" (current-continuation-marks))))]))
                   (~optional [var:id alt-body:expr ...+]
                              #:name "atom case"
                              #:defaults ([var #'_] [(alt-body 1) (list #'(raise (make-exn:fail:list-case "mlist-case: no matching atom case" (current-continuation-marks))))]))) ...)
  #`(let ([l-e list-expression])
      (if (mpair? l-e)
          #,(cond
              [(and (free-identifier=? #'kar #'_) (free-identifier=? #'kdr #'_))
               #'(let () pair-body ...)]
              [(free-identifier=? #'kar #'_)
               #'(let ([kdr (unsafe-mcdr l-e)]) pair-body ...)]
              [(free-identifier=? #'kdr #'_)
               #'(let ([kar (unsafe-mcar l-e)]) pair-body ...)]
              [else
               #'(let ([kar (unsafe-mcar l-e)] [kdr (unsafe-mcdr l-e)]) pair-body ...)])
          (if (null? l-e)
              (let () null-body ...)
              #,(if (free-identifier=? #'var #'_) #'(let () alt-body ...) #'(let ([var l-e]) alt-body ...)))))]))


(module+ test
  (define type-of
    (lambda (x)
      (list-case x
        [(_ . _) 'pair]
        [() 'null]
        [_ 'atom])))

  (check-eq? (type-of '(a . b)) 'pair )
  (check-eq? (type-of '()) 'null)
  (check-eq? (type-of 'x) 'atom)

  (define fold
    (lambda (proc seed ls)
      (let f ([acc seed] [ls ls])
        (list-case ls
          [(h . t) (f (proc h acc) t)]
          [() acc]
          [_ (error 'fold "not a list: ~S" ls)]))))

  (check-equal? (fold cons 0 '(1 2 3)) '(3 2 1 . 0))
  (check-exn exn:fail? (lambda () (fold cons 0 '(1 2 . 3))))
  (check-true (list-case '(1 . 2)
                [_ #f]
                [(_ . _) #t]))

  (check-exn exn:fail:list-case?
             (lambda ()
               (list-case 0
                 [(_ . _) #f]
                 [() #f])))
)
