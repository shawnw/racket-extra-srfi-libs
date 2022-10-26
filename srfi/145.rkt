#lang racket/base
;;; SRFI-145 Assumptions

(require racket/format
         (for-syntax racket/base syntax/parse racket/syntax-srcloc))
(module+ test (require rackunit))
(provide assume
         (struct-out exn:fail:contract:assume))

(struct exn:fail:contract:assume exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:assume
  #:transparent)

(define-syntax (assume stx)
  (syntax-parse stx
    ((_ expression:expr)
     (let ([srcloc (syntax-srcloc stx)])
       (if srcloc
           (with-syntax ([srcloc (srcloc->string srcloc)])
             #`(or expression
                   (raise (make-exn:fail:contract:assume (format "~A: assume: invalid assumption~%  assumption: ~V"
                                                                 srcloc
                                                                 (quote expression))
                                                         (current-continuation-marks)))))
           #`(or expression
                 (raise (make-exn:fail:contract:assume (format "assume: invalid assumption~%  assumption: ~V"
                                                               (quote expression))
                                                       (current-continuation-marks)))))))
    ((_ expression:expr message:expr ...+)
     (let ([srcloc (syntax-srcloc stx)])
       (if srcloc
           (with-syntax ([srcloc (srcloc->string srcloc)])
             #'(or expression
                   (raise (make-exn:fail:contract:assume (format "~A: assume: invalid assumption;~% ~A~%  assumption: ~V"
                                                                 srcloc
                                                                 (~a message ... #:separator " ")
                                                                 (quote expression))
                                                         (current-continuation-marks)))))
           #'(or expression
                 (raise (make-exn:fail:contract:assume (format "assume: invalid assumption;~% ~A~%  assumption: ~V"
                                                               (~a message ... #:separator " ")
                                                               (quote expression))
                                                       (current-continuation-marks)))))))))

(module+ test
  (test-not-false "successful assume" (assume (= 1 1) "this shouldn't fail"))
  (test-exn "failed assume" exn:fail:contract:assume? (lambda () (assume (= 1 0) "this should fail"))))
