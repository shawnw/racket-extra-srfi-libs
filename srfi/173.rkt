#lang racket/base
;;; SRFI-173 Hooks

(require racket/contract)
(module+ test (require rackunit))

(provide
 (contract-out
  [hook? predicate/c]
  [make-hook (-> exact-nonnegative-integer? hook?)]
  [list->hook (-> exact-nonnegative-integer? (listof procedure?) hook?)]
  [list->hook! (-> hook? (listof procedure?) void?)]
  [hook-add! (-> hook? procedure? void?)]
  [hook-delete! (-> hook? procedure? void?)]
  [hook-reset! (-> hook? void?)]
  [hook->list (-> hook? (listof procedure?))]
  [hook-run (-> hook? any/c ... void?)]))


(struct hook (arity [funcs #:mutable])
  #:transparent
  #:property prop:procedure
  (lambda (h . args) (%hook-run h args)))

(define (make-hook arity)
  (hook arity '()))

(define (check-arity name arity f)
  (unless (procedure-arity-includes? f arity)
    (raise (make-exn:fail:contract:arity
            (format "~S: procedure given takes the wrong number of arguments~%~%  procedure: ~A~%  expected: ~S~%  actual: ~A" name f arity (procedure-arity f))
            (current-continuation-marks)))))

(define (list->hook arity funcs)
  (for ([f (in-list funcs)])
    (check-arity 'list->hook arity f))
  (hook arity funcs))

(define (list->hook! h funcs)
  (for ([f (in-list funcs)])
    (check-arity 'list->hook! (hook-arity h) f))
  (set-hook-funcs! h funcs))

(define (hook-add! h f)
  (check-arity 'hook-add! (hook-arity h) f)
  (set-hook-funcs! h (cons f (hook-funcs h))))

(define (hook-delete! h f)
  (set-hook-funcs! h (remq f (hook-funcs h))))

(define (hook-reset! h)
  (set-hook-funcs! h '()))

(define (hook->list h)
  (hook-funcs h))

(define (%hook-run h args)
  (let ([len (length args)])
    (cond
      ((not (= (hook-arity h) len))
       (apply raise-arity-error 'hook-run (hook-arity h) args))
      ((= len 0)
       (for-each (lambda (f) (f)) (hook-funcs h)))
      (else
       (for-each (lambda (f) (apply f args)) (hook-funcs h))))))

(define (hook-run h . args) (%hook-run h args))

(module+ test
  (define-syntax-rule (test expected test-expr)
    (check-equal? test-expr expected))

  ;; test hook->list and make-hook
  (test 0 (length (hook->list (make-hook 0))))

  ;; test hook-add!
  (test 1 (let ((hook (make-hook 1))
                (counter 0))
            (hook-add! hook (lambda (x) (set! counter (+ counter x))))
            (length (hook->list hook))))

  (test-exn "Add callback with wrong arity"
            exn:fail:contract:arity?
            (lambda ()
              (let ([hook (make-hook 1)])
                (hook-add! hook (lambda (a b) (+ a b))))))

  ;; test hook-delete!
  (test 0 (let ((hook (make-hook 0))
                (counter 0))
            (let ((increment (lambda () (set! counter (+ counter 1)))))
              (hook-add! hook increment)
              (hook-delete! hook increment)
              (length (hook->list hook)))))

  ;; test hook-reset!
  (test 0 (let ((hook (make-hook 0))
                (counter 0))
            (let ((increment (lambda () (set! counter (+ counter 1))))
                  (decrement (lambda () (set! counter (- counter 1)))))
              (hook-add! hook increment)
              (hook-reset! hook)
              (length (hook->list hook)))))

  ;; test hook-run
  (test 0 (let ((hook (make-hook 0))
                (counter 0))
            (let ((increment (lambda () (set! counter (+ counter 1))))
                  (decrement (lambda () (set! counter (- counter 1)))))
              (hook-add! hook increment)
              (hook-add! hook decrement)
              (hook-run hook)
              counter)))

  (test-exn "Run hooks with wrong arity of arguments"
            exn:fail:contract:arity?
            (lambda ()
              (let ([hook (make-hook 0)])
                (hook-add! hook (lambda () #t))
                (hook-run hook 1))))

  (test-eqv? "Invoke hook object directly"
             (let ((hook (make-hook 0))
                   (counter 0))
               (let ((increment (lambda () (set! counter (+ counter 1))))
                     (decrement (lambda () (set! counter (- counter 1)))))
                 (hook-add! hook increment)
                 (hook)
                 counter))
             1)

  ;; test list->hook
  (test 0 (let* ((counter 0)
                 (increment (lambda () (set! counter (+ counter 1))))
                 (decrement (lambda () (set! counter (- counter 1)))))
            (let ((hook (list->hook 0 (list increment decrement))))
              (hook-add! hook increment)
              (hook-add! hook decrement)
              (hook-run hook)
              counter)))

  ;; test list->hook!
  (test 0 (let* ((counter 0)
                 (increment (lambda () (set! counter (+ counter 1))))
                 (decrement (lambda () (set! counter (- counter 1))))
                 (hook (make-hook 0)))
            (list->hook! hook (list increment decrement))
            (hook-add! hook increment)
            (hook-add! hook decrement)
            (hook-run hook)
            counter)))
