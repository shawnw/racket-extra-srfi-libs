#lang racket/base

;;; SRFI-174 for Racket.

(require racket/contract)
(module+ test (require "private/testwrappers.rkt"))

(provide
 (struct-out timespec)
 (contract-out
  [timespec=? (-> timespec? timespec? boolean?)]
  [timespec<? (-> timespec? timespec? boolean?)]
  [timespec-hash (-> timespec? exact-integer?)]
  [timespec->inexact (-> timespec? real?)]
  [inexact->timespec (-> real? timespec?)]
  ))

(struct/contract timespec ([seconds exact-integer?] [nanoseconds (integer-in 0 (- #e1e9 1))])
                 #:transparent)

(define (timespec=? a b)
  (and (= (timespec-seconds a) (timespec-seconds b))
       (= (timespec-nanoseconds a) (timespec-nanoseconds b))))

;;; From the reference implementation
(define (timespec<? a b)
  (let ((asecs (timespec-seconds a))
        (bsecs (timespec-seconds b))
        (ansecs (timespec-nanoseconds a))
        (bnsecs (timespec-nanoseconds b)))
    (cond
      ((< asecs bsecs) #t)
      ((> asecs bsecs) #f)
      ((negative? asecs) (> ansecs bnsecs))
      (else (< ansecs bnsecs)))))

(define (timespec-hash a) (equal-hash-code a))

;;; From the reference implementation
(define (timespec->inexact timespec)
  (let ((secs (timespec-seconds timespec))
        (nsecs (timespec-nanoseconds timespec)))
    (if (negative? secs)
      (- secs (/ nsecs #i1e9))
      (+ secs (/ nsecs #i1e9)))))

;;; From the reference implementation
(define (inexact->timespec inex)
  (let* ((quo (inexact->exact (truncate inex)))
         (rem (inexact->exact (- inex quo))))
    (timespec quo (inexact->exact (truncate (* (abs rem) #e1e9))))))

(module+ test
  (define ts1 (timespec 1 2))
  (define ts2 (timespec 1 2))
  (define ts3 (timespec 1 3))
  (define ts4 (timespec 2 2))
  (define ts-neg1 (timespec -1 2))
  (define ts-neg2 (timespec -1 5))
  (define ts-neg3 (timespec -2 0))

  (test-assert "timespec?" (timespec? ts1))
  (test-assert "not timespec?" (not (timespec? #f)))
  (test "seconds" 1 (timespec-seconds ts1))
  (test "nanos" 2 (timespec-nanoseconds ts1))
  (test-assert "equal" (timespec=? ts1 ts2))
  (test-assert "less nanos" (timespec<? ts1 ts3))
  (test-assert "less seconds" (timespec<? ts1 ts4))
  (test-assert "less -nanos" (timespec<? ts-neg2 ts-neg1))
  (test-assert "less -seconds" (timespec<? ts-neg3 ts-neg2))
  ;(test-assert "positive hash" (positive? (timespec-hash ts-neg1)))
  (test "to inexact" #i1.1 (timespec->inexact (timespec 1 #e1e8)))
  (let ((t (timespec 1 1))
        (u (inexact->timespec #i1.000000001)))
    (test "from inexact" t u))
  )