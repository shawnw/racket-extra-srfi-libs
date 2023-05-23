#lang typed/racket/base
;;; SRFI-223 specialization for byte strings

(require "../223.rkt")
(module+ test (require typed/rackunit))

(provide bytes-bisect-left bytes-bisect-right)

(define-values (bytes-bisect-left bytes-bisect-right)
  ((inst bisection Bytes Byte) bytes-ref (lambda ([bs : Bytes]) (values 0 (bytes-length bs)))))

(module+ test
  (define bs #"abcd")
  (test-equal? "bytes-bisect-left" (bytes-bisect-left bs (assert (char->integer #\c) byte?) <) 2)
  (test-equal? "bytes-bisect-right" (bytes-bisect-right bs (assert (char->integer #\c) byte?) <) 3))
