#lang racket/base
;;; Autogenerated on 2024-10-18T15:19:08
;;; DO NOT EDIT

(require (only-in "complex.rkt" c64vector-length c64vector-ref))

(require (only-in
          racket/unsafe/ops
          unsafe-bytes-length
          unsafe-flvector-length
          unsafe-fxvector-length))

(provide c64vector->list)

(define (c64vector->list vec (start 0) (end (c64vector-length vec)))
  (let loop ((i (- end 1)) (list '()))
    (if (< i start) list (loop (- i 1) (cons (c64vector-ref vec i) list)))))

