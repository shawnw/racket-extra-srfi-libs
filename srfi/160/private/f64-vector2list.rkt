#lang racket/base
;;; Autogenerated on 2024-10-18T15:19:08
;;; DO NOT EDIT

(require (only-in ffi/vector f64vector-length f64vector-ref))

(require (only-in
          racket/unsafe/ops
          unsafe-bytes-length
          unsafe-flvector-length
          unsafe-fxvector-length))

(provide f64vector->list)

(define (f64vector->list vec (start 0) (end (f64vector-length vec)))
  (let loop ((i (- end 1)) (list '()))
    (if (< i start) list (loop (- i 1) (cons (f64vector-ref vec i) list)))))

