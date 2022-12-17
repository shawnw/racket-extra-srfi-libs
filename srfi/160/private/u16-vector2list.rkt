#lang racket/base
;;; Autogenerated on 2022-11-02T18:09:29
;;; DO NOT EDIT

(require (only-in ffi/vector u16vector-length u16vector-ref))

(require (only-in
          racket/unsafe/ops
          unsafe-bytes-length
          unsafe-flvector-length
          unsafe-fxvector-length))

(provide u16vector->list)

(define (u16vector->list vec (start 0) (end (u16vector-length vec)))
  (let loop ((i (- end 1)) (list '()))
    (if (< i start) list (loop (- i 1) (cons (u16vector-ref vec i) list)))))
