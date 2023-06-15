#lang racket/base

;;; Adapt flvectors to SRFI-160.

(require racket/contract
         racket/unsafe/ops
         (only-in racket/fixnum
                  fxvector? fxvector make-fxvector fxvector-ref fxvector-set! fxvector-length for/fxvector))

(provide
 (except-out (all-from-out racket/fixnum) for/fxvector)
 (contract-out
  [list->fxvector (-> (listof fixnum?) fxvector?)]))

(module+ unsafe
  (provide
   fxvector? fxvector make-fxvector
   (rename-out
    [unsafe-fxvector-ref fxvector-ref]
    [unsafe-fxvector-set! fxvector-set!]
    [unsafe-fxvector-length fxvector-length])
   list->fxvector))


(define (list->fxvector lst)
  (for/fxvector #:length (length lst) ([elem (in-list lst)]) elem))
