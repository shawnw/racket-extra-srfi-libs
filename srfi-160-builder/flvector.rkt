#lang racket/base

;;; Adapt flvectors to SRFI-160.

(require racket/contract
         racket/unsafe/ops
         (only-in racket/flonum
                  flvector? flvector make-flvector flvector-ref flvector-set! flvector-length for/flvector)
         (only-in ffi/vector
                  f32vector? make-f32vector f32vector-ref f32vector-set! f32vector-length
                  f64vector? make-f64vector f64vector-ref f64vector-set! f64vector-length))
(provide
 (except-out (all-from-out racket/flonum) for/flvector)
 (contract-out
  [list->flvector (-> (listof flonum?) flvector?)]
  [flvector->f32vector (->* (flvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) f32vector?)]
  [flvector->f64vector (->* (flvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) f64vector?)]
  [f32vector->flvector (->* (f32vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) flvector?)]
  [f64vector->flvector (->* (f64vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) flvector?)]))

(define (list->flvector lst)
  (for/flvector #:length (length lst) ([elem (in-list lst)]) elem))

(define (flvector->f32vector fv [start 0] [end (flvector-length fv)])
  (let ([f32 (make-f32vector (- end start))])
    (do ([i start (+ i 1)]
         [j 0 (unsafe-fx+ j 1)])
        ((= i end) f32)
      (f32vector-set! f32 j (flvector-ref fv i)))))

(define (flvector->f64vector fv [start 0] [end (flvector-length fv)])
  (let ([f64 (make-f64vector (- end start))])
    (do ([i start (+ i 1)]
         [j 0 (unsafe-fx+ j 1)])
        ((= i end) f64)
      (unsafe-f64vector-set! f64 j (flvector-ref fv i)))))

(define (f32vector->flvector f32 [start 0] [end (f32vector-length f32)])
  (let ([fv (make-flvector (- end start))])
    (do ([i start (+ i 1)]
         [j 0 (unsafe-fx+ j 1)])
        ((= i end) fv)
      (unsafe-flvector-set! fv j (f32vector-ref f32 i)))))

(define (f64vector->flvector f64 [start 0] [end (f64vector-length f64)])
  (let ([fv (make-flvector (- end start))])
    (do ([i start (+ i 1)]
         [j 0 (unsafe-fx+ j 1)])
        ((= i end) fv)
      (unsafe-flvector-set! fv j (f64vector-ref f64 i)))))
