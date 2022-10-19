;;;; Implementation of SRFI 160 base @vector->list

(require (only-in racket/unsafe/ops unsafe-bytes-length unsafe-flvector-length))
(provide @vector->list)

(define (@vector->list vec [start 0] [end (@vector-length vec)])
  (let loop ((i (- end 1))
             (list '()))
    (if (< i start)
        list
        (loop (- i 1) (cons (@vector-ref vec i) list)))))
