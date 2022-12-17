#lang racket/base
;;; Autogenerated on 2022-11-02T18:09:29
;;; DO NOT EDIT

(require (only-in
          "base.rkt"
          fl?
          flvector?
          flvector
          make-flvector
          flvector-ref
          flvector-set!
          flvector-length
          flvector->list
          list->flvector)
         (only-in "../128.rkt" comparator? make-comparator)
         racket/contract
         racket/unsafe/ops)

(provide (all-from-out "base.rkt")
         flvector-append-subvectors
         (contract-out
          (flvector-unfold
           (-> procedure? exact-nonnegative-integer? any/c flvector?))
          (flvector-unfold-right
           (-> procedure? exact-nonnegative-integer? any/c flvector?))
          (flvector-copy
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            flvector?))
          (flvector-copy!
           (->*
            (flvector? exact-nonnegative-integer? flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            void?))
          (flvector-reverse-copy
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            flvector?))
          (flvector-reverse-copy!
           (->*
            (flvector? exact-nonnegative-integer? flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            void?))
          (flvector-append (-> flvector? ... flvector?))
          (flvector-concatenate (-> (listof flvector?) flvector?))
          (flvector-empty? (-> flvector? boolean?))
          (flvector= (-> flvector? flvector? flvector? ... boolean?))
          (flvector-take (-> flvector? exact-nonnegative-integer? flvector?))
          (flvector-take-right
           (-> flvector? exact-nonnegative-integer? flvector?))
          (flvector-drop (-> flvector? exact-nonnegative-integer? flvector?))
          (flvector-drop-right
           (-> flvector? exact-nonnegative-integer? flvector?))
          (flvector-segment
           (-> flvector? exact-positive-integer? (listof flvector?)))
          (flvector-fold
           (->i
            ((kons
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 2)))))
             (knil any/c)
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ any/c)))
          (flvector-fold-right
           (->i
            ((kons
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 2)))))
             (knil any/c)
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ any/c)))
          (flvector-map
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> fl?)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ flvector?)))
          (flvector-map!
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> fl?)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ flvector?)))
          (flvector-for-each
           (->i
            ((proc
              (vecs)
              (lambda (f)
                (and (procedure? f)
                     (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ void?)))
          (flvector-count
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ exact-nonnegative-integer?)))
          (flvector-cumulate (-> (-> any/c fl? fl?) any/c flvector? flvector?))
          (flvector-take-while (-> (-> fl? any/c) flvector? flvector?))
          (flvector-take-while-right (-> (-> fl? any/c) flvector? flvector?))
          (flvector-drop-while (-> (-> fl? any/c) flvector? flvector?))
          (flvector-drop-while-right (-> (-> fl? any/c) flvector? flvector?))
          (flvector-index
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ (or/c exact-nonnegative-integer? #f))))
          (flvector-index-right
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ (or/c exact-nonnegative-integer? #f))))
          (flvector-skip
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ (or/c exact-nonnegative-integer? #f))))
          (flvector-skip-right
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ (or/c exact-nonnegative-integer? #f))))
          (flvector-any
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ any/c)))
          (flvector-every
           (->i
            ((proc
              (vecs)
              (and/c
               (unconstrained-domain-> any/c)
               (lambda (f) (procedure-arity-includes? f (+ (length vecs) 1)))))
             (vec1 flvector?))
            ()
            #:rest
            (vecs (listof flvector?))
            (_ any/c)))
          (flvector-partition
           (->
            (-> fl? any/c)
            flvector?
            (values flvector? exact-nonnegative-integer?)))
          (flvector-filter (-> (-> fl? any/c) flvector? flvector?))
          (flvector-remove (-> (-> fl? any/c) flvector? flvector?))
          (flvector-swap!
           (->
            flvector?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            void?))
          (flvector-fill!
           (->*
            (flvector? fl?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            void?))
          (flvector-reverse!
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            void?))
          (flvector-unfold!
           (->
            (-> exact-nonnegative-integer? any/c (values fl? any/c))
            flvector?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            any/c
            void?))
          (flvector-unfold-right!
           (->
            (-> exact-nonnegative-integer? any/c (values fl? any/c))
            flvector?
            exact-nonnegative-integer?
            exact-nonnegative-integer?
            any/c
            void?))
          (reverse-flvector->list
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            (listof fl?)))
          (reverse-list->flvector (-> (listof fl?) flvector?))
          (flvector->vector
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            (vectorof fl?)))
          (vector->flvector
           (->*
            ((vectorof fl?))
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            flvector?))
          (make-flvector-generator
           (->*
            (flvector?)
            (exact-nonnegative-integer? exact-nonnegative-integer?)
            (-> (or/c fl? eof-object?))))
          (write-flvector (->* (flvector?) (output-port?) void?))
          (flvector-comparator comparator?)))

(define (flvector-unfold f len seed)
  (let ((v (make-flvector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (unsafe-flvector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (flvector-unfold-right f len seed)
  (let ((v (make-flvector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (unsafe-flvector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define (flvector-copy vec (start 0) (end (unsafe-flvector-length vec)))
  (let ((v (make-flvector (- end start))))
    (flvector-copy! v 0 vec start end)
    v))

(define (flvector-copy!
         to
         at
         from
         (start 0)
         (end (unsafe-flvector-length from)))
  (let loop ((at at) (i start))
    (unless (= i end)
      (flvector-set! to at (flvector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define (flvector-reverse-copy vec (start 0) (end (unsafe-flvector-length vec)))
  (let ((v (make-flvector (- end start))))
    (flvector-reverse-copy! v 0 vec start end)
    v))

(define (flvector-reverse-copy!
         to
         at
         from
         (start 0)
         (end (unsafe-flvector-length from)))
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (flvector-set! to at (flvector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (flvector-append . vecs) (flvector-concatenate vecs))

(define (flvector-concatenate vecs)
  (let ((v (make-flvector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (flvector-copy! v at vec 0 (unsafe-flvector-length vec))
          (loop (cdr vecs) (+ at (unsafe-flvector-length vec)))))
      v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (unsafe-flvector-length (car vecs)) (len-sum (cdr vecs)))))

(define (flvector-append-subvectors . args)
  (let ((v (make-flvector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args)) (start (cadr args)) (end (caddr args)))
          (flvector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs)) (len-subsum (cdddr vecs)))))

(define (flvector-empty? vec) (zero? (unsafe-flvector-length vec)))

(define (flvector= vec1 vec2 . vecs)
  (and (fldyadic-vecs=
        vec1
        0
        (unsafe-flvector-length vec1)
        vec2
        0
        (unsafe-flvector-length vec2))
       (or (null? vecs)
           (null? (car vecs))
           (flvector= vec2 (car vecs) (cdr vecs)))))

(define (fldyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
   ((not (= end1 end2)) #f)
   ((not (< start1 end1)) #t)
   ((let ((elt1 (flvector-ref vec1 start1)) (elt2 (flvector-ref vec2 start2)))
      (= elt1 elt2))
    (fldyadic-vecs= vec1 (+ start1 1) end1 vec2 (+ start2 1) end2))
   (else #f)))

(define (flvector-take vec n)
  (let ((v (make-flvector n))) (flvector-copy! v 0 vec 0 n) v))

(define (flvector-take-right vec n)
  (let ((v (make-flvector n)) (len (unsafe-flvector-length vec)))
    (flvector-copy! v 0 vec (- len n) len)
    v))

(define (flvector-drop vec n)
  (let* ((len (unsafe-flvector-length vec))
         (vlen (- len n))
         (v (make-flvector vlen)))
    (flvector-copy! v 0 vec n len)
    v))

(define (flvector-drop-right vec n)
  (let* ((len (unsafe-flvector-length vec))
         (rlen (- len n))
         (v (make-flvector rlen)))
    (flvector-copy! v 0 vec 0 rlen)
    v))

(define (flvector-segment vec n)
  (let loop ((r '()) (i 0) (remain (unsafe-flvector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
         (cons (flvector-copy vec i (+ i size)) r)
         (+ i size)
         (- remain size))))))

(define (%flvectors-ref vecs i) (map (lambda (v) (flvector-ref v i)) vecs))

(define (flvector-fold kons knil vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len) r (loop (kons r (unsafe-flvector-ref vec i)) (+ i 1)))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%flvectors-ref vecs i)) (+ i 1)))))))

(define (flvector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((r knil) (i (- (unsafe-flvector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (unsafe-flvector-ref vec i)) (- i 1)))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%flvectors-ref vecs i)) (- i 1)))))))

(define (flvector-map f vec . vecs)
  (if (null? vecs)
    (let* ((len (unsafe-flvector-length vec)) (v (make-flvector len)))
      (let loop ((i 0))
        (unless (= i len)
          (unsafe-flvector-set! v i (f (unsafe-flvector-ref vec i)))
          (loop (+ i 1))))
      v)
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs)))
           (v (make-flvector len)))
      (let loop ((i 0))
        (unless (= i len)
          (flvector-set! v i (apply f (%flvectors-ref vecs i)))
          (loop (+ i 1))))
      v)))

(define (flvector-map! f vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (unsafe-flvector-set! vec i (f (unsafe-flvector-ref vec i)))
          (loop (+ i 1)))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (flvector-set! vec i (apply f (%flvectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (flvector-for-each f vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0))
        (unless (= i len) (f (unsafe-flvector-ref vec i)) (loop (+ i 1)))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0))
        (unless (= i len) (apply f (%flvectors-ref vecs i)) (loop (+ i 1)))))))

(define (flvector-count pred vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (unsafe-flvector-length vec)) r)
         ((pred (unsafe-flvector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%flvectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (flvector-cumulate f knil vec)
  (let* ((len (unsafe-flvector-length vec)) (v (make-flvector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (unsafe-flvector-ref vec i))))
          (unsafe-flvector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (flvector-take-while pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (idx (flvector-skip pred vec))
         (idx* (if idx idx len)))
    (flvector-copy vec 0 idx*)))

(define (flvector-take-while-right pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (idx (flvector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (flvector-copy vec idx* len)))

(define (flvector-drop-while pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (idx (flvector-skip pred vec))
         (idx* (if idx idx len)))
    (flvector-copy vec idx* len)))

(define (flvector-drop-while-right pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (idx (flvector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (flvector-copy vec 0 (+ 1 idx*))))

(define (flvector-index pred vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (unsafe-flvector-ref vec i)) i)
         (else (loop (+ i 1))))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%flvectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (flvector-index-right pred vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (unsafe-flvector-ref vec i)) i)
         (else (loop (- i 1))))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%flvectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (flvector-skip pred vec . vecs)
  (if (null? vecs)
    (flvector-index (lambda (x) (not (pred x))) vec)
    (apply flvector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (flvector-skip-right pred vec . vecs)
  (if (null? vecs)
    (flvector-index-right (lambda (x) (not (pred x))) vec)
    (apply flvector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (flvector-any pred vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (unsafe-flvector-ref vec i)))
         (else (loop (+ i 1))))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%flvectors-ref vecs i)))
         (else (loop (+ i 1))))))))

(define (flvector-every pred vec . vecs)
  (if (null? vecs)
    (let ((len (unsafe-flvector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (unsafe-flvector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    (let* ((vecs (cons vec vecs))
           (len (apply min (map unsafe-flvector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%flvectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (flvector-partition pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (cnt (flvector-count pred vec))
         (r (make-flvector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
       ((= i len) r)
       ((pred (unsafe-flvector-ref vec i))
        (unsafe-flvector-set! r yes (unsafe-flvector-ref vec i))
        (loop (+ i 1) (+ yes 1) no))
       (else
        (unsafe-flvector-set! r no (unsafe-flvector-ref vec i))
        (loop (+ i 1) yes (+ no 1)))))))

(define (flvector-filter pred vec)
  (let* ((len (unsafe-flvector-length vec))
         (cnt (flvector-count pred vec))
         (r (make-flvector cnt)))
    (let loop ((i 0) (j 0))
      (cond
       ((= i len) r)
       ((pred (unsafe-flvector-ref vec i))
        (unsafe-flvector-set! r j (unsafe-flvector-ref vec i))
        (loop (+ i 1) (+ j 1)))
       (else (loop (+ i 1) j))))))

(define (flvector-remove pred vec)
  (flvector-filter (lambda (x) (not (pred x))) vec))

(define (flvector-swap! vec i j)
  (let ((ival (flvector-ref vec i)) (jval (flvector-ref vec j)))
    (flvector-set! vec i jval)
    (flvector-set! vec j ival)))

(define (flvector-fill! vec fill (start 0) (end (unsafe-flvector-length vec)))
  (unless (= start end)
    (flvector-set! vec start fill)
    (flvector-fill! vec fill (+ start 1) end)))

(define (flvector-reverse! vec (start 0) (end (unsafe-flvector-length vec)))
  (let loop ((i start) (j (- end 1)))
    (when (< i j) (flvector-swap! vec i j) (loop (+ i 1) (- j 1)))))

(define (flvector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (flvector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (flvector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (flvector-set! vec i elt)
        (loop (- i 1) seed)))))

(define (reverse-flvector->list
         vec
         (start 0)
         (end (unsafe-flvector-length vec)))
  (let loop ((i start) (r '()))
    (if (= i end) r (loop (+ 1 i) (cons (flvector-ref vec i) r)))))

(define (reverse-list->flvector list)
  (let* ((len (length list)) (r (make-flvector len)))
    (let loop ((i 0) (list list))
      (cond
       ((= i len) r)
       (else
        (flvector-set! r (- len i 1) (car list))
        (loop (+ i 1) (cdr list)))))))

(define (flvector->vector vec (start 0) (end (unsafe-flvector-length vec)))
  (let* ((len (- end start)) (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
       ((= i end) r)
       (else (vector-set! r o (flvector-ref vec i)) (loop (+ i 1) (+ o 1)))))))

(define (vector->flvector vec (start 0) (end (vector-length vec)))
  (let* ((len (- end start)) (r (make-flvector len)))
    (let loop ((i start) (o 0))
      (cond
       ((= i end) r)
       (else (flvector-set! r o (vector-ref vec i)) (loop (+ i 1) (+ o 1)))))))

(define (make-flvector-generator
         vec
         (start 0)
         (end (unsafe-flvector-length vec)))
  (lambda ()
    (if (>= start end)
      eof
      (let ((next (flvector-ref vec start))) (set! start (+ start 1)) next))))

(define (write-flvector vec (port (current-output-port)))
  (begin
    (fprintf port "#~A(" 'fl)
    (let ((last (- (unsafe-flvector-length vec) 1)))
      (let loop ((i 0))
        (cond
         ((= i last)
          (write (unsafe-flvector-ref vec i) port)
          (display ")" port))
         (else
          (write (unsafe-flvector-ref vec i) port)
          (display " " port)
          (loop (+ i 1))))))))

(define (flvector< vec1 vec2)
  (let ((len1 (unsafe-flvector-length vec1))
        (len2 (unsafe-flvector-length vec2)))
    (cond
     ((< len1 len2) #t)
     ((> len1 len2) #f)
     (else
      (let loop ((i 0))
        (cond
         ((= i len1) #f)
         ((< (unsafe-flvector-ref vec1 i) (unsafe-flvector-ref vec2 i)) #t)
         ((> (unsafe-flvector-ref vec1 i) (unsafe-flvector-ref vec2 i)) #f)
         (else (loop (+ i 1)))))))))

(define (flvector-hash vec)
  (let ((len (min 256 (unsafe-flvector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (unsafe-flvector-ref vec i)))))))

(define flvector-comparator
  (make-comparator flvector? flvector= flvector< flvector-hash))
