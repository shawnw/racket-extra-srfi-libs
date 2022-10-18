;;; This code is the same for all SRFI 160 vector sizes.
;;; The @s appearing in the code are expanded to u8, s8, etc.

;; make-@vector defined in (srfi 160 base)

;; @vector defined in (srfi 160 base)

(require (only-in "base.rkt" @? @vector? @vector make-@vector @vector-ref @vector-set! @vector-length
                  @vector->list list->@vector)
         (only-in "../128.rkt" comparator? make-comparator)
         racket/contract)
(module+ test (require rackunit))

(provide
 (all-from-out "base.rkt")
 @vector-append-subvectors ;; TODO: Add contract
 (contract-out
  [@vector-unfold (-> procedure? exact-nonnegative-integer? any/c @vector?)]
  [@vector-unfold-right (-> procedure? exact-nonnegative-integer? any/c @vector?)]
  [@vector-copy (->* (@vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) @vector?)]
  [@vector-copy! (->* (@vector? exact-nonnegative-integer? @vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [@vector-reverse-copy (->* (@vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) @vector?)]
  [@vector-reverse-copy! (->* (@vector? exact-nonnegative-integer? @vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [@vector-append (-> @vector? ... @vector?)]
  [@vector-concatenate (-> (listof @vector?) @vector?)]
  [@vector-empty? (-> @vector? boolean?)]
  [@vector= (-> @vector? @vector? @vector? ... boolean?)]
  [@vector-take (-> @vector? exact-nonnegative-integer? @vector?)]
  [@vector-take-right (-> @vector? exact-nonnegative-integer? @vector?)]
  [@vector-drop (-> @vector? exact-nonnegative-integer? @vector?)]
  [@vector-drop-right (-> @vector? exact-nonnegative-integer? @vector?)]
  [@vector-segment (-> @vector? exact-positive-integer? (listof @vector?))]
  [@vector-fold (-> (-> @? @? ... any/c any/c) any/c @vector? @vector? ... any/c)]
  [@vector-fold-right (-> (-> @? @? ... any/c any/c) any/c @vector? @vector? ... any/c)]
  [@vector-map (-> (-> @? @? ... @?) @vector? @vector? ... @vector?)]
  [@vector-map! (-> (-> @? @? ... @?) @vector? @vector? ... @vector?)]
  [@vector-for-each (-> (-> @? @? ... @?) @vector? @vector? ... void?)]
  [@vector-count (-> (-> @? @? ... any/c) @vector? @vector? ... exact-nonnegative-integer?)]
  [@vector-cumulate (-> (-> any/c @? @?) any/c @vector? @vector?)]
  [@vector-take-while (-> (-> @? any/c) @vector? @vector?)]
  [@vector-take-while-right (-> (-> @? any/c) @vector? @vector?)]
  [@vector-drop-while (-> (-> @? any/c) @vector? @vector?)]
  [@vector-drop-while-right (-> (-> @? any/c) @vector? @vector?)]
  [@vector-index (-> (-> @? @? ... any/c) @vector? @vector? ... (or/c exact-nonnegative-integer? #f))]
  [@vector-index-right (-> (-> @? @? ... any/c) @vector? @vector? ... (or/c exact-nonnegative-integer? #f))]
  [@vector-skip (-> (-> @? @? ... any/c) @vector? @vector? ... (or/c exact-nonnegative-integer? #f))]
  [@vector-skip-right (-> (-> @? @? ... any/c) @vector? @vector? ... (or/c exact-nonnegative-integer? #f))]
  [@vector-any (-> (-> @? @? ... any/c) @vector? @vector? ... any)]
  [@vector-every (-> (-> @? @? ... any/c) @vector? @vector? ... any)]
  [@vector-partition (-> (-> @? any/c) @vector? (values @vector? exact-nonnegative-integer?))]
  [@vector-filter (-> (-> @? any/c) @vector? @vector?)]
  [@vector-remove (-> (-> @? any/c) @vector? @vector?)]
  [@vector-swap! (-> @vector? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [@vector-fill! (->* (@vector? @?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [@vector-reverse! (->* (@vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [@vector-unfold! (-> (-> exact-nonnegative-integer? any/c (values @? any/c)) @vector?
                       exact-nonnegative-integer? exact-nonnegative-integer? any/c void?)]
  [@vector-unfold-right! (-> (-> exact-nonnegative-integer? any/c (values @? any/c)) @vector?
                             exact-nonnegative-integer? exact-nonnegative-integer? any/c void?)]
  [reverse-@vector->list (->* (@vector?)  (exact-nonnegative-integer? exact-nonnegative-integer?) (listof @?))]
  [reverse-list->@vector (-> (listof @?) @vector?)]
  [@vector->vector  (->* (@vector?)  (exact-nonnegative-integer? exact-nonnegative-integer?) (vectorof @?))]
  [vector->@vector  (->* ((vectorof @?)) (exact-nonnegative-integer? exact-nonnegative-integer?) @vector?)]
  [make-@vector-generator (->* (@vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (-> (or/c @? eof-object?)))]
  [write-@vector (->* (@vector?) (output-port?) void?)]
  [@vector-comparator comparator?]))


(define (@vector-unfold f len seed)
  (let ((v (make-@vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (@vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (@vector-unfold-right f len seed)
  (let ((v (make-@vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (@vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define (@vector-copy vec [start 0] [end (@vector-length vec)])
  (let ((v (make-@vector (- end start))))
    (@vector-copy! v 0 vec start end)
    v))

(define (@vector-copy! to at from [start 0] [end (@vector-length from)])
  (let loop ((at at) (i start))
    (unless (= i end)
      (@vector-set! to at (@vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define (@vector-reverse-copy vec [start 0] [end (@vector-length vec)])
  (let ((v (make-@vector (- end start))))
    (@vector-reverse-copy! v 0 vec start end)
    v))

(define (@vector-reverse-copy! to at from [start 0] [end (@vector-length from)])
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (@vector-set! to at (@vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (@vector-append . vecs)
  (@vector-concatenate vecs))

(define (@vector-concatenate vecs)
  (let ((v (make-@vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (@vector-copy! v at vec 0 (@vector-length vec))
          (loop (cdr vecs) (+ at (@vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (@vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (@vector-append-subvectors . args)
  (let ((v (make-@vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (@vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; @? defined in (srfi 160 base)

;; @vector? defined in (srfi 160 base)

(define (@vector-empty? vec)
  (zero? (@vector-length vec)))

(define (@vector= vec1 vec2 . vecs)
  (and (@dyadic-vecs= vec1 0 (@vector-length vec1)
                      vec2 0 (@vector-length vec2))
       (or (null? vecs)
           (@vector= vec2 (car vecs) (cdr vecs)))))

(define (@dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (@vector-ref vec1 start1))
           (elt2 (@vector-ref vec2 start2)))
      (= elt1 elt2))
     (@dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; @vector-ref defined in (srfi 160 base)

;; @vector-length defined in (srfi 160 base)

(define (@vector-take vec n)
  (let ((v (make-@vector n)))
    (@vector-copy! v 0 vec 0 n)
    v))

(define (@vector-take-right vec n)
  (let ((v (make-@vector n))
        (len (@vector-length vec)))
    (@vector-copy! v 0 vec (- len n) len)
    v))

(define (@vector-drop vec n)
 (let* ((len (@vector-length vec))
        (vlen (- len n))
        (v (make-@vector vlen)))
    (@vector-copy! v 0 vec n len)
    v))

(define (@vector-drop-right vec n)
  (let* ((len (@vector-length vec))
         (rlen (- len n))
         (v (make-@vector rlen)))
    (@vector-copy! v 0 vec 0 rlen)
    v))

(define (@vector-segment vec n)
  #;(unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (@vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (@vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%@vectors-ref vecs i)
  (map (lambda (v) (@vector-ref v i)) vecs))

(define (@vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (@vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%@vectors-ref vecs i))
                (+ i 1)))))))

(define (@vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((r knil) (i (- (@vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (@vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%@vectors-ref vecs i))
                (- i 1)))))))

(define (@vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (@vector-length vec))
           (v (make-@vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (@vector-set! v i (f (@vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs)))
           (v (make-@vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (@vector-set! v i (apply f (%@vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (@vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (@vector-set! vec i (f (@vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (@vector-set! vec i (apply f (%@vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (@vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (@vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%@vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (@vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (@vector-length vec)) r)
         ((pred (@vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%@vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (@vector-cumulate f knil vec)
  (let* ((len (@vector-length vec))
         (v (make-@vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (@vector-ref vec i))))
          (@vector-set! v i next)
          (loop next (+ i 1)))))
    v))

#;(define (@vector-foreach f vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (@vector-ref vec i))
        (loop (+ i 1))))))

(define (@vector-take-while pred vec)
  (let* ((len (@vector-length vec))
         (idx (@vector-skip pred vec))
         (idx* (if idx idx len)))
    (@vector-copy vec 0 idx*)))

(define (@vector-take-while-right pred vec)
  (let* ((len (@vector-length vec))
         (idx (@vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (@vector-copy vec idx* len)))

(define (@vector-drop-while pred vec)
  (let* ((len (@vector-length vec))
         (idx (@vector-skip pred vec))
         (idx* (if idx idx len)))
    (@vector-copy vec idx* len)))

(define (@vector-drop-while-right pred vec)
  (let* ((len (@vector-length vec))
         (idx (@vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (@vector-copy vec 0 (+ 1 idx*))))

(define (@vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (@vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%@vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (@vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (@vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%@vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (@vector-skip pred vec . vecs)
  (if (null? vecs)
    (@vector-index (lambda (x) (not (pred x))) vec)
    (apply @vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (@vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (@vector-index-right (lambda (x) (not (pred x))) vec)
    (apply @vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (@vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (@vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%@vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (@vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (@vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (@vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map @vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%@vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (@vector-partition pred vec)
  (let* ((len (@vector-length vec))
         (cnt (@vector-count pred vec))
         (r (make-@vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred (@vector-ref vec i))
         (@vector-set! r yes (@vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (@vector-set! r no (@vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (@vector-filter pred vec)
  (let* ((len (@vector-length vec))
         (cnt (@vector-count pred vec))
         (r (make-@vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (@vector-ref vec i))
         (@vector-set! r j (@vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (@vector-remove pred vec)
  (@vector-filter (lambda (x) (not (pred x))) vec))

;; @vector-set! defined in (srfi 160 base)

(define (@vector-swap! vec i j)
  (let ((ival (@vector-ref vec i))
        (jval (@vector-ref vec j)))
    (@vector-set! vec i jval)
    (@vector-set! vec j ival)))

(define (@vector-fill! vec fill [start 0] [end (@vector-length vec)])
  (unless (= start end)
    (@vector-set! vec start fill)
    (@vector-fill! vec fill (+ start 1) end)))

(define (@vector-reverse! vec [start 0] [end (@vector-length vec)])
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (@vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (@vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (@vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (@vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (@vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define (reverse-@vector->list vec [start 0] [end (@vector-length vec)])
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (@vector-ref vec i) r)))))

(define (reverse-list->@vector list)
  (let* ((len (length list))
         (r (make-@vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (@vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define (@vector->vector vec [start 0] [end (@vector-length vec)])
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (@vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define (vector->@vector vec [start 0] [end (vector-length vec)])
  (let* ((len (- end start))
         (r (make-@vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (@vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define (make-@vector-generator vec [start 0] [end (@vector-length vec)])
  (lambda () (if (>= start end)
                 eof
                 (let ((next (@vector-ref vec start)))
                   (set! start (+ start 1))
                   next))))

(define (write-@vector vec [port (current-output-port)])
  (fprintf port "#~A(" (quote @))
  (let ((last (- (@vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (@vector-ref vec i) port)
         (display ")" port))
        (else
          (write (@vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (@vector< vec1 vec2)
  (let ((len1 (@vector-length vec1))
        (len2 (@vector-length vec2)))
    (cond
      ((< len1 len2)
       #t)
      ((> len1 len2)
       #f)
      (else
       (let loop ((i 0))
         (cond
           ((= i len1)
            #f)
           ((< (@vector-ref vec1 i) (@vector-ref vec2 i))
            #t)
           ((> (@vector-ref vec1 i) (@vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (@vector-hash vec)
  (let ((len (min 256 (@vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (@vector-ref vec i)))))))

(define @vector-comparator
  (make-comparator @vector? @vector= @vector< @vector-hash))
