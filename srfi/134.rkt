#lang racket/base

;;;  Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
;;;  Copyright (c) 2022 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation files
;;;  (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify, merge,
;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;  and to permit persons to whom the Software is furnished to do so,
;;;  subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.</p>
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;  SOFTWARE
;;;

;; This implements banker's deque as described in
;; Chris Okasaki's Purely Functional Data Structures.
;; It provides amortized O(1) basic operations.
;; Original two-list version written by Shiro Kawai.
;; Stream version by Wolfgang Corcoran-Mathe.


(require racket/contract racket/struct
         (only-in srfi/1 unfold list= concatenate zip append-map iota take drop split-at) srfi/8 srfi/41 (only-in "158.rkt" generator generator->list))
(module+ test (require rackunit))

(provide
 (contract-out
  [ideque? predicate/c]
  [ideque (-> any/c ... ideque?)]
  [ideque-tabulate (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? any/c) ideque?)]
  [ideque-unfold (-> (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c ideque?)]
  [ideque-unfold-right (-> (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c ideque?)]
  [ideque-empty? (-> ideque? boolean?)]
  [ideque= (-> (-> any/c any/c any/c) ideque? ... boolean?)]
  [ideque-any (-> (-> any/c any/c) ideque? any/c)]
  [ideque-every (-> (-> any/c any/c) ideque? any/c)]

  [ideque-front (-> (and/c ideque? (not/c ideque-empty?)) any/c)]
  [ideque-add-front (-> ideque? any/c ideque?)]
  [ideque-remove-front (-> (and/c ideque? (not/c ideque-empty?)) ideque?)]
  [ideque-back (-> (and/c ideque? (not/c ideque-empty?)) any/c)]
  [ideque-add-back (-> ideque? any/c ideque?)]
  [ideque-remove-back (-> (and/c ideque? (not/c ideque-empty?)) ideque?)]

  [ideque-ref (-> ideque? exact-nonnegative-integer? any/c)]
  [ideque-take (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-take-right (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-drop (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-drop-right (-> ideque? exact-nonnegative-integer? ideque?)]
  [ideque-split-at (-> ideque? exact-nonnegative-integer? (values ideque? ideque?))]
  
  [ideque-length (-> ideque? exact-nonnegative-integer?)]
  [ideque-append (-> ideque? ... ideque?)]
  [ideque-reverse (-> ideque? ideque?)]
  [ideque-count (-> (-> any/c any/c) ideque? exact-nonnegative-integer?)]
  [ideque-zip (-> ideque? ideque? ... ideque?)]

  [ideque-map (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-filter-map (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-for-each (-> (-> any/c any/c) ideque? void?)]
  [ideque-for-each-right (-> (-> any/c any/c) ideque? void?)]
  [ideque-fold (-> (-> any/c any/c any/c) any/c ideque? any/c)]
  [ideque-fold-right (-> (-> any/c any/c any/c) any/c ideque? any/c)]
  [ideque-append-map (-> (-> any/c list?) ideque? ideque?)]

  [ideque-filter (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-remove (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-partition (-> (-> any/c any/c) ideque? (values ideque? ideque?))]

  [ideque-find (->* ((-> any/c any/c) ideque?) ((-> any/c)) any/c)]
  [ideque-find-right (->* ((-> any/c any/c) ideque?) ((-> any/c)) any/c)]
  [ideque-take-while (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-take-while-right (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-drop-while (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-drop-while-right (-> (-> any/c any/c) ideque? ideque?)]
  [ideque-span (-> (-> any/c any/c) ideque? (values ideque? ideque?))]
  [ideque-break (-> (-> any/c any/c) ideque? (values ideque? ideque?))]

  [list->ideque (-> list? ideque?)]
  [ideque->list (-> ideque? list?)]
  [generator->ideque (-> (-> any/c) ideque?)]
  [ideque->generator (-> ideque? (-> any/c))]

  ;; Extras
  [in-ideque (-> ideque? sequence?)]
  [in-ideque-backwards (-> ideque? sequence?)]
  ))

;;;; Stream utility

(define (stream-count pred s)
  (stream-fold (lambda (n x) (if (pred x) (+ n 1) n))
               0
               s))

(define stream-filter-map
  (stream-lambda (proc s)
                 (cond ((stream-null? s) stream-null)
                       ((proc (stream-car s)) =>
                                              (lambda (x)
                                                (stream-cons x (stream-filter-map proc (stream-cdr s)))))
                       (else (stream-filter-map proc (stream-cdr s))))))

;; From SRFI 41. Clever!
(define (stream-partition pred s)
  (stream-unfolds
   (lambda (s)
     (if (stream-null? s)
         (values s '() '())
         (let ((a (stream-car s)) (s* (stream-cdr s)))
           (if (pred a)
               (values s* (list a) #f)
               (values s* #f (list a))))))
   s))

;; Could be improved.
(define (stream-span pred s)
  (values (stream-take-while pred s) (stream-drop-while pred s)))

(define (stream-break pred s)
  (stream-span (lambda (x) (not (pred x))) s))

(define (stream-any pred s)
  (let lp ((s s))
    (cond ((stream-null? s) #f)
          ((pred (stream-car s)))
          (else (lp (stream-cdr s))))))

(define (stream-every pred s)
  (let lp ((s s) (last-val #t))
    (cond ((stream-null? s) last-val)
          ((pred (stream-car s)) =>
                                 (lambda (x) (lp (stream-cdr s) x)))
          (else #f))))

;; Compare two streams for equality using 'elt=' to compare elements.
(define (stream=? elt= s1 s2)
  (if (stream-null? s1)
      (stream-null? s2)
      (and (stream-pair? s2)
           (elt= (stream-car s1) (stream-car s2))
           (stream=? elt= (stream-cdr s1) (stream-cdr s2)))))

;; some compatibility stuff
#;(define-syntax receive
  (syntax-rules ()
    ((_ binds mv-expr body ...)
     (let-values ((binds mv-expr)) body ...))))

;;;; ideque type

(struct dq (lenf f lenr r)
  #:extra-constructor-name %make-dq
  #:methods gen:equal+hash
  [(define (equal-proc a b equal-wrapper?)
     (%ideque=-binary equal-wrapper? a b))
   (define (hash-proc a hash-code)
     (ideque-fold (lambda (elem hash) (bitwise-xor (hash-code elem) hash)) 0 a))
      (define (hash2-proc a hash-code)
     (ideque-fold (lambda (elem hash) (bitwise-xor (hash-code elem) hash)) 0 a))]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'ideque)
      (lambda (obj) (in-ideque obj))))]
  #:property prop:custom-print-quotable 'never
  #:property prop:sequence
  (lambda (idq) (in-ideque idq))
  )

(define (ideque? obj) (dq? obj))

;; We use a singleton for empty deque
(define *empty* (%make-dq 0 stream-null 0 stream-null))

;; Internal constructor.  Returns a new ideque, with balancing 'front' and
;; 'rear' chains.

;; Front/back stream length differential factor.
(define stream-length-factor 3)

(define (make-deque lenf f lenr r)
  (cond ((> lenf (+ (* lenr stream-length-factor) 1))
         (let* ((i (quotient (+ lenf lenr) 2))
                (j (- (+ lenf lenr) i))
                (f. (stream-take i f))
                (r. (stream-append
                     r
                     (stream-reverse (stream-drop i f)))))
           (%make-dq i f. j r.)))
        ((> lenr (+ (* lenf stream-length-factor) 1))
         (let* ((j (quotient (+ lenf lenr) 2))
                (i (- (+ lenf lenr) j))
                (r. (stream-take j r))
                (f. (stream-append
                     f
                     (stream-reverse (stream-drop j r)))))
           (%make-dq i f. j r.)))
        (else (%make-dq lenf f lenr r))))

;;;; Basic operations

(define (ideque-empty? dq)
  ;(%check-ideque dq)
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

(define (ideque-add-front dq x)
  ;(%check-ideque dq)
  (make-deque (+ (dq-lenf dq) 1)
              (stream-cons x (dq-f dq))
              (dq-lenr dq)
              (dq-r dq)))

(define (ideque-front dq)
  ;(%check-ideque dq)
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (raise-argument-error 'ideque-front "(not/c ideque-empty?)" dq)
          (stream-car (dq-r dq)))
      (stream-car (dq-f dq))))

(define (ideque-remove-front dq)
  ;(%check-ideque dq)
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (raise-argument-error 'ideque-remove-front "(not/c ideque-empty?)" dq)
          *empty*)
      (make-deque (- (dq-lenf dq) 1)
                  (stream-cdr (dq-f dq))
                  (dq-lenr dq)
                  (dq-r dq))))

(define (ideque-add-back dq x)
  ;(%check-ideque dq)
  (make-deque (dq-lenf dq)
              (dq-f dq)
              (+ (dq-lenr dq) 1)
              (stream-cons x (dq-r dq))))

(define (ideque-back dq)
  ;(%check-ideque dq)
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (raise-argument-error 'ideque-back "(not/c ideque-empty?)" dq)
          (stream-car (dq-f dq)))
      (stream-car (dq-r dq))))

(define (ideque-remove-back dq)
  ;(%check-ideque dq)
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (raise-argument-error 'ideque-remove-back "(not/c ideque-empty?)" dq)
          *empty*)
      (make-deque (dq-lenf dq)
                  (dq-f dq)
                  (- (dq-lenr dq) 1)
                  (stream-cdr (dq-r dq)))))

(define (ideque-reverse dq)
  ;(%check-ideque dq)
  (if (ideque-empty? dq)
      *empty*
      (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;; Exported constructors

(define (ideque . args)
  (if (null? args)
      *empty*
      (list->ideque args)))

(define (ideque-tabulate size init)
  (let ((lenf (quotient size 2))
        (lenr (quotient (+ size 1) 2)))
    (%make-dq lenf
              (stream-unfold init
                             (lambda (n) (< n lenf))
                             (lambda (n) (+ n 1))
                             0)
              lenr
              (stream-unfold (lambda (n) (init (- size n 1)))
                             (lambda (n) (< n lenr))
                             (lambda (n) (+ n 1))
                             0))))

(define (ideque-unfold p f g seed)
  (list->ideque (unfold p f g seed)))

(define (ideque-unfold-right p f g seed)
  (ideque-reverse (list->ideque (unfold p f g seed))))

;;;; Other operations

(define ideque=
  (case-lambda
    ((elt=) #t)
    ((elt= dq1 dq2) ; fast path
     (%ideque=-binary elt= dq1 dq2))
    ((elt= . dqs)
     ;; The comparison scheme is the same as srfi-1's list=.
     ;; This could be tuned.
     (apply list= elt= (map ideque->list dqs)))))

(define (%ideque-same-length dq1 dq2)
  (= (ideque-length dq1) (ideque-length dq2)))

(define (%ideque=-binary elt= dq1 dq2)
  ;(%check-ideque dq1)
  ;(%check-ideque dq2)
  (or (eq? dq1 dq2)
      (and (%ideque-same-length dq1 dq2)
           (stream=? elt=
                     (stream-append (dq-f dq1)
                                    (stream-reverse (dq-r dq1)))
                     (stream-append (dq-f dq2)
                                    (stream-reverse (dq-r dq2)))))))


(define (ideque-ref dq n)
  ;(%check-ideque dq)
  (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
    (cond ((or (< n 0) (>= n len)) (raise-range-error 'ideque-ref "ideque" "" n dq 0 (- len 1)))
          ((< n (dq-lenf dq)) (stream-ref (dq-f dq) n))
          (else (stream-ref (dq-r dq) (- len n 1))))))

(define (%ideque-take dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq)))
    (if (<= n lenf)
        (make-deque n (stream-take n f) 0 stream-null)
        (let ((k (- lenr (- n lenf))))
          (make-deque lenf f (- lenr k) (stream-drop k (dq-r dq)))))))

(define (%ideque-drop dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq))
        (r    (dq-r dq)))
    (if (<= n lenf)
        (make-deque (- lenf n) (stream-drop n f) lenr r)
        (let ((lenr. (- lenr (- n lenf))))
          (make-deque 0 stream-null lenr. (stream-take lenr. r))))))

(define-syntax-rule (define/index-in-range (name dq n) body ...)
  (define (name dq n)
    (unless (<= 0 n (ideque-length dq))
      (raise-range-error (quote name) "ideque" "" n dq 0 (- (ideque-length dq) 1)))
    body ...))

(define/index-in-range (ideque-take dq n)
  ;(%check-ideque dq)
  ;(%check-length dq n)
  (%ideque-take dq n))

(define/index-in-range (ideque-take-right dq n)
  ;(%check-ideque dq)
  ;(%check-length dq n)
  (%ideque-drop dq (- (ideque-length dq) n)))

(define/index-in-range (ideque-drop dq n)
  ;(%check-ideque dq)
  ;(%check-length dq n)
  (%ideque-drop dq n))

(define/index-in-range (ideque-drop-right dq n)
  ;(%check-ideque dq)
  ;(%check-length dq n)
  (%ideque-take dq (- (ideque-length dq) n)))

(define/index-in-range (ideque-split-at dq n)
  ;(%check-ideque dq)
  ;(%check-length dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

(define (ideque-length dq)
  ;(%check-ideque dq)
  (+ (dq-lenf dq) (dq-lenr dq)))

(define ideque-append
  (case-lambda
    ((dq)
     ;(%check-ideque dq)
     dq)
    ((dq1 dq2)  ; fast path
     ;(%check-ideque dq1)
     ;(%check-ideque dq2)
     (%ideque-append-binary dq1 dq2))
    (dqs
     ;(for-each %check-ideque dqs)
     (list->ideque (concatenate (map ideque->list dqs))))))

(define (%ideque-append-binary dq1 dq2)
  (cond ((zero? (ideque-length dq1)) dq2)
        ((zero? (ideque-length dq2)) dq1)
        (else
         (make-deque (ideque-length dq1)
                     (stream-append (dq-f dq1)
                                    (stream-reverse (dq-r dq1)))
                     (ideque-length dq2)
                     (stream-append (dq-r dq2)
                                    (stream-reverse (dq-f dq2)))))))

(define (ideque-count pred dq)
  ;(%check-ideque dq)
  (+ (stream-count pred (dq-f dq)) (stream-count pred (dq-r dq))))

(define (ideque-zip dq . dqs)
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (make-deque (length elts) (list->stream elts) 0 stream-null)))

(define (ideque-map proc dq)
  ;(%check-ideque dq)
  (%make-dq (dq-lenf dq) (stream-map proc (dq-f dq))
            (dq-lenr dq) (stream-map proc (dq-r dq))))

(define (ideque-filter-map proc dq)
  ;(%check-ideque dq)
  (let ((f (stream-filter-map proc (dq-f dq)))
        (r (stream-filter-map proc (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(define (ideque-for-each proc dq)
  ;(%check-ideque dq)
  (stream-for-each proc (dq-f dq))
  (stream-for-each proc (stream-reverse (dq-r dq))))

(define (ideque-for-each-right proc dq)
  ;(%check-ideque dq)
  (stream-for-each proc (dq-r dq))
  (stream-for-each proc (stream-reverse (dq-f dq))))

(define (ideque-fold proc knil dq)
  (let ((proc* (lambda (acc x) (proc x acc))))  ; stream-fold compat
    ;(%check-ideque dq)
    (stream-fold proc*
                 (stream-fold proc* knil (dq-f dq))
                 (stream-reverse (dq-r dq)))))

;; There's no stream-fold-right, so just convert dq.
(define (ideque-fold-right proc knil dq)
  ;(%check-ideque dq)
  (foldr proc knil (ideque->list dq)))

(define (ideque-append-map proc dq)
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(define (%ideque-filter pred dq)
  ;(%check-ideque dq)
  (let ((f (stream-filter pred (dq-f dq)))
        (r (stream-filter pred (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(define (ideque-filter pred dq) (%ideque-filter pred dq))
(define (ideque-remove pred dq)
  (%ideque-filter (lambda (x) (not (pred x))) dq))

(define (ideque-partition pred dq)
  ;(%check-ideque dq)
  (receive (f1 f2) (stream-partition pred (dq-f dq))
           (receive (r1 r2) (stream-partition pred (dq-r dq))
                    (values (make-deque (stream-length f1) f1 (stream-length r1) r1)
                            (make-deque (stream-length f2) f2 (stream-length r2) r2)))))

(define *not-found* (cons #f #f)) ; unique value

(define (%search pred seq1 seq2 failure)
  ;; We could write seek as CPS, but we employ *not-found* instead to avoid
  ;; closure allocation.
  (define (seek pred s)
    (cond ((stream-null? s) *not-found*)
          ((pred (stream-car s)) (stream-car s))
          (else (seek pred (stream-cdr s)))))
  (let ((r (seek pred seq1)))
    (if (not (eq? r *not-found*))
        r
        (let ((r (seek pred (stream-reverse seq2))))
          (if (not (eq? r *not-found*))
              r
              (failure))))))

(define (ideque-find pred dq . opts)
  ;(%check-ideque dq)
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-f dq) (dq-r dq) failure)))

(define (ideque-find-right pred dq . opts)
  ;(%check-ideque dq)
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-r dq) (dq-f dq) failure)))

(define (ideque-take-while pred dq)
  ;(%check-ideque dq)
  (receive (hd tl) (stream-span pred (dq-f dq))
           (if (stream-null? tl)
               (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
                        (make-deque (dq-lenf dq)
                                    (dq-f dq)
                                    (stream-length hd.)
                                    (stream-reverse hd.)))
               (make-deque (stream-length hd) hd 0 stream-null))))

(define (ideque-take-while-right pred dq)
  ;(%check-ideque dq)
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

(define (ideque-drop-while pred dq)
  ;(%check-ideque dq)
  (receive (hd tl) (stream-span pred (dq-f dq))
           (if (stream-null? tl)
               (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
                        (make-deque (stream-length tl.) tl. 0 stream-null))
               (make-deque (stream-length tl) tl (dq-lenr dq) (dq-r dq)))))

(define (ideque-drop-while-right pred dq)
  ;(%check-ideque dq)
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(define (%idq-span-break op pred dq)
  ;(%check-ideque dq)
  (receive (head tail) (op pred (dq-f dq))
           (if (stream-null? tail)
               (receive (head. tail.) (op pred (stream-reverse (dq-r dq)))
                        (values (make-deque (stream-length head)
                                            head
                                            (stream-length head.)
                                            (stream-reverse head.))
                                (make-deque (stream-length tail.) tail. 0 stream-null)))
               (values
                (make-deque (stream-length head) head 0 stream-null)
                (make-deque (stream-length tail) tail (dq-lenr dq) (dq-r dq))))))

(define (ideque-span pred dq) (%idq-span-break stream-span pred dq))
(define (ideque-break pred dq) (%idq-span-break stream-break pred dq))

(define (ideque-any pred dq)
  ;(%check-ideque dq)
  (if (stream-null? (dq-r dq))
      (stream-any pred (dq-f dq))
      (or (stream-any pred (dq-f dq))
          (stream-any pred (stream-reverse (dq-r dq))))))

(define (ideque-every pred dq)
  ;(%check-ideque dq)
  (if (stream-null? (dq-r dq))
      (stream-every pred (dq-f dq))
      (and (stream-every pred (dq-f dq))
           (stream-every pred (stream-reverse (dq-r dq))))))

(define (ideque->list dq)
  ;(%check-ideque dq)
  (append (stream->list (dq-f dq))
          (stream->list (stream-reverse (dq-r dq)))))

(define (list->ideque lis)
  (make-deque (length lis) (list->stream lis) 0 stream-null))

(define (ideque->generator dq)
  ;(%check-ideque dq)
  (lambda ()
    (if (ideque-empty? dq)
        eof
        (let ((v (ideque-front dq)))
          (set! dq (ideque-remove-front dq))
          v))))

(define (generator->ideque gen)
  (list->ideque (generator->list gen)))

(define (in-ideque dq)
  (make-do-sequence
   (lambda ()
     (values
      ideque-front
      ideque-remove-front
      dq
      (lambda (dq) (not (ideque-empty? dq)))
      #f
      #f))))

(define (in-ideque-backwards dq)
  (make-do-sequence
   (lambda ()
     (values
      ideque-back
      ideque-remove-back
      dq
      (lambda (dq) (not (ideque-empty? dq)))
      #f
      #f))))


(module+ test
  (define-syntax-rule (test-group name exprs ...)
    (let () exprs ...))
  (define-syntax test
    (syntax-rules ()
      ((test expected actual)
       (check-equal? actual expected))
      ((test name expected actual)
       (test-equal? name actual expected))))
  (define-syntax-rule (test-assert test-case)
    (check-true test-case))
  (define-syntax-rule (test-error expr)
    (check-exn exn:fail? (lambda () expr)))
  
  (test-group "ideque/constructors"
              (test '() (ideque->list (ideque)))
              (test '() (ideque->list (list->ideque '())))
              (test '(1 2 3) (ideque->list (ideque 1 2 3)))
              (test '(4 5 6 7) (ideque->list (list->ideque '(4 5 6 7))))
              (test '(10 9 8 7 6 5 4 3 2 1)
                    (ideque->list (ideque-unfold zero? values (lambda (n) (- n 1)) 10)))
              (test '(1 2 3 4 5 6 7 8 9 10)
                    (ideque->list (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10)))
              (test '(0 2 4 6 8 10)
                    (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2)))))

              ;; corner cases
              (test '() (ideque->list
                         (ideque-unfold (lambda (n) #t) values (lambda (n) (+ n 1)) 0)))
              (test '() (ideque->list
                         (ideque-unfold-right (lambda (n) #t) values (lambda (n) (+ n 1)) 0)))
              (test '() (ideque->list (ideque-tabulate 0 values)))
              )
  
  (test-group "ideque/predicates"
              (test-assert (ideque? (ideque)))
              (test-assert (not (ideque? 1)))
              (test-assert (ideque-empty? (ideque)))
              (test-assert (not (ideque-empty? (ideque 1))))
              (test-assert (ideque= eq?))
              (test-assert (ideque= eq? (ideque 1)))
              (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B)))
              (test-assert (ideque= char-ci=? (ideque) (ideque)))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B #\c))))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A))))
              (test-assert (ideque= char-ci=? (ideque) (ideque) (ideque)))
              (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\a #\B)))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A) (ideque #\a #\B))))
              (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\A #\B #\c))))
              )
  
  (test-group "ideque/queue-operations"
              (test-error (ideque-front (ideque)))
              (test-error (ideque-back (ideque)))
              (test 1 (ideque-front (ideque 1 2 3)))
              (test 3 (ideque-back (ideque 1 2 3)))
              (test 2 (ideque-front (ideque-remove-front (ideque 1 2 3))))
              (test 2 (ideque-back (ideque-remove-back (ideque 1 2 3))))
              (test 1 (ideque-front (ideque-remove-back (ideque 1 2 3))))
              (test 3 (ideque-back (ideque-remove-front (ideque 1 2 3))))
              (test-assert (ideque-empty? (ideque-remove-front (ideque 1))))
              (test-assert (ideque-empty? (ideque-remove-back (ideque 1))))
              (test 0 (ideque-front (ideque-add-front (ideque 1 2 3) 0)))
              (test 0 (ideque-back (ideque-add-back (ideque 1 2 3) 0)))
              ;; loss of front ideque
              (let ((id (ideque #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (set! id (ideque-remove-front (ideque-add-back id 1)))
                (test #f (ideque-front (ideque-take-right id 12))))
              )
  
  (test-group "ideque/other-accessors"
              (define (check name ideque-op list-op n)
                (let* ((lis (iota n))
                       (dq (list->ideque lis)))
                  (for-each (lambda (i)
                              (test (string-append (symbol->string name)
                                                   " "
                                                   (number->string i))
                                    (receive xs (list-op lis i) xs)
                                    (receive xs (ideque-op dq i)
                                             (map ideque->list xs))))
                            lis)))
              (check 'ideque-take ideque-take take 7)
              (test '(1 2 3 4) (ideque->list (ideque-take (ideque 1 2 3 4) 4)))
              (test '(1 2 3 4) (ideque->list (ideque-take-right (ideque 1 2 3 4) 4)))
              (check 'ideque-drop ideque-drop drop 6)
              (test '() (ideque->list (ideque-drop (ideque 1 2 3 4) 4)))
              (test '() (ideque->list (ideque-drop-right (ideque 1 2 3 4) 4)))
              (check 'ideque-split-at ideque-split-at split-at 8)
              ;; out-of-range conditions
              (test-error (ideque->list (ideque-take (ideque 1 2 3 4 5 6 7) 10)))
              (test-error (ideque->list (ideque-take-right (ideque 1 2 3 4 5 6 7) 10)))
              (test-error (ideque-split-at (ideque 1 2 3 4 5 6 7) 10))
              
              (test '(3 2 1) (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2)))
              (test-error (ideque-ref (ideque 3 2 1) -1))
              (test-error (ideque-ref (ideque 3 2 1) 3))
              )
  
  (test-group "ideque/whole-ideque"
              (test 7 (ideque-length (ideque 1 2 3 4 5 6 7)))
              (test 0 (ideque-length (ideque)))
              (test '() (ideque->list (ideque-append)))
              (test '() (ideque->list (ideque-append (ideque) (ideque))))
              (test '(1 2 3 a b c d 5 6 7 8 9)
                    (ideque->list (ideque-append (ideque 1 2 3)
                                                 (ideque 'a 'b 'c 'd)
                                                 (ideque)
                                                 (ideque 5 6 7 8 9))))
              (test '() (ideque->list (ideque-reverse (ideque))))
              (test '(5 4 3 2 1) (ideque->list (ideque-reverse (ideque 1 2 3 4 5))))
              (test 0 (ideque-count odd? (ideque)))
              (test 3 (ideque-count odd? (ideque 1 2 3 4 5)))
              (test '((1 a) (2 b) (3 c))
                    (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e))))
              (test '((1 a x) (2 b y) (3 c z))
                    (ideque->list (ideque-zip (ideque 1 2 3 4 5)
                                              (ideque 'a 'b 'c 'd 'e)
                                              (ideque 'x 'y 'z))))
              (test '((1) (2) (3))
                    (ideque->list (ideque-zip (ideque 1 2 3))))
              (test '()
                    (ideque->list (ideque-zip (ideque 1 2 3) (ideque))))
            )
  
  (test-group "ideque/mapping"
              (test-assert (ideque-empty? (ideque-map list (ideque))))
              (test '(-1 -2 -3 -4 -5) (ideque->list (ideque-map - (ideque 1 2 3 4 5))))
              (test '(-1 -3 5 -8)
                    (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x)))
                                                     (ideque 1 3 'a -5 8))))
              (test '(5 4 3 2 1)
                    (let ((r '()))
                      (ideque-for-each (lambda (n) (set! r (cons n r)))
                                       (ideque 1 2 3 4 5))
                      r))
              (test '(1 2 3 4 5)
                    (let ((r '()))
                      (ideque-for-each-right (lambda (n) (set! r (cons n r)))
                                             (ideque 1 2 3 4 5))
                      r))
              (test '(5 4 3 2 1 . z)
                    (ideque-fold cons 'z (ideque 1 2 3 4 5)))
              (test '(1 2 3 4 5 . z)
                    (ideque-fold-right cons 'z (ideque 1 2 3 4 5)))
              (test '(a a b b c c)
                    (ideque->list (ideque-append-map (lambda (x) (list x x))
                                                     (ideque 'a 'b 'c))))
              )
  
  (test-group "ideque/filtering"
              (test '(1 3 5)
                    (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5))))
              (test '(2 4)
                    (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5))))
              (test '((1 3 5) (2 4))
                    (receive xs (ideque-partition odd? (ideque 1 2 3 4 5))
                             (map ideque->list xs)))
              )
  
  (test-group "ideque/searching"
              (test 3 (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
              (test 'boo (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
              (test #f (ideque-find number? (ideque 'a 'b 'c 'd)))
              (test 4 (ideque-find-right number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo)))
              (test 'boo (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo)))
              (test #f (ideque-find-right number? (ideque 'a 'b 'c 'd)))
              (test '(1 3 2)
                    (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(5 8 4 6 3 4 2)
                    (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(3 4 2)
                    (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '(1 3 2 5 8 4 6)
                    (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 5 8 4 6 3 4 2))))
              (test '()
                    (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                                     (ideque 5 8 4 6 3 4 2 9))))
              (test '()
                    (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                                     (ideque 1 4 3 2 3 4 2 1))))
              (test '()
                    (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                           (ideque 5 8 4 6 3 4 2 9))))
              (test '()
                    (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                           (ideque 1 3 2 4 3 2 3 2))))
              (test '((1 3 2) (5 8 4 6 3 4 2))
                    (receive xs (ideque-span (lambda (n) (< n 5))
                                             (ideque 1 3 2 5 8 4 6 3 4 2))
                             (map ideque->list xs)))
              (test '((5 8) (4 6 3 4 2 9))
                    (receive xs (ideque-break (lambda (n) (< n 5))
                                              (ideque 5 8 4 6 3 4 2 9))
                             (map ideque->list xs)))
              (test 3 (ideque-any (lambda (x) (and (number? x) x))
                                  (ideque 'a 3 'b 'c 4 'd 'e)))
              (test 5 (ideque-any (lambda (x) (and (number? x) x))
                                  (ideque 'a 'b 'c 'd 'e 5)))
              (test #f (ideque-any (lambda (x) (and (number? x) x))
                                   (ideque 'a 'b 'c 'd 'e)))
              (test 9 (ideque-every (lambda (x) (and (number? x) x))
                                    (ideque 1 5 3 2 9)))
              (test #f (ideque-every (lambda (x) (and (number? x) x))
                                     (ideque 1 5 'a 2 9)))
              ;; check if we won't see further once we found the result
              (test 1 (ideque-any (lambda (x) (and (odd? x) x))
                                  (ideque 2 1 'a 'b 'c 'd)))
              (test #f (ideque-every (lambda (x) (and (odd? x) x))
                                     (ideque 1 2 'a 'b 'c 'd)))
              
              (test '(1 2 3) (generator->list (ideque->generator (ideque 1 2 3))))
              (test '() (generator->list (ideque->generator (ideque))))
              (test '(1 2 3) (ideque->list (generator->ideque (generator 1 2 3))))
              (test '() (ideque->list (generator->ideque (generator))))
              )

  (test-group "sequences"
              (test '(1 2 3) (for/list ([elem (ideque 1 2 3)]) elem))
              (test '(3 2 1) (for/list ([elem (in-ideque-backwards (ideque 1 2 3))]) elem)))

  (test-group "equality"
              (check-true (equal? (ideque 1 2 3) (ideque 1 2 3)))
              (check-false (equal? (ideque 1 2 3) (ideque 1 2 #\c))))
  )
