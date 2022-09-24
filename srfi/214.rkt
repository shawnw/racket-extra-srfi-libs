#lang racket/base
;;; SRFI-214 flexvectors

(require (rename-in data/gvector
                    [gvector flexvector]
                    [gvector? flexvector?]
                    [gvector-remove-last! flexvector-remove-back!]
                    [gvector-count flexvector-length]
                    [list->gvector list->flexvector])
         racket/contract racket/undefined (for-syntax racket/base racket/symbol)
         "145.rkt" "190.rkt" "223.rkt"
         )

(define (flexvectorof contents/c #:flat-contract [flat-contract? #f])
  (let ([name (string->symbol (format "(flexvectorof ~A)" (contract-name contents/c)))]
        [fun (lambda (thing) (and (flexvector? thing) (flexvector-every contents/c thing)))])
    (if flat-contract?
        (make-flat-contract #:name name #:first-order fun)
        (make-contract #:name name #:first-order fun))))

(provide flexvector flexvector? flexvector-remove-back!
         flexvector-length list->flexvector
         (contract-out
          [make-flexvector (->* (exact-positive-integer?) (any/c) flexvector?)]
          [flexvector-ref (-> flexvector? exact-nonnegative-integer? any/c)]
          [flexvector-set! (-> flexvector? exact-nonnegative-integer? any/c any/c)]
          [flexvector-add! (-> flexvector exact-nonnegative-integer? any/c any/c ... void?)]
          [flexvector-add-back! (-> flexvector? any/c ... flexvector?)]
          [flexvector-add-all! (-> flexvector? exact-nonnegative-integer? list? void?)]
          [flexvector-remove! (-> flexvector? exact-nonnegative-integer? any/c)]
          [flexvector-remove-range! (-> flexvector exact-integer? exact-integer? void?)]
          [flexvector-clear! (-> flexvector? void?)]
          [flexvector-filter/index (-> (-> exact-nonnegative-integer? any/c any/c) flexvector? flexvector?)]
          [flexvector-filter/index! (-> (-> exact-nonnegative-integer? any/c any/c) flexvector? flexvector?)]
          [flexvector-copy (->* (flexvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) flexvector?)]
          [flexvector-copy! (->* (flexvector? exact-nonnegative-integer? flexvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) flexvector?)]
          [flexvector-unfold (-> procedure? procedure? procedure? any/c any/c ... flexvector?)]
          [flexvector-unfold-right (-> procedure? procedure? procedure? any/c any/c ... flexvector?)]
          [flexvector-fill! (->* (flexvector? any/c) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
          [flexvector-reverse-copy (->* (flexvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) flexvector?)]
          [flexvector-append! (-> flexvector? flexvector? ... flexvector?)]
          [flexvector-front (-> flexvector? any/c)]
          [flexvector-back (-> flexvector? any/c)]
          [flexvector-add-front! (-> flexvector? any/c any/c ... void?)]
          [flexvector-remove-front! (-> flexvector? void?)]
          [flexvector=? (-> (-> any/c any/c any/c) flexvector? ... boolean?)]
          [flexvector-fold (-> procedure? any/c flexvector? flexvector? ... any/c)]
          [flexvector-fold-right (-> procedure? any/c flexvector? flexvector? ... any/c)]
          [flexvector-for-each/index (-> (-> exact-nonnegative-integer? any/c any/c) flexvector? flexvector? ... void?)]
          [flexvector-for-each (-> (-> any/c any/c) flexvector? flexvector? ... void?)]
          [flexvector-map/index! (-> (-> exact-nonnegative-integer? any/c any/c) flexvector? flexvector? ... flexvector?)]
          [flexvector-map! (-> (-> any/c any/c) flexvector? flexvector? ... flexvector?)]
          [flexvector-map/index (-> (-> exact-nonnegative-integer? any/c any/c) flexvector? flexvector? ... flexvector?)]
          [flexvector-map (-> (-> any/c any/c) flexvector? flexvector? ... flexvector?)]
          [flexvector-append-map/index (-> (-> exact-nonnegative-integer? any/c any/c ... flexvector?) flexvector? flexvector? ... flexvector?)]
          [flexvector-append-map (-> (-> any/c any/c ... flexvector?) flexvector? flexvector? ... flexvector?)]
          [flexvector-filter! (-> (-> any/c any/c) flexvector? flexvector?)]
          [flexvector-filter (-> (-> any/c any/c) flexvector? flexvector?)]
          [flexvector-index (-> procedure? flexvector? flexvector? ... (or/c exact-nonnegative-integer? #f))]
          [flexvector-index-right (-> procedure? flexvector? flexvector? ... (or/c exact-nonnegative-integer? #f))]
          [flexvector-skip (-> procedure? flexvector? flexvector? ... (or/c exact-nonnegative-integer? #f))]
          [flexvector-skip-right (-> procedure? flexvector? flexvector? ... (or/c exact-nonnegative-integer? #f))]
          [flexvector-binary-search (->* (flexvector? any/c (-> any/c any/c exact-integer?)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
          [flexvector-any (-> procedure? flexvector? flexvector? ... any/c)]
          [flexvector-every (-> procedure? flexvector? flexvector? ... any/c)]
          [flexvector-swap! (-> flexvector? exact-nonnegative-integer? exact-nonnegative-integer? flexvector?)]
          [flexvector-reverse! (-> flexvector? flexvector?)]
          [flexvector-append (-> flexvector? flexvector? ... flexvector?)]
          [flexvector-concatenate (-> (listof flexvector?) flexvector?)]
          [flexvector-append-subvectors (-> list? flexvector?)]
          [flexvector-empty? (-> flexvector? boolean?)]
          [flexvector-count (-> procedure? flexvector? flexvector? ... exact-nonnegative-integer?)]
          [flexvector-cumulate (-> (-> any/c any/c any/c) any/c flexvector? flexvector?)]
          [flexvector-partition (-> (-> any/c any/c) flexvector? (values flexvector? flexvector?))]
          [flexvector->list (->* (flexvector?) (exact-integer? exact-integer?) list?)]
          [reverse-flexvector->list (->* (flexvector?) (exact-integer? exact-integer?) list?)]
          [reverse-list->flexvector (-> list? flexvector?)]
          [vector->flexvector (->* (vector?) (exact-integer? exact-integer?) flexvector?)]
          [flexvector->vector (->* (flexvector?) (exact-integer? exact-integer?) vector?)]
          [string->flexvector (->* (string?) (exact-integer? exact-integer?) (flexvectorof char?))]
          [flexvector->string (->* ((flexvectorof char?)) (exact-integer? exact-integer?) string?)]
          [generator->flexvector (-> (-> any/c) flexvector?)]
          [flexvector->generator (-> flexvector? (-> any/c))]
          ;; Extras
          [flexvectorof (->* (contract?) (#:flat-contract boolean?)  contract?)]
          [build-flexvector (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? any/c) flexvector?)]
          [bytes->flexvector (->* (bytes?) (exact-integer? exact-integer?) (flexvectorof byte?))]
          [flexvector->bytes (->* ((flexvectorof byte?)) (exact-integer? exact-integer?) bytes?)]
          [flexvector-bisect-left (case-> (-> flexvector? any/c (-> any/c any/c any/c) integer?)
                                          (-> flexvector? any/c (-> any/c any/c any/c) integer? integer? integer?))]
          [flexvector-bisect-right (case-> (-> flexvector? any/c (-> any/c any/c any/c) integer?)
                                           (-> flexvector? any/c (-> any/c any/c any/c) integer? integer? integer?))]

          ))
(module+ test (require rackunit))

;; Fundamental building blocks directly using gvector functions or otherwise different from the reference version


;; Boilerplate range checking macros

(define-syntax (clamp-ranges stx)
  (syntax-case stx ()
      [(_ (name fv start end) . body)
       #'(let ([start (max start 0)]
               [end (min end (flexvector-length fv))])
           (when (< end start)
             (raise-range-error (quote name) "flexvector" "ending " end fv start (flexvector-length fv) 0))
           . body)]))

(define-syntax (define/clamped stx)
  (syntax-case stx ()
    [(_ (name v start end #:length v-length #:type type-name) . body)
     #`(define (name v [start 0] [end (v-length v)])
         (let ([start (max start 0)]
               [end (min end (v-length v))])
           (when (< end start)
             (raise-range-error (quote name) #,(let ([tn (syntax->datum #'type-name)])
                                                 (cond ((symbol? tn) (symbol->immutable-string tn))
                                                       ((string? tn) (string->immutable-string tn))))
                                "ending " end v start (v-length v) 0))
           . body))]
    [(_ (name fv start end) . body)
     #'(define/clamped (name fv start end #:length flexvector-length #:type "flexvector") . body)]))

(define (build-flexvector len proc)
  (let loop ([fv (make-gvector #:capacity len)]
             [i 0])
    (cond
      ((= i len) fv)
      (else
       (gvector-add! fv (proc i))
       (loop fv (+ i 1))))))

(define (make-flexvector len [fill undefined])
  (let loop ([fv (make-gvector #:capacity len)]
             [i 0])
    (cond
      ((= i len) fv)
      (else
       (gvector-add! fv fill)
       (loop fv (+ i 1))))))

(define (flexvector-ref fv i)
  (gvector-ref fv i (lambda () (raise-range-error 'flexvector-ref "flexvector" "" i fv 0 (- (flexvector-length fv) 1)))))

(define (flexvector-set! fv i val)
  (when (or (< i 0) (> i (flexvector-length fv)))
    (raise-range-error 'flexvector-set! "flexvector" "" i fv 0 (flexvector-length fv)))
  (let ([old (gvector-ref fv i undefined)])
    (gvector-set! fv i val)
    old))

(define flexvector-add!
  (case-lambda
    ((fv i x) (gvector-insert! fv i x))
    ((fv i . xs) (flexvector-add-all! fv i xs))))

(define (flexvector-add-back! fv . elems)
  (apply gvector-add! fv elems)
  fv)

(define (flexvector-add-all! fv i xs)
  (when (or (< i 0) (> i (flexvector-length fv)))
    (raise-range-error 'flexvector-add-all! "" i fv 0 (flexvector-length fv)))
  (let loop ([xs xs]
             [i i])
    (cond
      ((null? xs) fv)
      (else
       (gvector-insert! fv i (car xs))
       (loop (cdr xs) (+ i 1))))))

(define (flexvector-remove! fv i)
  (let ([old (gvector-ref fv i (lambda () (raise-range-error 'flexvector-remove! "flexvector" "" i fv 0 (- (flexvector-length fv) 1))))])
    (gvector-remove! fv i)
    old))

(define/clamped (flexvector-remove-range! fv start end)
  (let loop ([i (- end 1)])
    (when (>= i start)
      (gvector-remove! fv i)
      (loop (- i 1))))
  fv)

(define (flexvector-clear! fv)
  (let loop ()
    (when (> (flexvector-length fv) 0)
      (flexvector-remove-back! fv)
      (loop)))
  fv)

(define (flexvector-filter/index pred? fv)
  (for/gvector ([(elem i) (in-indexed (in-gvector fv))]
                #:when (pred? i elem))
    elem))

(define (flexvector-filter pred? fv)
  (for/gvector ([elem (in-gvector fv)]
                #:when (pred? elem))
    elem))

(define (flexvector-filter/index! pred? fv)
  (let loop ([i (- (flexvector-length fv) 1)])
    (cond
      ((< i 0) fv)
      ((pred? i (gvector-ref fv i))
       (loop (- i 1)))
      (else
       (gvector-remove! fv i)
       (loop (- i 1))))))

(define (flexvector-filter! pred? fv)
  (let loop ([i (- (flexvector-length fv) 1)])
    (cond
      ((< i 0) fv)
      ((pred? (gvector-ref fv i))
       (loop (- i 1)))
      (else
       (gvector-remove! fv i)
       (loop (- i 1))))))

(define/clamped (flexvector-copy fv start end)
  (let ([new-fv (make-gvector #:capacity (- end start))])
    (for ([i (in-range start end)])
      (gvector-add! new-fv (gvector-ref fv i)))
    new-fv))

(define (flexvector-copy! to at from [start 0] [end (flexvector-length from)])
  (clamp-ranges ('flexvector-copy! from start end)
                (when (or (< at 0) (> at (flexvector-length to)))
                  (raise-range-error 'flexvector-copy! "flexvector" "" at to 0 (flexvector-length to)))
                (if (eq? from to)
                    (let ([tmp (flexvector-copy from start end)]) ;; Possibly overlapping copy
                      (let loop ([i 0]
                                 [at at])
                        (when (< i (flexvector-length tmp))
                          (gvector-set! to at (gvector-ref tmp i))
                          (loop (+ i 1) (+ at 1)))))
                    (let loop ([i start] ;; Fast path
                               [at at])
                      (when (< i end)
                        (gvector-set! to at (gvector-ref from i))
                        (loop (+ i 1) (+ at 1)))))
                to))

(define (flexvector-concatenate fvs)
  (for*/gvector ([fv (in-list fvs)]
                 [elem (in-gvector fv)])
    elem))

(define (flexvector-append . fvs)
  (flexvector-concatenate fvs))

(define (min-length fv fvs)
  (foldl (lambda (fv cur-min) (min (flexvector-length fv) cur-min)) (flexvector-length fv) fvs))

(define (get-values i fvs)
  (map (lambda (fv) (gvector-ref fv i)) fvs))

(define (flexvector-map/index proc fv . fvs)
  (for/gvector ([i (in-range (apply min (flexvector-length fv) (map flexvector-length fvs)))])
    (apply proc i (gvector-ref fv i) (get-values i fvs))))

(define (flexvector-map proc fv . fvs)
  (for/gvector ([i (in-range (min-length fv fvs))])
    (apply proc (gvector-ref fv i) (get-values i fvs))))

(define (flexvector-for-each/index proc fv . fvs)
  (for ([i (in-range (min-length fv fvs))])
    (apply proc i (gvector-ref fv i) (get-values i fvs))))

(define (flexvector-for-each proc fv . fvs)
  (for ([i (in-range (min-length fv fvs))])
    (apply proc (gvector-ref fv i) (get-values i fvs))))

(define/clamped (flexvector->list fv start end)
  (for/list ([i (in-range start end)])
    (gvector-ref fv i)))

(define/clamped (vector->flexvector v start end #:length vector-length #:type vector)
    (for/gvector ([elem (in-vector v start end)]) elem))

(define/clamped (flexvector->vector fv start end)
  (build-vector (- end start) (lambda (i) (flexvector-ref fv (+ i start)))))

(define/clamped (string->flexvector s start end #:length string-length #:type string)
    (for/gvector ([ch (in-string s start end)]) ch))

(define/clamped (flexvector->string fv start end)
  (build-string (- end start) (lambda (i) (gvector-ref fv (+ i start)))))

(define-values (flexvector-bisect-left flexvector-bisect-right)
  (bisection gvector-ref (lambda (fv) (values 0 (flexvector-length fv)))))

(define/clamped (bytes->flexvector bs start end #:length bytes-length #:type bytes)
  (for/gvector ([b (in-bytes bs start end)]) b))

(define/clamped (flexvector->bytes fv start end)
  (let ([bs (make-bytes (- end start))])
    (let loop ([start start]
               [i 0])
      (cond
        ((= start end)
         bs)
        (else
         (bytes-set! bs i (gvector-ref fv start))
         (loop (+ start 1) (+ i 1)))))))

(define-coroutine-generator (flexvector->generator fv)
  (for ([elem (in-gvector fv)])
    (yield elem)))

(define (generator->flexvector g)
  (for/gvector ([elem (in-producer g eof)]) elem))

;; Other stuff lifted from the reference implementation

;; © Adam Nelson 2020-2021.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(define flexvector-unfold
  (case-lambda
    ((p f g seed)
      (define fv (flexvector))
      #;(assume (procedure? p))
      #;(assume (procedure? f))
      #;(assume (procedure? g))
      (do ((seed seed (g seed))) ((p seed) fv)
        (flexvector-add-back! fv (f seed))))
    ((p f g . seeds)
      (define fv (flexvector))
      #;(assume (procedure? p))
      #;(assume (procedure? f))
      #;(assume (procedure? g))
      (do ((seeds seeds (call-with-values (lambda () (apply g seeds)) list)))
          ((apply p seeds) fv)
        (flexvector-add-back! fv (apply f seeds))))))

(define (flexvector-unfold-right . args)
  (define fv (apply flexvector-unfold args))
  (flexvector-reverse! fv)
  fv)

(define flexvector-fill!
  (case-lambda
    ((fv fill)
      (flexvector-fill! fv fill 0 (flexvector-length fv)))
    ((fv fill start)
      (flexvector-fill! fv fill start (flexvector-length fv)))
    ((fv fill start end)
      (let ((actual-end (min end (flexvector-length fv))))
        (do ((i (max 0 start) (+ i 1)))
            ((>= i actual-end))
          (flexvector-set! fv i fill))))))

(define (flexvector-reverse-copy . args)
  (define fv (apply flexvector-copy args))
  (flexvector-reverse! fv)
  fv)

(define flexvector-reverse-copy!
  (case-lambda
    ((to at from)
      #;(assume (flexvector? from))
      (flexvector-reverse-copy! to at from 0 (flexvector-length from)))
    ((to at from start)
      #;(assume (flexvector? from))
      (flexvector-reverse-copy! to at from start (flexvector-length from)))
    ((to at from start end)
      (flexvector-copy! to at from start end)
      (flexvector-reverse! to at (+ at (- end start))))))

(define (flexvector-append! fv . fvs)
  #;(assume (flexvector? fv))
  #;(assume (every flexvector? fvs))
  (for-each
    (lambda (fv2) (flexvector-copy! fv (flexvector-length fv) fv2))
    fvs)
  fv)

(define (flexvector-front fv)
  #;(assume (flexvector? fv))
  #;(assume (not (flexvector-empty? fv)))
  (when (flexvector-empty? fv)
    (raise-argument-error 'flexvector-front "(not/c flexvector-empty?)" fv))
  (flexvector-ref fv 0))

(define (flexvector-back fv)
  #;(assume (flexvector? fv))
  #;(assume (not (flexvector-empty? fv)))
  (when (flexvector-empty? fv)
    (raise-argument-error 'flexvector-back "(not/c flexvector-empty?)" fv))
  (flexvector-ref fv (- (flexvector-length fv) 1)))

(define flexvector-add-front!
  (case-lambda
    ((fv x) (flexvector-add! fv 0 x))
    ((fv . xs) (apply flexvector-add! fv 0 xs))))

(define (flexvector-remove-front! fv)
  #;(assume (flexvector? fv))
  #;(assume (not (flexvector-empty? fv)))
  (when (flexvector-empty? fv)
    (raise-argument-error 'flexvector-remove-front! "(not/c flexvector-empty?)" fv))
  (flexvector-remove! fv 0))

(define (flexvector=? eq . o)
  (cond
    ((null? o) #t)
    ((null? (cdr o)) #t)
    (else
      (and (let* ((fv1 (car o))
                  (fv2 (cadr o))
                  (len (flexvector-length fv1)))
             (and (= len (flexvector-length fv2))
                  (let lp ((i 0))
                    (or (>= i len)
                        (and (eq (flexvector-ref fv1 i) (flexvector-ref fv2 i))
                             (lp (+ i 1)))))))
           (apply flexvector=? eq (cdr o))))))

(define (flexvector-fold kons knil fv1 . o)
  #;(assume (procedure? kons))
  #;(assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (if (null? o)
        (let lp ((i 0) (acc knil))
          (if (>= i len) acc (lp (+ i 1) (kons acc (flexvector-ref fv1 i)))))
        (let lp ((i 0) (acc knil))
          (if (>= i len)
              acc
              (lp (+ i 1)
                  (apply kons acc (flexvector-ref fv1 i)
                         (map (lambda (fv) (flexvector-ref fv i)) o))))))))

(define (flexvector-fold-right kons knil fv1 . o)
  #;(assume (procedure? kons))
  #;(assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (if (null? o)
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i) acc (lp (- i 1) (kons acc (flexvector-ref fv1 i)))))
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i)
              acc
              (lp (- i 1)
                  (apply kons acc (flexvector-ref fv1 i)
                         (map (lambda (fv) (flexvector-ref fv i)) o))))))))

(define flexvector-map/index!
  (case-lambda
    ((proc fv)
      #;(assume (procedure? proc))
      #;(assume (flexvector? fv))
      (flexvector-for-each/index
        (lambda (i x) (flexvector-set! fv i (proc i x)))
        fv)
      fv)
    ((proc fv . fvs)
      #;(assume (procedure? proc))
      #;(assume (flexvector? fv))
      (apply flexvector-for-each/index
        (lambda (i . xs) (flexvector-set! fv i (apply proc i xs)))
        fv
        fvs)
      fv)))

(define flexvector-map!
  (case-lambda
    ((proc fv)
      #;(assume (procedure? proc))
      (flexvector-map/index! (lambda (i x) (proc x)) fv))
    ((proc . fvs)
      #;(assume (procedure? proc))
      (apply flexvector-map/index! (lambda (i . xs) (apply proc xs)) fvs))))

(define (flexvector-append-map/index proc fv . fvs)
  (define out (flexvector))
  (flexvector-for-each
    (lambda (x) (flexvector-append! out x))
    (apply flexvector-map/index proc fv fvs))
  out)

(define (flexvector-append-map proc fv . fvs)
  (define out (flexvector))
  (flexvector-for-each
    (lambda (x) (flexvector-append! out x))
    (apply flexvector-map proc fv fvs))
  out)

(define (flexvector-index pred? fv1 . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (let lp ((i 0))
      (and (< i len)
           (if (apply pred?
                      (flexvector-ref fv1 i)
                      (map (lambda (fv) (flexvector-ref fv i)) o))
               i
               (lp (+ i 1)))))))

(define (flexvector-index-right pred? fv1 . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (let lp ((i (- len 1)))
      (and (>= i 0)
           (if (apply pred?
                      (flexvector-ref fv1 i)
                      (map (lambda (fv) (flexvector-ref fv i)) o))
               i
               (lp (- i 1)))))))

(define (complement f)
  (lambda args (not (apply f args))))

(define (flexvector-skip pred? fv1 . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv1))
  (apply flexvector-index (complement pred?) fv1 o))

(define (flexvector-skip-right pred? fv1 . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv1))
  (apply flexvector-index-right (complement pred?) fv1 o))

(define flexvector-binary-search
  (case-lambda
    ((fv value cmp)
      (flexvector-binary-search fv value cmp 0 (flexvector-length fv)))
    ((fv value cmp start)
      (flexvector-binary-search fv value cmp start (flexvector-length fv)))
    ((fv value cmp start end)
      #;(assume (flexvector? fv))
      #;(assume (procedure? cmp))
      #;(assume (integer? start))
      #;(assume (integer? end))
      #;(assume (<= start end))
      (unless (and (<= start end) (<= end (flexvector-length fv)))
        (raise-range-error 'flexvector-binary-search "flexvector" "ending " end fv start (flexvector-length fv) 0))
      (let lp ((lo (max start 0))
               (hi (- (min end (flexvector-length fv)) 1)))
        (and (<= lo hi)
             (let* ((mid (quotient (+ lo hi) 2))
                    (x (flexvector-ref fv mid))
                    (y (cmp value x)))
               (cond
                 ((< y 0) (lp lo (- mid 1)))
                 ((> y 0) (lp (+ mid 1) hi))
                 (else mid))))))))

(define (flexvector-any pred? fv . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv))
  (let ((len (apply min (flexvector-length fv) (map flexvector-length o))))
    (let lp ((i 0))
      (and (< i len)
           (or (apply pred?
                      (flexvector-ref fv i)
                      (map (lambda (v) (flexvector-ref v i)) o))
               (lp (+ i 1)))))))

(define (flexvector-every pred? fv . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv))
  (let ((len (apply min (flexvector-length fv) (map flexvector-length o))))
    (or (zero? len)
      (let lp ((i 0))
        (let ((x (apply pred?
                        (flexvector-ref fv i)
                        (map (lambda (v) (flexvector-ref v i)) o))))
          (if (= i (- len 1))
              x
              (and x (lp (+ i 1)))))))))

(define (flexvector-swap! fv i j)
  #;(assume (flexvector? fv))
  #;(assume (integer? i))
  #;(assume (integer? j))
  (let ((tmp (flexvector-ref fv i)))
    (flexvector-set! fv i (flexvector-ref fv j))
    (flexvector-set! fv j tmp)))

(define (flexvector-reverse! fv . o)
  #;(assume (flexvector? fv))
  (let lp ((left (if (pair? o) (car o) 0))
           (right (- (if (and (pair? o) (pair? (cdr o)))
                         (cadr o)
                         (flexvector-length fv))
                     1)))
    (cond
      ((>= left right) (void))
      (else
        (flexvector-swap! fv left right)
        (lp (+ left 1) (- right 1))))))

(define (flexvector-append-subvectors . o)
  (let lp ((ls o) (vecs '()))
    (if (null? ls)
        (flexvector-concatenate (reverse vecs))
        (lp (cdr (cddr ls))
            (cons (flexvector-copy (car ls) (cadr ls) (car (cddr ls))) vecs)))))

(define (flexvector-empty? fv)
  #;(assume (flexvector? fv))
  (zero? (flexvector-length fv)))

(define (flexvector-count pred? fv1 . o)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv1))
  (apply flexvector-fold
         (lambda (count . x) (+ count (if (apply pred? x) 1 0)))
         0
         fv1 o))

(define (flexvector-cumulate f knil fv)
  #;(assume (procedure? f))
  #;(assume (flexvector? fv))
  (let* ((len (flexvector-length fv))
         (res (make-vector len)))
    (let lp ((i 0) (acc knil))
      (if (>= i len)
          (vector->flexvector res)
          (let ((acc (f acc (flexvector-ref fv i))))
            (vector-set! res i acc)
            (lp (+ i 1) acc))))))

(define (flexvector-partition pred? fv)
  #;(assume (procedure? pred?))
  #;(assume (flexvector? fv))
  (let ((left (flexvector)) (right (flexvector)))
    (flexvector-for-each
      (lambda (x) (flexvector-add-back! (if (pred? x) left right) x))
      fv)
    (values left right)))

(define (reverse-flexvector->list fv . o)
  #;(assume (flexvector? fv))
  (flexvector->list (apply flexvector-reverse-copy fv o)))

(define (reverse-list->flexvector ls)
  #;(assume (list? ls))
  (let ((fv (list->flexvector ls)))
    (flexvector-reverse! fv)
    fv))

(module+ test
  (define-syntax-rule (test-equal name expected tst)
    (test-equal? name tst expected))

  (test-equal "flexvector?" #t (flexvector? (flexvector)))
  (test-equal "flexvector-length" 3 (flexvector-length (make-flexvector 3 #f)))
  (test-equal "flexvector" 3 (flexvector-length (flexvector 1 2 3)))

  (let ((fv (flexvector 'a 'b 'c)))
    (test-equal "flexvector-ref" 'b (flexvector-ref fv 1))
    (test-equal "flexvector-front" 'a (flexvector-front fv))
    (test-equal "flexvector-back" 'c (flexvector-back fv))
    (test-equal "flexvector-set! return" 'b (flexvector-set! fv 1 'd))
    (test-equal "flexvector-set! mutate" 'd (flexvector-ref fv 1))
    (test-equal "flexvector-add-back! return" fv (flexvector-add-back! fv 'e))
    (test-equal "flexvector-add-back! mutate" '(4 . e)
                (cons (flexvector-length fv)
                      (flexvector-ref fv (- (flexvector-length fv) 1))))
    (test-equal "flexvector-remove! return" 'd (flexvector-remove! fv 1))
    (test-equal "flexvector-remove! mutate" '(3 . c)
                (cons (flexvector-length fv)
                      (flexvector-ref fv 1)))
    (test-equal "flexvector-clear! return" fv (flexvector-clear! fv))
    (test-equal "flexvector-clear! mutate" 0 (flexvector-length fv))
    (test-equal "flexvector-empty?" #t (flexvector-empty? fv)))

  (test-equal "flexvector=? same symbols" #t
              (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
  (test-equal "flexvector=? different symbols" #f
              (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))
  (test-equal "flexvector=? different lengths" #f
              (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))
  (test-equal "flexvector=? same numbers" #t
              (flexvector=? = (flexvector 1 2 3 4) (flexvector 1 2 3 4)))
  (test-equal "flexvector=? 0 arguments" #t
              (flexvector=? eq?))
  (test-equal "flexvector=? 1 argument" #t
              (flexvector=? eq? (flexvector 'a)))

  (test-equal "make-flexvector" #(a a a) (flexvector->vector (make-flexvector 3 'a)))

  (test-equal "flexvector-unfold"
              #(1 4 9 16 25 36 49 64 81 100)
              (flexvector->vector
               (flexvector-unfold (lambda (x) (> x 10))
                                  (lambda (x) (* x x))
                                  (lambda (x) (+ x 1))
                                  1)))
  (test-equal "flexvector-unfold-right"
              #(100 81 64 49 36 25 16 9 4 1)
              (flexvector->vector
               (flexvector-unfold-right (lambda (x) (> x 10))
                                        (lambda (x) (* x x))
                                        (lambda (x) (+ x 1))
                                        1)))


  (test-equal "string->flexvector" #(#\a #\b #\c)
              (flexvector->vector (string->flexvector "abc")))
  (test-equal "flexvector->string" "abc" (flexvector->string (flexvector #\a #\b #\c)))

  (define genlist '(a b c))
  (define (mock-generator)
    (if (pair? genlist)
        (let ((value (car genlist)))
          (set! genlist (cdr genlist))
          value)
        eof))

  (test-equal "generator->flexvector" #(a b c)
              (flexvector->vector (generator->flexvector mock-generator)))
  (test-equal "flexvector->generator" '(a b c #t)
              (let* ((gen (flexvector->generator (flexvector 'a 'b 'c)))
                     (one (gen))
                     (two (gen))
                     (three (gen))
                     (four (eof-object? (gen))))
                (list one two three four)))

  ; Nondestructive operations on one vector
  (let ((fv (flexvector 10 20 30)))
    (test-equal "flexvector->vector" #(10 20 30) (flexvector->vector fv))
    (test-equal "flexvector->list" '(10 20 30) (flexvector->list fv))
    (test-equal "reverse-flexvector->list" '(30 20 10) (reverse-flexvector->list fv))
    (test-equal "flexvector-copy" #t
                (let ((copy (flexvector-copy fv)))
                  (and (= (flexvector-length fv) (flexvector-length copy))
                       (not (eq? fv copy)))))
    (test-equal "flexvector-reverse-copy" #(30 20 10)
                (flexvector->vector (flexvector-reverse-copy fv)))
    (test-equal "flexvector-copy start" #(20 30)
                (flexvector->vector (flexvector-copy fv 1)))
    (test-equal "flexvector-copy start end" #(20)
                (flexvector->vector (flexvector-copy fv 1 2)))
    (test-equal "flexvector-for-each" '(30 20 10)
                (let ((res '()))
                  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
                  res))
    (test-equal "flexvector-for-each/index" '(34 22 10)
                (let ((res '()))
                  (flexvector-for-each/index
                   (lambda (i x) (set! res (cons (+ x (* i 2)) res)))
                   fv)
                  res))
    (test-equal "flexvector-map" #(100 200 300)
                (flexvector->vector (flexvector-map (lambda (x) (* x 10)) fv)))
    (test-equal "flexvector-map/index" #(10 22 34)
                (flexvector->vector (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv)))
    (test-equal "flexvector-append-map" #(10 100 20 200 30 300)
                (flexvector->vector
                 (flexvector-append-map (lambda (x) (flexvector x (* x 10))) fv)))
    (test-equal "flexvector-append-map/index" #(0 10 10 1 20 22 2 30 34)
                (flexvector->vector
                 (flexvector-append-map/index
                  (lambda (i x) (flexvector i x (+ x (* i 2))))
                  fv)))
    (test-equal "flexvector-filter" #(10)
                (flexvector->vector (flexvector-filter (lambda (x) (< x 15)) fv)))
    (test-equal "flexvector-filter/index" #(10 30)
                (flexvector->vector (flexvector-filter/index (lambda (i x) (not (= i 1))) fv)))
    (test-equal "flexvector-fold" '(30 20 10)
                (flexvector-fold (lambda (x y) (cons y x)) '() fv))
    (test-equal "flexvector-fold-right" '(10 20 30)
                (flexvector-fold-right (lambda (x y) (cons y x)) '() fv))
    (test-equal "flexvector-count" 2
                (flexvector-count (lambda (x) (< x 25)) fv))
    (test-equal "flexvector-cumulate" #(3 4 8 9 14 23 25 30 36)
                (flexvector->vector
                 (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6))))
    (test-equal "flexvector-any" '(#t . #f)
                (cons (flexvector-any (lambda (x) (= x 20)) fv)
                      (flexvector-any (lambda (x) (= x 21)) fv)))
    (test-equal "flexvector-every" '(#t . #f)
                (cons (flexvector-every (lambda (x) (< x 40)) fv)
                      (flexvector-every (lambda (x) (< x 30)) fv)))
    (test-equal "flexvector-index" 1
                (flexvector-index (lambda (x) (> x 10)) fv))
    (test-equal "flexvector-index-right" 2
                (flexvector-index-right (lambda (x) (> x 10)) fv))
    (test-equal "flexvector-skip" 1
                (flexvector-skip (lambda (x) (< x 20)) fv))
    (test-equal "flexvector-skip-right" 0
                (flexvector-skip-right (lambda (x) (> x 10)) fv))
    (test-equal "flexvector-partition" '(#(10 20) #(30))
                (call-with-values
                 (lambda () (flexvector-partition (lambda (x) (< x 25)) fv))
                 (lambda vs (map flexvector->vector vs)))))

  (let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
        (cmp (lambda (char1 char2)
               (cond ((char<? char1 char2) -1)
                     ((char=? char1 char2) 0)
                     (else 1)))))
    (test-equal "flexvector-binary-search" 3
                (flexvector-binary-search fv #\d cmp))
    (test-equal "flexvector-binary-search first" 0
                (flexvector-binary-search fv #\a cmp))
    (test-equal "flexvector-binary-search last" 9
                (flexvector-binary-search fv #\j cmp))
    (test-equal "flexvector-binary-search not found" #f
                (flexvector-binary-search fv #\k cmp))

    (test-equal "flexvector-binary-search in range" 5
                (flexvector-binary-search fv #\f cmp 2 6))
    (test-equal "flexvector-binary-search out of range" #f
                (flexvector-binary-search fv #\f cmp 1 5)))

  ; Nondestructive operations on multiple vectors
  (test-equal "flexvector-append" #(10 20 30 40 50 60)
              (flexvector->vector
               (flexvector-append (flexvector 10 20)
                                  (flexvector)
                                  (flexvector 30 40)
                                  (flexvector 50 60))))
  (test-equal "flexvector-concatenate" #(10 20 30 40 50 60)
              (flexvector->vector
               (flexvector-concatenate
                (list (flexvector 10 20)
                      (flexvector)
                      (flexvector 30 40)
                      (flexvector 50 60)))))
  (test-equal "flexvector-append-subvectors" #(a b h i)
              (flexvector->vector
               (flexvector-append-subvectors
                (flexvector 'a 'b 'c 'd 'e) 0 2
                (flexvector 'f 'g 'h 'i 'j) 2 4)))


  ; Destructive operations on one vector
  (define-syntax mutate-as
    (syntax-rules ()
      ((_ name vec expr)
       (let ((name (vector->flexvector vec)))
         expr
         (flexvector->vector name)))))

  (test-equal "flexvector-add! empty" '#(foo)
              (mutate-as x '#() (flexvector-add! x 0 'foo)))
  (test-equal "flexvector-add! empty multiple" '#(foo bar baz)
              (mutate-as x '#() (flexvector-add! x 0 'foo 'bar 'baz)))
  (test-equal "flexvector-add! start" '#(foo bar baz)
              (mutate-as x '#(bar baz) (flexvector-add! x 0 'foo)))
  (test-equal "flexvector-add! start multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(qux quux) (flexvector-add! x 0 'foo 'bar 'baz)))
  (test-equal "flexvector-add! middle" '#(foo bar baz)
              (mutate-as x '#(foo baz) (flexvector-add! x 1 'bar)))
  (test-equal "flexvector-add! middle multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(foo quux) (flexvector-add! x 1 'bar 'baz 'qux)))
  (test-equal "flexvector-add! end" '#(foo bar baz)
              (mutate-as x '#(foo bar) (flexvector-add! x 2 'baz)))
  (test-equal "flexvector-add! end multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(foo bar) (flexvector-add! x 2 'baz 'qux 'quux)))

  (test-equal "flexvector-add-all!" '#(foo bar baz qux)
              (mutate-as x '#(foo qux) (flexvector-add-all! x 1 '(bar baz))))

  (test-equal "flexvector-add-front! empty" '#(foo)
              (mutate-as x '#() (flexvector-add-front! x 'foo)))
  (test-equal "flexvector-add-front! empty multiple" '#(foo bar baz)
              (mutate-as x '#() (flexvector-add-front! x 'foo 'bar 'baz)))
  (test-equal "flexvector-add-front!" '#(foo bar baz)
              (mutate-as x '#(bar baz) (flexvector-add-front! x 'foo)))
  (test-equal "flexvector-add-front! multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(qux quux) (flexvector-add-front! x 'foo 'bar 'baz)))

  (test-equal "flexvector-add-back! empty" '#(foo)
              (mutate-as x '#() (flexvector-add-back! x 'foo)))
  (test-equal "flexvector-add-back! empty multiple" '#(foo bar baz)
              (mutate-as x '#() (flexvector-add-back! x 'foo 'bar 'baz)))
  (test-equal "flexvector-add-back!" '#(foo bar baz)
              (mutate-as x '#(foo bar) (flexvector-add-back! x 'baz)))
  (test-equal "flexvector-add-back! multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(foo bar) (flexvector-add-back! x 'baz 'qux 'quux)))

  (test-equal "flexvector-append!" '#(foo bar baz qux)
              (mutate-as x '#(foo bar) (flexvector-append! x (flexvector 'baz 'qux))))
  (test-equal "flexvector-append! multiple" '#(foo bar baz qux quux)
              (mutate-as x '#(foo bar) (flexvector-append! x (flexvector 'baz 'qux) (flexvector 'quux))))

  (test-equal "flexvector-remove!" '#(foo baz)
              (mutate-as x '#(foo bar baz) (flexvector-remove! x 1)))
  (test-equal "flexvector-remove! only" '#()
              (mutate-as x '#(foo) (flexvector-remove! x 0)))

  (test-equal "flexvector-remove-front!" '#(bar baz)
              (mutate-as x '#(foo bar baz) (flexvector-remove-front! x)))
  (test-equal "flexvector-remove-front! only" '#()
              (mutate-as x '#(foo) (flexvector-remove-front! x)))

  (test-equal "flexvector-remove-back!" '#(foo bar)
              (mutate-as x '#(foo bar baz) (flexvector-remove-back! x)))
  (test-equal "flexvector-remove-back! only" '#()
              (mutate-as x '#(foo) (flexvector-remove-back! x)))

  (test-equal "flexvector-remove-range!" '#(a e f)
              (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 1 4)))
  (test-equal "flexvector-remove-range! empty range" '#(a b c d e f)
              (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 1 1)))
  (test-equal "flexvector-remove-range! overflow left" '#(e f)
              (mutate-as x '#(a b c d e f) (flexvector-remove-range! x -1 4)))
  (test-equal "flexvector-remove-range! overflow right" '#(a b)
              (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 2 10)))

  (test-equal "flexvector-map!" '#(100 200 300)
              (mutate-as fv '#(10 20 30) (flexvector-map! (lambda (x) (* x 10)) fv)))
  (test-equal "flexvector-map/index!" '#(10 22 34)
              (mutate-as fv '#(10 20 30) (flexvector-map/index! (lambda (i x) (+ x (* i 2))) fv)))
  (test-equal "flexvector-filter!" '#(10)
              (mutate-as fv '#(10 20 30) (flexvector-filter! (lambda (x) (< x 15)) fv)))
  (test-equal "flexvector-filter/index!" '#(10 30)
              (mutate-as fv '#(10 20 30) (flexvector-filter/index! (lambda (i x) (not (= i 1))) fv)))

  (test-equal "flexvector-swap!" #(10 30 20)
              (mutate-as fv '#(10 20 30) (flexvector-swap! fv 1 2)))
  (test-equal "flexvector-reverse!" #(30 20 10)
              (mutate-as fv '#(10 20 30) (flexvector-reverse! fv)))

  (test-equal "flexvector-copy!" #(1 20 30 40 5)
              (mutate-as fv '#(1 2 3 4 5) (flexvector-copy! fv 1 (flexvector 20 30 40))))
  (test-equal "flexvector-copy! bounded" #(1 20 30 40 5)
              (mutate-as fv '#(1 2 3 4 5) (flexvector-copy! fv 1 (flexvector 10 20 30 40 50) 1 4)))
  (test-equal "flexvector-copy! overflow" #(1 2 30 40 50)
              (mutate-as fv '#(1 2 3) (flexvector-copy! fv 2 (flexvector 30 40 50))))
  (test-equal "flexvector-reverse-copy!" #(1 40 30 20 5)
              (mutate-as fv '#(1 2 3 4 5) (flexvector-reverse-copy! fv 1 (flexvector 20 30 40))))
  (test-equal "flexvector-reverse-copy! bounded" #(1 40 30 20 5)
              (mutate-as fv '#(1 2 3 4 5) (flexvector-reverse-copy! fv 1 (flexvector 10 20 30 40 50) 1 4)))
  (test-equal "flexvector-reverse-copy! overflow" #(1 2 50 40 30)
              (mutate-as fv '#(1 2 3) (flexvector-reverse-copy! fv 2 (flexvector 30 40 50))))

  (test-equal "flexvector-fill!" '#(foo foo foo)
              (mutate-as x '#(1 2 3) (flexvector-fill! x 'foo)))
  (test-equal "flexvector-fill! start" '#(1 2 bar bar bar)
              (mutate-as x '#(1 2 3 4 5) (flexvector-fill! x 'bar 2)))
  (test-equal "flexvector-fill! start end" '#(1 2 baz baz 5)
              (mutate-as x '#(1 2 3 4 5) (flexvector-fill! x 'baz 2 4)))
  (test-equal "flexvector-fill! clamped" '#(qux qux qux)
              (mutate-as x '#(1 2 3) (flexvector-fill! x 'qux -1 10)))


  (test-equal "flexvector->bytes" #"abcd" (flexvector->bytes (flexvector (char->integer #\a) (char->integer #\b) (char->integer #\c) (char->integer #\d))))
  (test-equal "bytes->flexvector" (flexvector (char->integer #\a) (char->integer #\b) (char->integer #\c) (char->integer #\d)) (bytes->flexvector #"abcd"))

  )
