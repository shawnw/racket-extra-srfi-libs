#lang racket/base

(require racket/contract (only-in racket/list make-list) srfi/41 racket/format
         (only-in racket/generator [generator? rkt-generator?] [generator-state rkt-generator-state]))
(module+ test (require rackunit))
(provide
 (contract-out
  #:unprotected-submodule unsafe
  ;; SRFI-158
  [generator (-> any/c ... (-> any/c))]
  [circular-generator (-> any/c ... (-> any/c))]
  [make-iota-generator (->* (exact-nonnegative-integer?) (number? number?) (-> (or/c number? eof-object?)))]
  [make-range-generator (->* (number?) (number? number?) (-> (or/c number? eof-object?)))]
  [make-coroutine-generator (-> (-> procedure? any/c) (-> any/c))]
  [list->generator (-> list? (-> any/c))]
  [vector->generator (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (-> any/c))]
  [reverse-vector->generator (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) (-> any/c))]
  [string->generator (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) (-> (or/c char? eof-object?)))]
  [bytevector->generator (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) (-> (or/c byte? eof-object?)))]
  [make-for-each-generator (-> (-> (-> any/c any/c) any/c any/c) any/c (-> any/c))]
  [make-unfold-generator (-> (-> any/c any/c) (-> any/c any/c) (-> any/c any/c) any/c (-> any/c))]
  [gcons* (-> any/c ... (-> any/c) (-> any/c))]
  [gappend (-> (-> any/c) ... (-> any/c))]
  [gflatten (-> (-> any/c) (-> any/c))]
  [ggroup (->* ((-> any/c) exact-positive-integer?) (any/c) (-> any/c))]
  [gmerge (->* ((-> any/c any/c any/c) (-> any/c)) () #:rest (listof (-> any/c)) (-> any/c))]
  [gmap (->i ([proc (gens) (and/c (unconstrained-domain-> any/c)
                                  (lambda (f) (procedure-arity-includes? f (+ (length gens) 1))))]
              [g1 (-> any/c)])
             () #:rest [gens (listof (-> any/c))]
             [_ (-> any/c)])]
  [gcombine (->i ([proc (gens) (and/c (unconstrained-domain-> (values any/c any/c))
                                      (lambda (f) (procedure-arity-includes? f (+ (length gens) 2))))]
                  [seed any/c]
                  [g1 (-> any/c)])
                 () #:rest [gens (listof (-> any/c))]
                 [_ (-> any/c)])]
  [gfilter (-> predicate/c (-> any/c) (-> any/c))]
  [gremove (-> predicate/c (-> any/c) (-> any/c))]
  [gstate-filter (-> (-> any/c any/c (values boolean? any/c)) any/c (-> any/c) (-> any/c))]
  [gtake (->* ((-> any/c) exact-nonnegative-integer?) (any/c) (-> any/c))]
  [gdrop (-> (-> any/c) exact-nonnegative-integer? (-> any/c))]
  [gtake-while (-> predicate/c (-> any/c) (-> any/c))]
  [gdrop-while (-> predicate/c (-> any/c) (-> any/c))]
  [gdelete (->* (any/c (-> any/c)) ((-> any/c any/c any/c)) (-> any/c))]
  [gdelete-neighbor-dups (->* ((-> any/c)) ((-> any/c any/c any/c)) (-> any/c))]
  [gindex (-> (-> any/c) (-> any/c) (-> any/c))]
  [gselect (-> (-> any/c) (-> any/c) (-> any/c))]
  [generator->list (->* ((-> any/c)) (exact-nonnegative-integer?) list?)]
  [generator->reverse-list (->* ((-> any/c)) (exact-nonnegative-integer?) list?)]
  [generator->vector (->* ((-> any/c)) (exact-nonnegative-integer?) vector?)]
  [generator->vector! (-> vector? exact-nonnegative-integer? (-> any/c) any)]
  [generator->string (->* ((-> (or/c char? eof-object?))) (exact-nonnegative-integer?) string?)]
  [generator-fold (->i ([proc (gens) (and/c (unconstrained-domain-> any/c)
                                            (lambda (f) (procedure-arity-includes? f (+ (length gens) 2))))]
                        [knil any/c]
                        [g1 (-> any/c)])
                       ()
                       #:rest [gens (listof (-> any/c))]
                       [_ any/c])]
  [generator-for-each (->i ([proc (gens) (lambda (f)
                                           (and (procedure? f)
                                                (procedure-arity-includes? f (+ (length gens) 1))))]
                            [g1 (-> any/c)])
                           () #:rest [gens (listof (-> any/c))]
                           [_ void?])]
  [generator-map->list (->i ([proc (gens) (and/c (unconstrained-domain-> any/c)
                                                 (lambda (f) (procedure-arity-includes? f (+ (length gens) 1))))]
                             [g1 (-> any/c)])
                            () #:rest [gens (listof (-> any/c))]
                            [_ list?])]
  [generator-find (-> predicate/c (-> any/c) any/c)]
  [generator-count (-> predicate/c (-> any/c) exact-nonnegative-integer?)]
  [generator-any (-> predicate/c (-> any/c) any/c)]
  [generator-every (-> predicate/c (-> any/c) any/c)]
  [generator-unfold (->i ([g1 (-> any/c)]
                          [proc (args) (and/c (unconstrained-domain-> any/c)
                                              (lambda (f) (procedure-arity-includes? f (+ (length args) 4))))])
                         () #:rest [args (listof any/c)]
                         [_ any/c])]
  [make-accumulator (-> procedure? any/c procedure? any)]
  [count-accumulator (-> procedure?)]
  [list-accumulator (-> procedure?)]
  [reverse-list-accumulator (-> procedure?)]
  [vector-accumulator (-> procedure?)]
  [reverse-vector-accumulator (-> procedure?)]
  [vector-accumulator! (-> (and/c vector? (not/c immutable?)) exact-nonnegative-integer? procedure?)]
  [string-accumulator (-> procedure?)]
  [bytevector-accumulator (-> procedure?)]
  [bytevector-accumulator! (-> (and/c bytes? (not/c immutable?)) exact-nonnegative-integer? procedure?)]
  [sum-accumulator (-> procedure?)]
  [product-accumulator (-> procedure?)]
  ;; SRFI-221
  [gcompose-left (->* ((-> (-> any/c))) () #:rest (listof (-> (-> any/c) (-> any/c))) (-> any/c))]
  [gcompose-right (->* ((-> (-> any/c) (-> any/c))) () #:rest (listof (-> (-> any/c))) (-> any/c))]
  [accumulate-generated-values (-> (-> any/c) (-> any/c any) any)]
  [genumerate (-> (-> any/c) (-> (cons/c exact-positive-integer? any/c)))]
  [gchoice (->* ((-> exact-nonnegative-integer?)) () #:rest (listof (-> any/c)) (-> any/c))]
  [stream->generator (-> stream? (-> any/c))]
  [generator->stream (-> (-> any/c) stream?)]
  ;; Racket-specific additions
  [generator? predicate/c]
  [rkt-generator->srfi-generator (-> rkt-generator? (-> any/c))]
  [sequence->generator (-> sequence? (-> any/c))]
  ))

(define (generator? obj)
  (and
   (procedure? obj)
   (procedure-arity-includes? obj 0)
   (let ([returns (procedure-result-arity obj)])
     (or (eq? returns #f)
         (eqv? returns 1)))))

;; generator
(define (generator . args)
  (lambda () (if (null? args)
               eof
               (let ((next (car args)))
                (set! args (cdr args))
                next))))

;; circular-generator
(define (circular-generator . args)
  (let ((base-args args))
    (lambda ()
      (when (null? args)
        (set! args base-args))
      (let ((next (car args)))
        (set! args (cdr args))
        next))))


;; make-iota-generator
(define make-iota-generator
  (case-lambda ((count) (make-iota-generator count 0 1))
               ((count start) (make-iota-generator count start 1))
               ((count start step) (make-iota count start step))))

;; make-iota
(define (make-iota count start step)
  (lambda ()
    (cond
      ((<= count 0)
       eof)
      (else
        (let ((result start))
         (set! count (- count 1))
         (set! start (+ start step))
         result)))))


;; make-range-generator
(define make-range-generator
  (case-lambda ((start end) (make-range-generator start end 1))
               ((start) (make-infinite-range-generator start))
               ((start end step)
                (set! start (- (+ start step) step))
                (lambda () (if (< start end)
                             (let ((v start))
                              (set! start (+ start step))
                              v)
                             eof)))))

(define (make-infinite-range-generator start)
  (lambda ()
    (let ((result start))
     (set! start (+ start 1))
     result)))



;; make-coroutine-generator
#;(define (make-coroutine-generator proc)
  (define return #f)
  (define resume #f)
  (define yield (lambda (v) (call/cc (lambda (r) (set! resume r) (return v)))))
  (lambda () (call/cc (lambda (cc) (set! return cc)
                        (if resume
                          (resume (void))  ; void? or yield again?
                          (begin (proc yield)
                                 (set! resume (lambda (v) (return eof)))
                                 (return eof)))))))

;; Use a continuation prompt instead of saving a second continuation for where to return to.
(require racket/control racket/function)
(define (make-coroutine-generator proc)
  (define coroutine-tag (make-continuation-prompt-tag))
  (define resume #f)
  (define (yield v) (call/cc (lambda (r) (set! resume r) (abort/cc coroutine-tag v)) coroutine-tag))
  (lambda ()
    (call/prompt
     (thunk
      (cond
        (resume (resume (void)))
        (else
         (proc yield)
         (set! resume (lambda (v) (abort/cc coroutine-tag eof)))
         eof)))
     coroutine-tag
     identity)))

;; list->generator
(define (list->generator lst)
  (lambda () (if (null? lst)
               eof
               (let ((next (car lst)))
                (set! lst (cdr lst))
                next))))


;; vector->generator
(define (vector->generator vec [start 0] [end (vector-length vec)])
  (lambda () (if (>= start end)
                 eof
                 (let ((next (vector-ref vec start)))
                   (set! start (+ start 1))
                   next))))


;; reverse-vector->generator
(define (reverse-vector->generator vec [start 0] [end (vector-length vec)])
  (lambda () (if (>= start end)
                 eof
                 (let ((next (vector-ref vec (- end 1))))
                   (set! end (- end 1))
                   next))))


;; string->generator
(define (string->generator str [start 0] [end (string-length str)])
  (lambda () (if (>= start end)
                 eof
                 (let ((next (string-ref str start)))
                   (set! start (+ start 1))
                   next))))


;; bytevector->generator
(define (bytevector->generator str [start 0] [end (bytes-length str)])
  (lambda () (if (>= start end)
                 eof
                 (let ((next (bytes-ref str start)))
                   (set! start (+ start 1))
                   next))))


;; make-for-each-generator
;FIXME: seems to fail test
(define (make-for-each-generator for-each obj)
  (make-coroutine-generator (lambda (yield) (for-each yield obj))))


;; make-unfold-generator
(define (make-unfold-generator stop? mapper successor seed)
  (make-coroutine-generator (lambda (yield)
                              (let loop ((s seed))
                               (if (stop? s)
                                 (void)
                                 (begin (yield (mapper s))
                                        (loop (successor s))))))))


;; gcons*
(define (gcons* . args)
  (lambda () (if (null? args)
               eof
               (if (= (length args) 1)
                 ((car args))
                 (let ((v (car args)))
                  (set! args (cdr args))
                  v)))))


;; gappend
(define (gappend . args)
  (lambda () (if (null? args)
               eof
               (let loop ((v ((car args))))
                (if (eof-object? v)
                  (begin (set! args (cdr args))
                         (if (null? args)
                           eof
                           (loop ((car args)))))
                  v)))))

;; gflatten
(define (gflatten gen)
  (let ((state '()))
    (lambda ()
      (when (null? state) (set! state (gen)))
      (if (eof-object? state)
        state
        (let ((obj (car state)))
          (set! state (cdr state))
          obj)))))

;; ggroup
(define ggroup
  (case-lambda
    ((gen k)
     (simple-ggroup gen k))
    ((gen k padding)
     (padded-ggroup (simple-ggroup gen k) k padding))))

(define (simple-ggroup gen k)
  (lambda ()
    (let loop ((item (gen)) (result '()) (count (- k 1)))
      (if (eof-object? item)
        (if (null? result) item (reverse result))
        (if (= count 0)
          (reverse (cons item result))
          (loop (gen) (cons item result) (- count 1)))))))

(define (padded-ggroup gen k padding)
  (lambda ()
    (let ((item (gen)))
      (if (eof-object? item)
        item
        (let ((len (length item)))
          (if (= len k)
              item
              (append item (make-list (- k len) padding))))))))

;; gmerge
(define gmerge
  (case-lambda
    ((<) (raise-arity-error 'gmerge (make-arity-at-least 2)))
    ((< gen) gen)
    ((< genleft genright)
     (let ((left (genleft))
           (right (genright)))
       (lambda ()
         (cond
          ((and (eof-object? left) (eof-object? right))
           left)
          ((eof-object? left)
           (let ((obj right)) (set! right (genright)) obj))
          ((eof-object? right)
           (let ((obj left))  (set! left (genleft)) obj))
          ((< right left)
           (let ((obj right)) (set! right (genright)) obj))
          (else
           (let ((obj left)) (set! left (genleft)) obj))))))
    ((< . gens)
     (apply gmerge <
            (let loop ((gens gens) (gs '()))
              (cond ((null? gens) (reverse gs))
                    ((null? (cdr gens)) (reverse (cons (car gens) gs)))
                    (else (loop (cddr gens)
                                (cons (gmerge < (car gens) (cadr gens)) gs)))))))))

;; gmap
(define gmap
  (case-lambda
    ((proc) (raise-arity-error 'gmap (make-arity-at-least 2) proc))
    ((proc gen)
     (lambda ()
       (let ((item (gen)))
         (if (eof-object? item) item (proc item)))))
    ((proc . gens)
     (lambda ()
       (let ((items (map (lambda (x) (x)) gens)))
         (if (ormap eof-object? items) eof (apply proc items)))))))

;; gcombine
(define (gcombine proc seed . gens)
  (lambda ()
    (define items (map (lambda (x) (x)) gens))
    (if (ormap eof-object? items)
      eof
      (let ()
       (define-values (value newseed) (apply proc (append items (list seed))))
       (set! seed newseed)
       value))))

;; gfilter
(define (gfilter pred gen)
  (lambda () (let loop ()
              (let ((next (gen)))
               (if (or (eof-object? next)
                       (pred next))
                 next
                 (loop))))))

;; gstate-filter
(define (gstate-filter proc seed gen)
  (let ((state seed))
    (lambda ()
      (let loop ((item (gen)))
        (if (eof-object? item)
          item
          (let-values (((yes newstate) (proc item state)))
            (set! state newstate)
            (if yes
               item
               (loop (gen)))))))))



;; gremove
(define (gremove pred gen)
  (gfilter (lambda (v) (not (pred v))) gen))



;; gtake
(define gtake
  (case-lambda ((gen k) (gtake gen k eof))
               ((gen k padding)
                (make-coroutine-generator (lambda (yield)
                                            (if (> k 0)
                                                (let loop ((i 0) (v (gen)))
                                                  (begin (if (eof-object? v) (yield padding) (yield v))
                                                         (if (< (+ 1 i) k)
                                                             (loop (+ 1 i) (gen))
                                                             eof)))
                                                eof))))))



;; gdrop
(define (gdrop gen k)
  (lambda () (do () ((<= k 0)) (set! k (- k 1)) (gen))
    (gen)))



;; gdrop-while
(define (gdrop-while pred gen)
  (define found #f)
  (lambda ()
    (let loop ()
     (let ((val (gen)))
      (cond (found val)
            ((and (not (eof-object? val)) (pred val)) (loop))
            (else (set! found #t) val))))))


;; gtake-while
(define (gtake-while pred gen)
  (lambda () (let ((next (gen)))
              (if (eof-object? next)
                next
                (if (pred next)
                  next
                  (begin (set! gen (generator))
                         (gen)))))))



;; gdelete
(define gdelete
  (case-lambda ((item gen) (gdelete item gen equal?))
               ((item gen ==)
                (lambda () (let loop ((v (gen)))
                            (cond
                              ((eof-object? v) eof)
                              ((== item v) (loop (gen)))
                              (else v)))))))



;; gdelete-neighbor-dups
(define gdelete-neighbor-dups
  (case-lambda ((gen)
                (gdelete-neighbor-dups gen equal?))
               ((gen ==)
                (define firsttime #t)
                (define prev #f)
                (lambda () (if firsttime
                             (begin (set! firsttime #f)
                                    (set! prev (gen))
                                    prev)
                             (let loop ((v (gen)))
                              (cond
                                ((eof-object? v)
                                 v)
                                ((== prev v)
                                 (loop (gen)))
                                (else
                                  (set! prev v)
                                  v))))))))


;; gindex
(define (gindex value-gen index-gen)
  (let ((done? #f) (count 0))
   (lambda ()
     (if done?
       eof
       (let loop ((value (value-gen)) (index (index-gen)))
        (cond
          ((or (eof-object? value) (eof-object? index))
           (set! done? #t)
           eof)
          ((= index count)
           (set! count (+ count 1))
           value)
          (else
            (set! count (+ count 1))
            (loop (value-gen) index))))))))


;; gselect
(define (gselect value-gen truth-gen)
  (let ((done? #f))
   (lambda ()
     (if done?
       eof
       (let loop ((value (value-gen)) (truth (truth-gen)))
        (cond
          ((or (eof-object? value) (eof-object? truth))
           (set! done? #t)
           eof)
          (truth value)
          (else (loop (value-gen) (truth-gen)))))))))

;; generator->list
(define generator->list
  (case-lambda ((gen n)
		(generator->list (gtake gen n)))
               ((gen)
		(for/list ([elem (in-producer gen eof)]) elem))))

;; generator->reverse-list
(define generator->reverse-list
  (case-lambda ((gen n)
		(generator->reverse-list (gtake gen n)))
               ((gen)
		(generator-fold cons '() gen))))

;; generator->vector
(define generator->vector
  (case-lambda ((gen) (for/vector ([elem (in-producer gen eof)]) elem))
               ((gen n) (list->vector (generator->list gen n)))))


;; generator->vector!
(define (generator->vector! vector at gen)
  (let loop ((value (gen)) (count 0) (at at))
   (cond
     ((eof-object? value) count)
     ((>= at (vector-length vector)) count)
     (else (begin
             (vector-set! vector at value)
             (loop (gen) (+ count 1) (+ at 1)))))))


;; generator->string
(define generator->string
  (case-lambda ((gen) (list->string (generator->list gen)))
               ((gen n) (list->string (generator->list gen n)))))




;; generator-fold
(define (generator-fold f seed . gs)
  (define (inner-fold seed)
    (let ((vs (map (lambda (g) (g)) gs)))
     (if (ormap eof-object? vs)
       seed
       (inner-fold (apply f (append vs (list seed)))))))
  (inner-fold seed))



;; generator-for-each
(define (generator-for-each f . gs)
  (let loop ()
   (let ((vs (map (lambda (g) (g)) gs)))
    (if (ormap eof-object? vs)
      (void)
      (begin (apply f vs)
             (loop))))))


(define (generator-map->list f . gs)
  (let loop ((result '()))
   (let ((vs (map (lambda (g) (g)) gs)))
    (if (ormap eof-object? vs)
      (reverse result)
      (loop (cons (apply f vs) result))))))


;; generator-find
(define (generator-find pred g)
  (let loop ((v (g)))
    (cond ((eof-object? v) #f)
          ((pred v) v)
          (else (loop (g))))))

;; generator-count
(define (generator-count pred g)
  (generator-fold (lambda (v n) (if (pred v) (+ 1 n) n)) 0 g))


;; generator-any
(define (generator-any pred gen)
  (let loop ((item (gen)))
    (cond ((eof-object? item) #f)
          ((pred item))
          (else (loop (gen))))))


;; generator-every
(define (generator-every pred gen)
  (let loop ((item (gen)) (last #t))
    (if (eof-object? item)
      last
      (let ((r (pred item)))
        (if r
          (loop (gen) r)
          #f)))))


;; generator-unfold
(define (generator-unfold g unfold . args)
  (apply unfold eof-object? (lambda (x) x) (lambda (x) (g)) (g) args))


;; make-accumulator
(define (make-accumulator kons knil finalize)
  (let ((state knil))
    (lambda (obj)
      (if (eof-object? obj)
        (finalize state)
        (set! state (kons obj state))))))


;; count-accumulator
(define (count-accumulator) (make-accumulator
                            (lambda (obj state) (+ 1 state)) 0 (lambda (x) x)))

;; list-accumulator
(define (list-accumulator) (make-accumulator cons '() reverse))

;; reverse-list-accumulator
(define (reverse-list-accumulator) (make-accumulator cons '() (lambda (x) x)))

;; vector-accumulator
(define (vector-accumulator)
  (make-accumulator cons '() (lambda (x) (list->vector (reverse x)))))

;; reverse-vector-accumulator
(define (reverse-vector-accumulator)
  (make-accumulator cons '() list->vector))

;; vector-accumulator!
(define (vector-accumulator! vec at)
  (lambda (obj)
    (if (eof-object? obj)
      vec
      (begin
        (vector-set! vec at obj)
        (set! at (+ at 1))))))

;; bytevector-accumulator
(define (bytevector-accumulator)
  (make-accumulator cons '() (lambda (x) (list->bytes (reverse x)))))

(define (bytevector-accumulator! bytevec at)
  (lambda (obj)
    (if (eof-object? obj)
      bytevec
      (begin
        (bytes-set! bytevec at obj)
        (set! at (+ at 1))))))

;; string-accumulator
(define (string-accumulator)
  (make-accumulator cons '()
        (lambda (lst) (list->string (reverse lst)))))

;; sum-accumulator
(define (sum-accumulator) (make-accumulator + 0 (lambda (x) x)))

;; product-accumulator
(define (product-accumulator) (make-accumulator * 1 (lambda (x) x)))

;;; SRFI-221 functions

(define (accumulate-generated-values acc gen)
  (let ((value (gen)))
   (if (eof-object? value)
       (acc value)
       (begin
         (acc value)
         (accumulate-generated-values acc gen)))))

(define gdelete-duplicates
  (case-lambda
    ((gen) (gdelete-duplicates* gen equal?))
    ((gen =) (gdelete-duplicates* gen =))))

(define (gdelete-duplicates* gen =)
  (define seen '())
  ;; first parameter should be older value than second. However in `member` it's other way around.
  ;; as such function is changed to switch parameters in places
  (define (=* a b) (= b a))
  (define (seen? value)
    (member value seen =*))
  (lambda ()
    (let loop ((value (gen)))
     (cond
       ((eof-object? value)
        value)
       ((seen? value)
        (loop (gen)))
       (else
         (begin
           (set! seen (cons value seen))
           value))))))

(define (genumerate gen)
  (gmap
    cons
    (make-range-generator 0)
    gen))

(define (gcompose-left constr . ops)
  (let loop ((gen (constr))
             (ops ops))
   (if (null? ops)
       gen
       (let* ((op (car ops))
              (new-gen (op gen)))
         (loop new-gen (cdr ops))))))

(define (gcompose-right . args)
  (apply gcompose-left (reverse args)))

(define (gchoice choice-gen . source-gens)
  (define source-gens-v (list->vector source-gens))
  (define l (vector-length source-gens-v))
  (define exhausted-count 0)
  #;(unless (procedure? choice-gen)
    (raise-argument-error 'gchoice "procedure?" choice-gen))
  #;(for-each
    (lambda (g)
      (unless (procedure? g)
        (raise-argument-error 'gchoice "(listof procedure?)" source-gens)))
    source-gens)
  (lambda ()
    (let loop ((i (choice-gen)))
     (cond
      ;; all source-gens have been exhausted
      ((= exhausted-count l) eof)
      ;; choice-gen have been exhausted
      ((eof-object? i) eof)
      ;; source-gen returned bad value
      ((or (not (integer? i))
           (< i 0)
           (>= i l))
       (raise-result-error 'gchoice (~a "choice-gen didn't return an integer in range 0 to " (- l 1)) i))
      (else
        (let ((gen (vector-ref source-gens-v i)))
         (if (not gen)
             ;; we picked exhausted generator -- pick again
             (loop (choice-gen))
             (let ((value (gen)))
              (if (eof-object? value)
                  ;; picked generator was exhausted on this iteration -- mark it and pick again
                  (begin
                    (vector-set! source-gens-v i #f)
                    (set! exhausted-count (+ 1 exhausted-count))
                    (loop (choice-gen)))
                  value)))))))))

(define (generator->stream gen)
  (define gen-stream
    (stream-lambda ()
                   (stream-cons (gen) (gen-stream))))
  (stream-take-while
   (lambda (value) (not (eof-object? value)))
   (gen-stream)))

(define (stream->generator stream)
  (lambda ()
    (if (stream-null? stream)
        eof
        (let ((value (stream-car stream)))
         (set! stream (stream-cdr stream))
         value))))

(define (rkt-generator->srfi-generator g)
  (lambda ()
    (if (eq? (rkt-generator-state g) 'done)
        eof
        (let ([v (g)])
          (if (eq? (rkt-generator-state g) 'done)
              (if (void? v) eof g)
              v)))))


(define (sequence->generator seq)
  (let-values ([(more? next) (sequence-generate seq)])
    (make-coroutine-generator
     (lambda (yield)
       (let loop ()
         (cond
           ((more?)
            (yield (next))
            (loop))
           (else
            eof)))))))

(module+ test
  (require (only-in srfi/1 unfold) (only-in "141.rkt" truncate/)
           (only-in racket/list make-list) (only-in racket/port with-input-from-string)
           srfi/41 (only-in racket/generator [generator rkt-generator] [yield rkt-yield]))

  (define (string-for-each proc string)
    (let ((len (string-length string)))
      (let loop ((i 0))
        (cond
          ((< i len)
           (proc (string-ref string i))
           (loop (+ i 1)))
          (else
           (void))))))

  (define-syntax-rule (test expected tst)
    (check-equal? tst expected))
  (define-syntax-rule (test-equal expected tst)
    (check-equal? tst expected))
  (define-syntax-rule (test-assert condition)
    (check-not-false condition))
  (define-syntax-rule (test-group name tests ...)
    (begin ;(printf "Starting ~A...~%" name)
      tests ...
      #;(displayln "Done.")))

  (test-group "generators"
              (test-group "generators/constructors"
                          (test '() (generator->list (generator)))
                          (test '(1 2 3) (generator->list (generator 1 2 3)))
                          (test '(1 2 3 1 2) (generator->list (circular-generator 1 2 3) 5))
                          (test '(8 9 10) (generator->list (make-iota-generator 3 8)))
                          (test '(8 10 12) (generator->list (make-iota-generator 3 8 2)))
                          (test '(3 4 5 6) (generator->list (make-range-generator 3) 4))
                          (test '(3 4 5 6 7) (generator->list (make-range-generator 3 8)))
                          (test '(3 5 7) (generator->list (make-range-generator 3 8 2)))
                          (define g
                            (make-coroutine-generator
                             (lambda (yield) (let loop ((i 0))
                                               (when (< i 3) (yield i) (loop (+ i 1)))))))
                          (test '(0 1 2) (generator->list g))
                          (test '(1 2 3 4 5) (generator->list (list->generator '(1 2 3 4 5))))
                          (test '(1 2 3 4 5) (generator->list (vector->generator '#(1 2 3 4 5))))
                          (test '#(0 0 1 2 4)
                                (let ((v (make-vector 5 0)))
                                  (generator->vector! v 2 (generator 1 2 4))
                                  v))
                          (test '(5 4 3 2 1) (generator->list (reverse-vector->generator '#(1 2 3 4 5))))
                          (test '(#\a #\b #\c #\d #\e) (generator->list (string->generator "abcde")))
                          (test '(10 20 30) (generator->list (bytevector->generator (bytes 10 20 30))))
                          (define (for-each-digit proc n)
                            (when (> n 0)
                              (let-values (((div rem) (truncate/ n 10)))
                                (proc rem)
                                (for-each-digit proc div))))
                          (test '(5 4 3 2 1) (generator->list
                                              (make-for-each-generator for-each-digit
                                                                       12345)))
                          (test '(0 2 4 6 8 10) (generator->list
                                                 (make-unfold-generator
                                                  (lambda (s) (> s 5))
                                                  (lambda (s) (* s 2))
                                                  (lambda (s) (+ s 1))
                                                  0)))
                          ) ; end "generators/constructors"

              (test-group "generators/operators"
                          (test '(a b 0 1) (generator->list (gcons* 'a 'b (make-range-generator 0 2))))
                          (test '(0 1 2 0 1) (generator->list (gappend (make-range-generator 0 3)
                                                                       (make-range-generator 0 2))))
                          (test '() (generator->list (gappend)))
                          (define g1 (generator 1 2 3))
                          (define g2 (generator 4 5 6 7))
                          (define (proc . args) (values (apply + args) (apply + args)))
                          (test '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))
                          (test '(1 3 5 7 9) (generator->list (gfilter
                                                               odd?
                                                               (make-range-generator 1 11))))
                          (test '(2 4 6 8 10) (generator->list (gremove
                                                                odd?
                                                                (make-range-generator 1 11))))
                          (define g3 (make-range-generator 1 5))
                          (test '(1 2 3) (generator->list (gtake g3 3)))
                          (test '(4) (generator->list g3))
                          (test '(1 2) (generator->list (gtake (make-range-generator 1 3) 3)))
                          (test '(1 2 0) (generator->list (gtake (make-range-generator 1 3) 3 0)))
                          (test '(3 4) (generator->list (gdrop (make-range-generator 1 5) 2)))
                          (define g4 (make-range-generator 1 5))
                          (define (small? x) (< x 3))
                          (test '(1 2) (generator->list (gtake-while small? g4)))
                          (define g5 (make-range-generator 1 5))
                          (test '(3 4) (generator->list (gdrop-while small? g5)))
                          (test '() (generator->list (gdrop-while (lambda args #t) (generator 1 2 3))))
                          (test '(0.0 1.0 0 2) (generator->list (gdelete 1
                                                                         (generator 0.0 1.0 0 1 2))))
                          (test '(0.0 0 2) (generator->list (gdelete 1
                                                                     (generator 0.0 1.0 0 1 2)
                                                                     =)))
                          (test '(a c e) (generator->list (gindex (list->generator '(a b c d e f))
                                                                  (list->generator '(0 2 4)))))
                          (test '(a d e) (generator->list (gselect (list->generator '(a b c d e f))
                                                                   (list->generator '(#t #f #f #t #t #f)))))
                          (test '(1 2 3) (generator->list (gdelete-neighbor-dups
                                                           (generator 1 1 2 3 3 3)
                                                           =)))
                          (test '(1) (generator->list (gdelete-neighbor-dups
                                                       (generator 1 2 3)
                                                       (lambda args #t))))
                          (test '(1 2 3 a b c)
                                (generator->list
                                 (gflatten (generator '(1 2 3) '(a b c)))))
                          (test '((1 2 3) (4 5 6) (7 8))
                                (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3)))
                          (test '((1 2 3) (4 5 6) (7 8 0))
                                (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3 0)))
                          (test '(1 2 3)
                                (generator->list (gmerge < (generator 1 2 3))))
                          (test '(1 2 3 4 5 6)
                                (generator->list (gmerge < (generator 1 2 3) (generator 4 5 6))))
                          (test '(1 2 3 4 4 5 6)
                                (generator->list (gmerge <
                                                         (generator 1 2 4 6)
                                                         (generator)
                                                         (generator 3 4 5))))
                          (test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                                (generator->list (gmerge <
                                                         (generator 1 10 11)
                                                         (generator 2 9 12)
                                                         (generator 3 8 13)
                                                         (generator 4 7 14)
                                                         (generator 5 6 15))))
                          ;; check the tie-break rule
                          (test '((1 a) (1 e) (1 b) (1 c) (1 d))
                                (generator->list (gmerge (lambda (x y) (< (car x) (car y)))
                                                         (generator '(1 a) '(1 e))
                                                         (generator '(1 b))
                                                         (generator '(1 c) '(1 d)))))

                          (test '(-1 -2 -3 -4 -5)
                                (generator->list (gmap - (generator 1 2 3 4 5))))
                          (test '(7 9 11 13)
                                (generator->list (gmap +
                                                       (generator 1 2 3 4 5)
                                                       (generator 6 7 8 9))))
                          (test '(54 140 264)
                                (generator->list (gmap *
                                                       (generator 1 2 3 4 5)
                                                       (generator 6 7 8)
                                                       (generator 9 10 11 12 13))))
                          (test '(a c e g i)
                                (generator->list
                                 (gstate-filter
                                  (lambda (item state) (values (even? state) (+ 1 state)))
                                  0
                                  (generator 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))))
                          ) ; end "generators/operators"


              (test-group "generators/consumers"
                          ;; no test for plain generator->list (used throughout)
                          (test '(1 2 3) (generator->list (generator 1 2 3 4 5) 3))
                          (test '(5 4 3 2 1) (generator->reverse-list (generator 1 2 3 4 5)))
                          (test '#(1 2 3 4 5) (generator->vector (generator 1 2 3 4 5)))
                          (test '#(1 2 3) (generator->vector (generator 1 2 3 4 5) 3))
                          (test "abc" (generator->string (generator #\a #\b #\c)))
                          (test '(e d c b a . z) (with-input-from-string "a b c d e"
                                                   (lambda () (generator-fold cons 'z read))))

                          (define n 0)
                          (generator-for-each (lambda values (set! n (apply + values)))
                                              (generator 1) (generator 2) (generator 3))
                          (test 6 n)
                          (test '(6 15)
                                (generator-map->list (lambda values (apply + values))
                                                     (generator 1 4) (generator 2 5) (generator 3 6)))
                          (test 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5)))
                          (test #f (generator-find (lambda (x) (> x 10)) (make-range-generator 1 5)))
                          (test 2 (generator-count odd? (make-range-generator 1 5)))
                          (define g6 (make-range-generator 2 5))
                          (test #t (generator-any odd? g6))
                          (test '(4) (generator->list g6))
                          (define g7 (make-range-generator 2 5))
                          (test 3 (generator-any (lambda (x) (and (odd? x) x)) g7))
                          (define g8 (make-range-generator 2 5))
                          (test #f (generator-every odd? g8))
                          (test '(3 4) (generator->list g8))
                          (define g9 (make-range-generator 2 5))
                          (test 4 (generator-every (lambda (x) (and (> x 1) x)) g9))
                          (test '() (generator->list g9))
                          (test '(#\a #\b #\c) (generator-unfold (make-for-each-generator string-for-each "abc") unfold))

                          ) ; end "generators/consumers"

              ) ; end "generators"

  (test-group "accumulators"
              (test -8
                    (let ((a (make-accumulator * 1 -)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test 3
                    (let ((a (count-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test '(1 2 4)
                    (let ((a (list-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test '(4 2 1)
                    (let ((a (reverse-list-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test '#(1 2 4)
                    (let ((a (vector-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test '#(0 0 1 2 4)
                    (let* ((v (vector 0 0 0 0 0))
                           (a (vector-accumulator! v 2)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test #"\x00\x00\x01\x02\x04"
                    (let* ((v (bytes 0 0 0 0 0))
                           (a (bytevector-accumulator! v 2)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test '#(4 2 1)
                    (let ((a (reverse-vector-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test "abc"
                    (let ((a (string-accumulator)))
                      (a #\a)
                      (a #\b)
                      (a #\c)
                      (a eof)))

              (test #"\x01\x02\x04"
                    (let ((a (bytevector-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test 7
                    (let ((a (sum-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              (test 8
                    (let ((a (product-accumulator)))
                      (a 1)
                      (a 2)
                      (a 4)
                      (a eof)))

              ) ; end "accumulators"

  (define test-g-equal
    (case-lambda
      ((g-expect g-actual)
       (test-equal
        (generator->list g-expect)
        (generator->list g-actual)))
      ((g-expect g-actual take-count)
       (test-g-equal
        (gtake g-expect take-count)
        (gtake g-actual take-count)))))


  (test-group
   "accumulate-generated-values"
   (define expect '(1 2 3 4))
   (define actual
     (accumulate-generated-values
      (list-accumulator)
      (generator 1 2 3 4)))
   (test-equal expect actual))

  (test-group
   "genumerate"

   ;; test normal case
   (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                 (genumerate (generator 'a 'b 'c)))

   ;; test empty
   (test-g-equal (generator)
                 (genumerate (generator)))

   ;; infinite case with take
   (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                 (genumerate (circular-generator 'a 'b 'c))
                 3)
   )

  (test-group
   "gcompose-left"

   (test-g-equal
    (generator 1 2 3 4)
    (gcompose-left
     (lambda () (make-range-generator 1))
     (lambda (g) (gtake g 4))))

   (test-g-equal
    (generator 1 2 3 4)
    (gcompose-left
     (lambda () (generator 1 2 3 4)))))

  (test-group
   "gcompose-right"

   (test-g-equal
    (generator 1 2 3 4)
    (gcompose-right
     (lambda (g) (gtake g 4))
     (lambda () (make-range-generator 1))))

   (test-g-equal
    (generator 1 2 3 4)
    (gcompose-right
     (lambda () (generator 1 2 3 4)))))

  (test-group
   "gchoice"

   ;; test normal
   (test-g-equal
    (generator 1 2 1 3)
    (gchoice
     (generator 0 1 0 2)
     (circular-generator 1)
     (circular-generator 2)
     (circular-generator 3)))

   ;; test exhausted source
   (test-g-equal
    (generator 1 2 3)
    (gchoice
     (generator 0 0 0 0 0 1 1 2)
     (generator 1)
     (generator 2)
     (generator 3)))

   ;; test exhausted source (choice generator won't be exhausted)
   (test-g-equal
    (generator 1 3 5 2 4 6)
    (gchoice
     (make-unfold-generator (lambda (_) #f)
                            (lambda (x) (modulo x 3))
                            (lambda (x) (+ x 1))
                            0)
     (generator 1 2)
     (generator 3 4)
     (generator 5 6))))

  (test-group
   "generator->stream"
   (define (test-stream-equal str1 str2)
     (if (stream-null? str1)
         (test-assert (stream-null? str2))
         (begin
           (test-equal (stream-car str1) (stream-car str2))
           (test-stream-equal (stream-cdr str1) (stream-cdr str2)))))

   ;; test normal
   (test-stream-equal
    (stream 1 2 3)
    (generator->stream (generator 1 2 3)))

   ;; test infinite with take
   (test-stream-equal
    (stream 1 2 3)
    (stream-take 3 (generator->stream (circular-generator 1 2 3)))))

  (test-group
   "stream->generator"
   ;; test normal
   (test-g-equal
    (generator 1 2 3)
    (stream->generator (stream 1 2 3)))

   ;; test infinite with take
   (test-g-equal
    (circular-generator 1 2 3)
    (stream->generator (stream-constant 1 2 3))
    20))

  (define rkt-g (rkt-generator ()
                               (let loop ([lst '(1 2 3)])
                                 (unless (null? lst)
                                   (rkt-yield (car lst))
                                   (loop (cdr lst))))))

  (test-equal? "racket to srfi generator conversion"
               (generator->list (rkt-generator->srfi-generator rkt-g))
               '(1 2 3))


  (test-equal? "sequence to generator conversion"
               (generator->list (sequence->generator (in-range 10)))
               '(0 1 2 3 4 5 6 7 8 9))

  (test-true "generator? random" (generator? random))
  (test-false "generator? cons" (generator? cons)))
