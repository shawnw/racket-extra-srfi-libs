#lang racket/base
;;; Integer Mappings

(require racket/contract racket/hash-code racket/require racket/dict
         (only-in racket/fixnum most-negative-fixnum)
         srfi/41 "145.rkt"
         (only-in srfi/1 fold unfold every iota partition)
         (only-in "128.rkt" comparator? =? make-comparator make-default-comparator)
         (only-in "158.rkt" make-coroutine-generator generator->stream)
         (for-syntax racket/base (only-in racket/string string-prefix?)))
(require (filtered-in
          (lambda (name)
            (and (string-prefix? name "unsafe-fx")
                 (substring name 7)))
          racket/unsafe/ops))
(module+ test (require rackunit))

(define (validate-args kvs)
  (cond
    ((null? kvs) #t)
    ((null? (cdr kvs)) (format "missing value for key ~S" (car kvs)))
    ((fixnum? (car kvs)) (validate-args (cddr kvs)))
    (else (format "key ~S isn't a fixnum" (car kvs)))))

(provide
 (contract-out
  #:unprotected-submodule unsafe
  [fxmapping? predicate/c]
  [fxmapping (->i () () #:rest [kvs list?]
                  #:pre/desc (kvs) (validate-args kvs)
             [_ fxmapping?])]
  [fxmapping-unfold (->* ((->* (any/c) () #:rest list? any/c) (->* (any/c) () #:rest list? (values fixnum? any/c)) (->* (any/c) () #:rest list? any) any/c) ()
                         #:rest list? fxmapping?)]
  [fxmapping-accumulate (-> procedure? any/c any/c ... fxmapping?)]
  [alist->fxmapping (-> (listof (cons/c fixnum? any/c)) fxmapping?)]
  [alist->fxmapping/combinator (-> (-> fixnum? any/c any/c any/c) (listof (cons/c fixnum? any/c)) fxmapping?)]
  [fxmapping-contains? (-> fxmapping? fixnum? boolean?)]
  [fxmapping-empty? (-> fxmapping? boolean?)]
  [fxmapping-disjoint? (-> fxmapping? fxmapping? boolean?)]
  [fxmapping-ref (->* (fxmapping? fixnum?) (procedure? procedure?) any)]
  [fxmapping-ref/default (-> fxmapping? fixnum? any/c any/c)]
  [fxmapping-min (-> fxmapping? (values fixnum? any/c))]
  [fxmapping-max (-> fxmapping? (values fixnum? any/c))]
  (fxmapping-adjoin (->i ([fx fxmapping?] [k1 fixnum?] [v1 any/c]) () #:rest [kvs list?]
                         #:pre/desc (kvs) (validate-args kvs)
                         [_ fxmapping?]))
  (fxmapping-adjoin/combinator (->i ([fx fxmapping?] [p procedure?] [k1 fixnum?] [v1 any/c]) () #:rest [kvs list?]
                                    #:pre/desc (kvs) (validate-args kvs)
                                    [_ fxmapping?]))

  (fxmapping-set (->i ([fx fxmapping?] [k1 fixnum?] [v1 any/c]) () #:rest [kvs list?]
                      #:pre/desc (kvs) (validate-args kvs)
                      [_ fxmapping?]))
  [fxmapping-adjust (-> fxmapping? fixnum? (-> fixnum? any/c any/c) fxmapping?)]
  [fxmapping-delete (->* (fxmapping? fixnum?) () #:rest fixnum? fxmapping?)]
  [fxmapping-delete-all (-> fxmapping? (listof fixnum?) fxmapping?)]
  [fxmapping-alter (-> fxmapping? fixnum? procedure? procedure? any)]
  [fxmapping-delete-min (-> fxmapping? fxmapping?)]
  [fxmapping-delete-max (-> fxmapping? fxmapping?)]
  [fxmapping-update-min (-> fxmapping? procedure? any)]
  [fxmapping-update-max (-> fxmapping? procedure? any)]
  [fxmapping-pop-min (-> fxmapping? (values fixnum? any/c fxmapping?))]
  [fxmapping-pop-max (-> fxmapping? (values fixnum? any/c fxmapping?))]
  [fxmapping-size (-> fxmapping? exact-nonnegative-integer?)]
  [fxmapping-find (->* ((-> fixnum? any/c boolean?) fxmapping? procedure?) (procedure?) any)]
  [fxmapping-count (-> (-> fixnum? any/c boolean?) fxmapping? exact-nonnegative-integer?)]
  [fxmapping-any? (-> (-> fixnum? any/c boolean?) fxmapping? boolean?)]
  [fxmapping-every? (-> (-> fixnum? any/c boolean?) fxmapping? boolean?)]
  [fxmapping-map (-> (-> fixnum? any/c any/c) fxmapping? fxmapping?)]
  [fxmapping-for-each (-> (-> fixnum? any/c any/c) fxmapping? any)]
  [fxmapping-fold (-> (-> fixnum? any/c any/c any/c) any/c fxmapping? any/c)]
  [fxmapping-fold-right (-> (-> fixnum? any/c any/c any/c) any/c fxmapping? any/c)]
  [fxmapping-map->list (-> (-> fixnum? any/c any/c) fxmapping? list?)]
  [fxmapping-relation-map (-> (-> fixnum? any/c (values fixnum? any/c)) fxmapping? fxmapping?)]
  [fxmapping-filter (-> (-> fixnum? any/c boolean?) fxmapping? fxmapping?)]
  [fxmapping-remove (-> (-> fixnum? any/c boolean?) fxmapping? fxmapping?)]
  [fxmapping-partition (-> (-> fixnum? any/c boolean?) fxmapping? (values fxmapping? fxmapping?))]
  [fxmapping->alist (-> fxmapping? (listof (cons/c fixnum? any/c)))]
  [fxmapping->decreasing-alist (-> fxmapping? (listof (cons/c fixnum? any/c)))]
  [fxmapping-keys (-> fxmapping? (listof fixnum?))]
  [fxmapping-values (-> fxmapping? (listof any/c))]
  [fxmapping->generator (-> fxmapping? (-> (or/c (cons/c fixnum? any/c) eof-object?)))]
  [fxmapping=? (->* (comparator? fxmapping? fxmapping?) () #:rest (listof fxmapping?) boolean?)]
  [fxmapping<? (->* (comparator? fxmapping? fxmapping?) () #:rest (listof fxmapping?) boolean?)]
  [fxmapping<=? (->* (comparator? fxmapping? fxmapping?) () #:rest (listof fxmapping?) boolean?)]
  [fxmapping>? (->* (comparator? fxmapping? fxmapping?) () #:rest (listof fxmapping?) boolean?)]
  [fxmapping>=? (->* (comparator? fxmapping? fxmapping?) () #:rest (listof fxmapping?) boolean?)]
  [fxmapping-union (->* (fxmapping? fxmapping?) () #:rest (listof fxmapping?) fxmapping?)]
  [fxmapping-intersection (->* (fxmapping? fxmapping?) () #:rest (listof fxmapping?) fxmapping?)]
  [fxmapping-difference (->* (fxmapping? fxmapping?) () #:rest (listof fxmapping?) fxmapping?)]
  [fxmapping-xor (-> fxmapping? fxmapping? fxmapping?)]
  [fxmapping-union/combinator (->* ((-> fixnum? any/c any/c any/c) fxmapping? fxmapping?) () #:rest (listof fxmapping?) fxmapping?)]
  [fxmapping-intersection/combinator (->* ((-> fixnum? any/c any/c any/c) fxmapping? fxmapping?) () #:rest (listof fxmapping?) fxmapping?)]
  [fxmapping-open-interval (-> fxmapping? fixnum? fixnum? fxmapping?)]
  [fxmapping-closed-interval (-> fxmapping? fixnum? fixnum? fxmapping?)]
  [fxmapping-open-closed-interval (-> fxmapping? fixnum? fixnum? fxmapping?)]
  [fxmapping-closed-open-interval (-> fxmapping? fixnum? fixnum? fxmapping?)]
  [fxsubmapping= (-> fxmapping? fixnum? fxmapping?)]
  [fxsubmapping< (-> fxmapping? fixnum? fxmapping?)]
  [fxsubmapping<= (-> fxmapping? fixnum? fxmapping?)]
  [fxsubmapping> (-> fxmapping? fixnum? fxmapping?)]
  [fxsubmapping>= (-> fxmapping? fixnum? fxmapping?)]
  [fxmapping-split (-> fxmapping? fixnum? (values fxmapping? fxmapping?))]
  ))

(define fx-least (most-negative-fixnum))
(define (fxpositive? n) (fx> n 0))
(define (fxnegative? n) (fx< n 0))
(define (fxzero? n) (fx= n 0))
(define (fxneg n) (fx- n))

(define-syntax-rule (values->list expr)
  (call-with-values
   (lambda () expr)
   list))

;;; Pattern-matching macro for trie values.
;;;
;;; Based on William Byrd's pmatch modification of Oleg Kiselyov's
;;; simple linear pattern-matcher.

;;; Syntax:
;;;
;;; (tmatch exp <clause> ... [<else-clause>])
;;;
;;; <clause>      ::= (<pattern> [<guard>] exp ...)
;;; <else-clause> ::= (else exp ...)
;;; <guard>       ::= (guard boolean-exp ...)
;;; <pattern>     ::= empty
;;;                 | (leaf <pattern> <pattern>)
;;;                 | (branch <pattern> <pattern> <pattern> <pattern>)

;; (define-syntax tmatch
;;   (syntax-rules (else)
;;     ((tmatch exp (e ...) ...)
;;      (tmatch-aux #f exp (e ...) ...))
;;     ((tmatch name exp (e ...) ...)
;;      (tmatch-aux name exp (e ...) ...))))

(define-syntax tmatch
  (syntax-rules (else guard)
    ((tmatch (f x ...) cs ...)
     (let ((v (f x ...)))
       (tmatch v cs ...)))
    ((tmatch v)
     (error "tmatch: no clause matched" v))
    ((tmatch _ (else e0 e1 ...)) (begin e0 e1 ...))
    ((tmatch v (pat (guard g ...) e0 e1 ...) cs ...)
     (let ((fk (lambda () (tmatch v cs ...))))
       (tpat v pat (if (and g ...) (begin e0 e1 ...) (fk)) (fk))))
    ((tmatch v (pat e0 e1 ...) cs ...)
     (let ((fk (lambda () (tmatch v cs ...))))
       (tpat v pat (begin e0 e1 ...) (fk))))))

;; Uses pmatch's `ppat' auxilliary macro, see below.
(define-syntax tpat
  (syntax-rules (empty leaf branch unquote)
    ((tpat v empty kt kf) (if v kf kt))
    ((tpat v (leaf pkey pval) kt kf)
     (if (leaf? v)
         (let ((key (leaf-key v)) (value (leaf-value v)))
           (ppat key pkey (ppat value pval kt kf) kf))
         kf))
    ((tpat v (branch pp pm pl pr) kt kf)
     (if (branch? v)
         (let ((pfx (branch-prefix v))
               (bit (branch-branching-bit v))
               (left (branch-left v))
               (right (branch-right v)))
           (ppat pfx
                 pp
                 (ppat bit pm (ppat left pl (ppat right pr kt kf) kf) kf)
                 kf))
         kf))))

;; Shorthands for a unary function that immediately pattern-matches
;; its trie parameter.
(define-syntax tmatch-lambda
  (syntax-rules ()
    ((tmatch-lambda cs ...)
     (lambda (arg) (tmatch arg cs ...)))))

;;; pmatch, by Oleg Kiselyov, rev. Will Byrd.
;;; The original public-domain code can be found at
;;; http://okmij.org/ftp/Scheme/match-case-simple.scm

;; This is a new version of pmatch (August 8, 2012).
;; It has two important new features:
;; 1.  It allows for a name to be given to the pmatch if an error ensues.
;; 2.  A line from the specification has been removed. (see below).  Without
;; that line removed, it was impossible for a pattern to be (quote ,x),
;; which might be worth having especially when we write an interpreter
;; for Scheme, which includes quote as a language form.

;;; Code written by Oleg Kiselyov
;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS (and R6RS) Scheme system.

; (pmatch exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

;; We've removed the name parameter for now, since it seems to cause
;; problems for the expander in many Schemes.

;; (define-syntax pmatch
;;   (syntax-rules (else guard)
;;     ((pmatch v (e ...) ...)
;;      (pmatch-aux #f v (e ...) ...))
;;     ((pmatch v name (e ...) ...)
;;      (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((pmatch (rator rand ...) cs ...)
     (let ((v (rator rand ...)))     ; avoid multiple evals
       (pmatch v cs ...)))
    ((pmatch v)  ; no more clauses
     (error "pmatch failed" v))
    ((pmatch _ (else e0 e ...)) (begin e0 e ...))
    ((pmatch v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((pmatch v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (? unquote)
    ((ppat _ ? kt _) kt)  ; the ? wildcard always matches
    ((ppat v () kt kf) (if (null? v) kt kf))
;   ((ppat v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((ppat v (unquote var) kt _) (let ((var v)) kt))
    ((ppat v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))

;;; Shorthands for functions that immediately pattern-match their
;;; parameter(s).

;; One-argument form.
(define-syntax pmatch-lambda
  (syntax-rules ()
    ((pmatch-lambda cs ...)
     (lambda (arg) (pmatch arg cs ...)))))

;; Multi-argument form.
(define-syntax pmatch-lambda*
  (syntax-rules ()
    ((pmatch-lambda* cs ...)
     (lambda args (pmatch args cs ...)))))

;;; This file implements integer maps as big-endian binary radix
;;; trees (AKA Patricia tries), as described by Chris Okasaki and
;;; Andrew Gill in "Fast Mergeable Integer Maps" (1998).  Integers
;;; in big-endian binary encoding are stored in a trie structure
;;; which allows fast lookup, insertion, and set-theoretical
;;; operations (union, intersection, etc.)
;;;
;;; A trie is represented by #f (the empty trie), a leaf, or a branch.
;;;
;;; Throughout this code, the empty trie (#f) is always returned
;;; as an explicit value, not, e.g. as the default value of an
;;; (and ...) expression, to clarify its use as a trie value.

;;;; Utility

(define (swap-last-args proc)
  (lambda (k x y) (proc k y x)))

(define the-empty-trie #f)

(define (trie-empty? t) (not t))

(struct leaf (key value) #:transparent)
(struct branch (prefix branching-bit left right)
  #:name <branch>
  #:constructor-name raw-branch
  #:transparent)

(define (valid-integer? x) (fixnum? x))

;; Zero the bits of k at and below (BE) the set bit of m.
(define (mask k m)
  (if (fx= m fx-least)
      0
      (fxand k (fxxor (fxnot (fx- m 1)) m))))

;; Does the m-masked prefix of k match p?
(define (match-prefix? k p m)
  (fx= (mask k m) p))

(define (branching-bit p1 m1 p2 m2)
  (if (fxnegative? (fxxor p1 p2))
      fx-least        ; different signs
      (highest-bit-mask (fxxor p1 p2) (fxmax 1 (fx* 2 (fxmax m1 m2))))))

;; Two's-complement trick.
(define (lowest-set-bit b)
  (fxand b (fxneg b)))

(define (highest-bit-mask k guess-m)
  (let lp ((x (fxand k (fxnot (fx- guess-m 1)))))
    (let ((m (lowest-set-bit x)))
      (if (fx= x m)
          m
          (lp (fx- x m))))))

(define (zero-bit? k m)
  (fxzero? (fxand k m)))

;; Insert the association (key, value) into trie, replacing any old
;; association.
(define (trie-insert trie key value)
  (trie-insert/combine trie key value (lambda (_k new _) new)))

;; Insert the association (key, value) into trie, preserving any old
;; association.
(define (trie-adjoin trie key value)
  (trie-insert/combine trie key value (lambda (_k _new old) old)))

;; Insert (key, value) into trie if key doesn't already have an
;; association.  If it does, add a new association for key and
;; the result of calling combine on the key, new, and old values.
(define (trie-insert/combine trie key value combine)
  (letrec
   ((new-leaf (leaf key value))
    (insert
     (lambda (t)
       (tmatch t
         (empty (leaf key value))
         ((leaf ,k ,v)
          (if (fx= key k)
              (leaf k (combine k value v))
              (trie-join key 0 new-leaf k 0 t)))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? key p m)
              (if (zero-bit? key m)
                  (branch p m (insert l) r)
                  (branch p m l (insert r)))
              (trie-join key 0 new-leaf p m t)))))))
    (assume (valid-integer? key) "invalid key")
    (insert trie)))

(define (trie-join prefix1 mask1 trie1 prefix2 mask2 trie2)
  (let ((m (branching-bit prefix1 mask1 prefix2 mask2)))
    (if (zero-bit? prefix1 m)
        (branch (mask prefix1 m) m trie1 trie2)
        (branch (mask prefix1 m) m trie2 trie1))))

;; If (key, value) is an association in trie, then replace it
;; with (key, (proc key value)).  Otherwise, return a copy of trie.
(define (trie-adjust trie key proc)
  (letrec
   ((update
     (lambda (t)
       (tmatch t
         (empty t)
         ((leaf ,k ,v)
          (if (fx= key k) (leaf k (proc k v)) t))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? key p m)
              (if (zero-bit? key m)
                  (branch p m (update l) r)
                  (branch p m l (update r)))
              t))))))
    (update trie)))

(define (trie-update trie key proc failure wrapper)
  (letrec
   ((update
     (lambda (t build)
       (tmatch t
         (empty (failure))
         ((leaf ,k ,v)
          (if (fx= key k)
              (proc k
                    v
                    (lambda (v*) (wrapper (build (leaf k v*))))
                    (lambda () (wrapper (build the-empty-trie))))
              (failure)))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? key p m)
              (if (zero-bit? key m)
                  (update l (lambda (l*) (build (branch p m l* r))))
                  (update r (lambda (r*) (build (branch p m l r*)))))
              (failure)))))))
    (update trie values)))

(define (trie-alter trie key failure success wrapper)
  (letrec
   ((update
     (lambda (t build)
       (tmatch t
         (empty
          (failure (lambda (v)
                     (wrapper (build (leaf key v))))     ; insert
                   (lambda ()
                     (wrapper (build the-empty-trie))))) ; ignore
         ((leaf ,k ,v)
          (if (fx= key k)
              (success k
                       v
                       (lambda (v*)                      ; replace
                         (wrapper (build (leaf k v*))))
                       (lambda ()                        ; delete
                         (wrapper (build the-empty-trie))))
              (failure (lambda (u)                       ; insert
                         (wrapper
                          (build (trie-join key 0 (leaf key u) k 0 t))))
                       (lambda ()                        ; ignore
                         (wrapper (build t))))))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? key p m)
              (if (zero-bit? key m)
                  (update l (lambda (l*)
                              (build (branch p m l* r))))
                  (update r (lambda (r*)
                              (build (branch p m l r*)))))
              (failure (lambda (v)                    ; insert
                         (wrapper
                          (build (trie-join key 0 (leaf key v) p m t))))
                       (lambda ()                     ; ignore
                         (wrapper (build t))))))))))
    (update trie values)))

;; If `key' has an association in `trie', then call `success' with
;; on the associated value.  Otherwise, call `failure'.
(define (trie-assoc trie key failure success)
  (letrec
   ((search
     (tmatch-lambda
       ((leaf ,k ,v) (guard (fx= k key)) (success v))
       ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
        (if (zero-bit? key m) (search l) (search r)))
       (else (failure)))))
    (search trie)))

;; If `key' has an association in `trie', then return the associated
;; value.  Otherwise, return `default'.
(define (trie-assoc/default trie key default)
  (letrec
   ((search
     (tmatch-lambda
       ((leaf ,k ,v) (guard (fx= k key)) v)
       ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
        (if (zero-bit? key m) (search l) (search r)))
       (else default))))
    (search trie)))

;; Return the number of associations in trie.
(define (trie-size trie)
  (if (trie-empty? trie)
      0
      (let lp ((n 0) (t trie) (kont values))
        (cond ((leaf? t) (kont (+ n 1)))
              (else (lp n
                        (branch-left t)
                        (lambda (m)
                          (lp m (branch-right t) kont))))))))

(define (trie-contains? trie key)
  (letrec
   ((search
     (tmatch-lambda
       ((leaf ,k ,v) (fx= k key))
       ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
        (if (zero-bit? key m) (search l) (search r)))
       (else #f))))
    (search trie)))

(define (trie-find pred trie failure success)
  (letrec
   ((search
     (lambda (t kont)
       (tmatch t
         ((leaf ,k ,v) (guard (pred k v)) (success k v))
         ((branch ? ? ,l ,r) (search l (lambda () (search r kont))))
         (else (kont))))))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (negative? m))
       (search r (lambda () (search l failure))))
      (else (search trie failure)))))

(define (branching-bit-higher? mask1 mask2)
  (if (negative? (fxxor mask1 mask2))  ; signs differ
      (negative? mask1)
      (fx> mask1 mask2)))

;; Merge two tries.  `combine' is used to merge duplicated mappings.
(define (trie-merge combine trie1 trie2)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((trie-empty? s) t)
              ((trie-empty? t) s)
              ((leaf? s)
               (trie-insert/combine t (leaf-key s) (leaf-value s) combine))
              ((leaf? t)
               (trie-insert/combine s
                                    (leaf-key t)
                                    (leaf-value t)
                                    (swap-last-args combine)))
              ((and (branch? s) (branch? t)) (merge-branches s t)))))
     (merge-branches
      (lambda (s t)
        (tmatch s
          ((branch ,p ,m ,s1 ,s2)
           (tmatch t
             ((branch ,q ,n ,t1 ,t2)
              (cond ((and (fx= m n) (fx= p q))
                     (branch p m (merge s1 t1) (merge s2 t2)))
                    ((and (branching-bit-higher? m n)
                          (match-prefix? q p m))
                     (if (zero-bit? q m)
                         (branch p m (merge s1 t) s2)
                         (branch p m s1 (merge s2 t))))
                    ((and (branching-bit-higher? n m)
                          (match-prefix? p q n))
                     (if (zero-bit? p n)
                         (branch q n (merge s t1) t2)
                         (branch q n t1 (merge s t2))))
                    (else (trie-join p m s q n t))))))))))
    (merge trie1 trie2)))

;; Construct a branch only if the subtrees are non-empty.
(define (branch prefix mask trie1 trie2)
  (cond ((not trie1) trie2)
        ((not trie2) trie1)
        (else (raw-branch prefix mask trie1 trie2))))

(define (trie-union s t)
  (trie-merge trie-insert s t))

(define (trie-partition pred trie)
  (letrec
   ((part
     (lambda (t)
       (tmatch t
         (empty (values the-empty-trie the-empty-trie))
         ((leaf ,k ,v)
          (if (pred k v)
              (values t the-empty-trie)
              (values the-empty-trie t)))
         ((branch ,p ,m ,l ,r)
          (let-values (((il ol) (part l))
                       ((ir or) (part r)))
            (values (branch p m il ir) (branch p m ol or))))))))
    (part trie)))

;;;; Map and fold

(define (trie-map proc trie)
  (letrec
   ((tmap
     (tmatch-lambda
       (empty the-empty-trie)
       ((leaf ,k ,v)
        (leaf k (proc k v)))
       ((branch ,p ,m ,l ,r)
        (branch p m (tmap l) (tmap r))))))
    (tmap trie)))

(define (trie-fold-left proc nil trie)
  (if (trie-empty? trie)
      nil
      (let lp ((t trie) (b nil) (kont values))
        (if (leaf? t)
            (kont (proc (leaf-key t) (leaf-value t) b))
            (lp (branch-left t)
                b
                (lambda (c)
                  (lp (branch-right t) c kont)))))))

(define (trie-fold-right proc nil trie)
  (if (trie-empty? trie)
      nil
      (let lp ((t trie) (b nil) (kont values))
        (if (leaf? t)
            (kont (proc (leaf-key t) (leaf-value t) b))
            (lp (branch-right t)
                b
                (lambda (c)
                  (lp (branch-left t) c kont)))))))

(define (trie-filter pred trie)
  (letrec ((filter
            (lambda (t)
              (tmatch t
                (empty the-empty-trie)
                ((leaf ,k ,v) (guard (pred k v)) t)
                ((leaf ? ?) the-empty-trie)
                ((branch ,p ,m ,l ,r)
                 (branch p m (filter l) (filter r)))))))
    (filter trie)))

(define (trie-min trie)
  (letrec
   ((search
     (tmatch-lambda
       ((leaf ,k ,v) (values k v))
       ((branch ? ? ,l ?) (search l)))))
    (tmatch trie
      (empty (error "empty trie"))
      ((leaf ,k ,v) (values k v))
      ((branch ? ,m ,l ,r)
       (if (fxnegative? m) (search r) (search l))))))

(define (trie-update-min trie success wrapper)
  (letrec
   ((update
     (lambda (t build)
       (tmatch t
         (empty (wrapper (build the-empty-trie)))
         ((leaf ,k ,v)
          (success k
                   v
                   (lambda (v*)
                     (wrapper (build (leaf k v*))))
                   (lambda ()
                     (wrapper (build the-empty-trie)))))
         ((branch ,p ,m ,l ,r)
          (update l (lambda (l*)
                      (build (branch p m l* r)))))))))
    (tmatch trie
      ((branch ,p ,m ,l ,r)
       (if (negative? m)
           (update r (lambda (r*) (branch p m l r*)))
           (update l (lambda (l*) (branch p m l* r)))))
      (else (update trie values)))))

(define (trie-pop-min trie)
  (letrec
   ((pop
     (tmatch-lambda
       (empty (error "trie-pop-min: empty trie"))
       ((leaf ,k ,v) (values k v the-empty-trie))
       ((branch ,p ,m ,l ,r)
        (let-values (((k v l*) (pop l)))
          (values k v (branch p m l* r)))))))
    (tmatch trie
      ((branch ,p ,m ,l ,r)
       (if (fxnegative? m)
           (let-values (((k v r*) (pop r)))
             (values k v (branch p m l r*)))
           (let-values (((k v l*) (pop l)))
             (values k v (branch p m l* r)))))
      (else (pop trie)))))

(define (trie-max trie)
  (letrec
   ((search
     (tmatch-lambda
       ((leaf ,k ,v) (values k v))
       ((branch ? ? ? ,r) (search r)))))
    (tmatch trie
      (empty (error "empty trie"))
      ((branch ? ,m ,l ,r)
       (if (fxnegative? m) (search l) (search r)))
      ((leaf ,k ,v) (values k v)))))

(define (trie-update-max trie success wrapper)
  (letrec
   ((update
     (lambda (t build)
       (tmatch t
         (empty (wrapper (build the-empty-trie)))
         ((leaf ,k ,v)
          (success k
                   v
                   (lambda (v*)
                     (wrapper (build (leaf k v*))))
                   (lambda ()
                     (wrapper (build the-empty-trie)))))
         ((branch ,p ,m ,l ,r)
          (update r (lambda (r*)
                      (build (branch p m l r*)))))))))
    (tmatch trie
      ((branch ,p ,m ,l ,r)
       (if (negative? m)
           (update l (lambda (l*) (branch p m l* r)))
           (update r (lambda (r*) (branch p m l r*)))))
      (else (update trie values)))))

(define (trie-pop-max trie)
  (letrec
   ((pop
     (tmatch-lambda
       (empty (error "trie-pop-max: empty trie"))
       ((leaf ,k ,v) (values k v the-empty-trie))
       ((branch ,p ,m ,l ,r)
        (let-values (((k v r*) (pop r)))
          (values k v (branch p m l r*)))))))
    (tmatch trie
      ((branch ,p ,m ,l ,r)
       (if (fxnegative? m)
           (let-values (((k v l*) (pop l)))
             (values k v (branch p m l* r)))
           (let-values (((k v r*) (pop r)))
             (values k v (branch p m l r*)))))
      (else (pop trie)))))

;;;; Comparisons

(define (trie=? comp trie1 trie2)
  (let loop ((s trie1) (t trie2))
    (cond ((and (trie-empty? s) (trie-empty? t)) #t)
          ((leaf? s)
           (and (leaf? t)
                (fx= (leaf-key s) (leaf-key t))
                (=? comp (leaf-value s) (leaf-value t))))
          ((and (branch? s) (branch? t))
           (tmatch s
            ((branch ,p ,m ,l1 ,r1)
             (tmatch t
               ((branch ,q ,n ,l2 ,r2)
                (and (fx= m n)
                     (fx= p q)
                     (loop l1 l2)
                     (loop r1 r2)))))))
          (else #f))))

;; Returns the symbol 'less' if trie1 is a proper subset of trie2,
;; 'equal' if they are the same, and 'greater' otherwise.  NB that
;; disjoint mappings will compare as greater.
(define (trie-subset-compare comp trie1 trie2)
  (letrec
   ((compare
     (lambda (s t)
       (cond ((eqv? s t) 'equal)
             ((trie-empty? s) 'less)
             ((trie-empty? t) 'greater)  ; disjoint
             ((and (leaf? s) (leaf? t))
              (if (and (fx= (leaf-key s) (leaf-key t))
                       (=? comp (leaf-value s) (leaf-value t)))
                  'equal
                  'greater))
             ((leaf? s)             ; leaf / branch
              (tmatch t
                ((branch ,p ,m ,l ,r)
                 (let ((k (leaf-key s)))
                   (when (match-prefix? k p m)
                       (case (compare s (if (zero-bit? k m) l r))
                         ((greater) 'greater)
                         (else 'less)))))))
             ((leaf? t) 'greater)   ; branch / leaf
             (else (compare-branches s t)))))
    (compare-branches
     (lambda (s t)
       (tmatch s
         ((branch ,p ,m ,sl ,sr)
          (tmatch t
            ((branch ,q ,n ,tl ,tr)
             (cond ((branching-bit-higher? m n) 'greater)
                   ((branching-bit-higher? n m)
                    (if (match-prefix? p q n)
                        (let ((res (if (zero-bit? p n)
                                       (compare s tl)
                                       (compare s tr))))
                          (if (eqv? res 'greater) res 'less))
                        'greater))
                   ((fx= p q)  ; same prefix, compare subtrees
                    (let ((cl (compare sl tl)) (cr (compare sr tr)))
                      (cond ((or (eqv? cl 'greater) (eqv? cr 'greater))
                             'greater)
                            ((and (eqv? cl 'equal) (eqv? cr 'equal))
                             'equal)
                            (else 'less))))
                   (else 'greater)))))))))  ; disjoint
    (compare trie1 trie2)))

(define (trie-proper-subset? comp trie1 trie2)
  (eqv? (trie-subset-compare comp trie1 trie2) 'less))

;; Two tries are disjoint if they have no keys in common.
(define (trie-disjoint? trie1 trie2)
  (letrec
   ((disjoint?
     (lambda (s t)
       (or (trie-empty? s)
           (trie-empty? t)
           (cond ((leaf? s)
                  (let ((k (leaf-key s)))
                    (if (leaf? t)
                        (not (fx= k (leaf-key t)))
                        (not (trie-contains? t k)))))
                 ((leaf? t) (not (trie-contains? s (leaf-key t))))
                 (else (branches-disjoint? s t))))))
    (branches-disjoint?
     (lambda (s t)
       (tmatch s
         ((branch ,p ,m ,sl ,sr)
          (tmatch t
            ((branch ,q ,n ,tl ,tr)
             (cond ((and (fx= m n) (fx= p q))
                    (and (disjoint? sl tl) (disjoint? sr tr)))
                   ((and (branching-bit-higher? m n)
                         (match-prefix? q p m))
                    (if (zero-bit? q m)
                        (disjoint? sl t)
                        (disjoint? sr t)))
                   ((and (branching-bit-higher? n m)
                         (match-prefix? p q n))
                    (if (zero-bit? p n)
                        (disjoint? s tl)
                        (disjoint? s tr)))
                   (else #t)))))))))      ; the prefixes disagree
    (disjoint? trie1 trie2)))

(define (trie-delete trie key)
  (letrec
   ((update
     (lambda (t)
       (tmatch t
         (empty the-empty-trie)
         ((leaf ,k ?) (if (fx= k key) the-empty-trie t))
         ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
          (if (zero-bit? key m)
               (branch p m (update l) r)
               (branch p m l (update r))))
         (else t)))))  ; key doesn't occur in t
    (update trie)))

(define (trie-intersection combine trie1 trie2)
  (letrec
   ((intersect
     (lambda (s t)
       (cond ((or (trie-empty? s) (trie-empty? t)) the-empty-trie)
             ((and (leaf? s) (leaf? t))
              (tmatch s
                ((leaf ,sk ,sv)
                 (tmatch t
                   ((leaf ,tk ,tv) (guard (fx= sk tk))
                    (leaf sk (combine sk sv tv)))
                   (else the-empty-trie)))))
             ((leaf? s) (insert-leaf combine s t))
             ((leaf? t) (insert-leaf (swap-last-args combine) t s))
             (else (intersect-branches s t)))))
    (insert-leaf
     (lambda (comb lf t)
       (tmatch lf
         ((leaf ,k ,v)
          (let lp ((t t))
            (tmatch t
              ((leaf ,tk ,tv) (guard (fx= k tk))
               (leaf k (comb k v tv)))
              ((leaf ? ?) the-empty-trie)
              ((branch ,p ,m ,l ,r)
               (and (match-prefix? k p m)
                    (if (zero-bit? k m) (lp l) (lp r))))
              (else the-empty-trie)))))))
    (intersect-branches
     (lambda (s t)
       (tmatch s
         ((branch ,p ,m ,sl ,sr)
          (tmatch t
            ((branch ,q ,n ,tl ,tr)
             (cond ((branching-bit-higher? m n)
                    (and (match-prefix? q p m)
                         (if (zero-bit? q m)
                             (intersect sl t)
                             (intersect sr t))))
                   ((branching-bit-higher? n m)
                    (and (match-prefix? p q n)
                         (if (zero-bit? p n)
                             (intersect s tl)
                             (intersect s tr))))
                   ((fx= p q)
                    (branch p m (intersect sl tl) (intersect sr tr)))
                   (else the-empty-trie)))))))))
    (intersect trie1 trie2)))

(define (trie-difference trie1 trie2)
  (letrec
   ((difference
     (lambda (s t)
       (cond ((trie-empty? s) the-empty-trie)
             ((trie-empty? t) s)
             ((leaf? s)
              (trie-assoc t
                          (leaf-key s)
                          (lambda () s)
                          (lambda (_) the-empty-trie)))
             ((leaf? t) (trie-delete s (leaf-key t)))
             (else (branch-difference s t)))))
    (branch-difference
     (lambda (s t)
       (tmatch s
         ((branch ,p ,m ,sl ,sr)
          (tmatch t
            ((branch ,q ,n ,tl ,tr)
             (cond ((and (fx= m n) (fx= p q))
                    (branch p m (difference sl tl) (difference sr tr)))
                   ((and (branching-bit-higher? m n)
                         (match-prefix? q p m))
                    (if (zero-bit? q m)
                        (branch p m (difference sl t) sr)
                        (branch p m sl (difference sr t))))
                   ((and (branching-bit-higher? n m)
                         (match-prefix? p q n))
                    (if (zero-bit? p n)
                        (difference s tl)
                        (difference s tr)))
                   (else s)))))))))
    (difference trie1 trie2)))

;; Remove the assoc for key if it exists in trie; otherwise, add the
;; assoc (key, value).
(define (%trie-insert-xor trie key value)
  (trie-alter trie
              key
              (lambda (insert _ig) (insert value))
              (lambda (_k _v _rep delete) (delete))
              values))

(define (trie-xor trie1 trie2)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((trie-empty? s) t)
              ((trie-empty? t) s)
              ((leaf? s)
               (%trie-insert-xor t (leaf-key s) (leaf-value s)))
              ((leaf? t)
               (%trie-insert-xor s (leaf-key t) (leaf-value t)))
              (else (merge-branches s t)))))
     (merge-branches
      (lambda (s t)
        (tmatch s
          ((branch ,p ,m ,s1 ,s2)
           (tmatch t
             ((branch ,q ,n ,t1 ,t2)
              (cond ((and (fx= m n) (fx= p q))
                     (branch p m (merge s1 t1) (merge s2 t2)))
                    ((and (branching-bit-higher? m n) (match-prefix? q p m))
                     (if (zero-bit? q m)
                         (branch p m (merge s1 t) s2)
                         (branch p m s1 (merge s2 t))))
                    ((and (branching-bit-higher? n m) (match-prefix? p q n))
                     (if (zero-bit? p n)
                         (branch q n (merge s t1) t2)
                         (branch q n t1 (merge s t2))))
                    (else
                     (trie-join p m s q n t))))))))))
    (merge trie1 trie2)))

;; Return a trie containing all the elements of `trie' which are
;; less than k, if `inclusive' is false, or less than or equal to
;; k if `inclusive' is true.
(define (subtrie< trie k inclusive)
  (letrec
    ((split
      (lambda (t)
        (tmatch t
          (empty the-empty-trie)
          ((leaf ,tk ?)
           (cond ((fx< tk k) t)
                 ((and (fx= tk k) inclusive) t)
                 (else the-empty-trie)))
          ((branch ,p ,m ,l ,r)
           (if (match-prefix? k p m)
               (if (zero-bit? k m)
                   (split l)
                   (trie-union l (split r)))
               (and (fx< p k) t)))))))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (fxnegative? m))
       (if (fxnegative? k) (split r) (trie-union (split l) r)))
      (else (split trie)))))

;; Return a trie containing all the elements of `trie' which are
;; greater than k, if `inclusive' is false, or greater than or equal
;; to k if `inclusive' is true.
(define (subtrie> trie k inclusive)
  (letrec
   ((split
     (lambda (t)
       (tmatch t
         (empty the-empty-trie)
         ((leaf ,tk ?)
          (cond ((fx> tk k) t)
                ((and (fx= tk k) inclusive) t)
                (else the-empty-trie)))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? k p m)
              (if (zero-bit? k m)
                  (trie-union (split l) r)
                  (split r))
              (and (fx> p k) t)))))))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (fxnegative? m))
       (if (fxnegative? k) (trie-union (split r) l) (split l)))
      (else (split trie)))))

;; Return a trie containing all the elements of `trie' which are
;; greater than/greater than or equal to a and less than/less than
;; or equal to b, depending on the truth values of
;; low-/high-inclusive.
(define (subtrie-interval trie a b low-inclusive high-inclusive)
  (letrec
   ((interval
     (lambda (t)
       (tmatch t
         (empty the-empty-trie)
         ((leaf ,tk ?)
          (and ((if low-inclusive fx>= fx>) tk a)
               ((if high-inclusive fx<= fx<) tk b)
               t))
         (else (branch-interval t)))))
    (branch-interval
     (lambda (t)
       (tmatch t
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? a p m)
              (if (zero-bit? a m)
                  (if (match-prefix? b p m)
                      (if (zero-bit? b m)
                          (interval l)  ; all x < b is in l
                          (trie-union (subtrie> l a low-inclusive)
                                      (subtrie< r b high-inclusive)))
                      ;; everything or nothing is less than b
                      (and (fx< b p)
                           (trie-union (subtrie> l a low-inclusive) r)))
                  (interval r)) ; all x > b is in r
              ;; everything or nothing is greater than a
              (and (fx> p a) (subtrie< t b high-inclusive))))))))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (fxnegative? m))
       (cond ((and (fxnegative? a) (fxnegative? b)) (interval r))
             ((and (fxpositive? a) (fxpositive? b)) (interval l))
              ;; (a, 0) U (0, b)
              (else (trie-union (subtrie> r a low-inclusive)
                                (subtrie< l b high-inclusive)))))
      (else (interval trie)))))

(define (trie-split trie pivot)
  (letrec
   ((split
     (lambda (t)
       (tmatch t
         ((leaf ,k ,v)
          (if (fx<= k pivot)
              (values t the-empty-trie)
              (values the-empty-trie t)))
         ((branch ,p ,m ,l ,r)
          (if (match-prefix? pivot p m)
              (if (zero-bit? pivot m)
                  (let-values (((ta tb) (split l)))
                    (values ta (trie-union tb r)))
                  (let-values (((ta tb) (split r)))
                    (values (trie-union l ta) tb)))
              (if (fx<= p pivot)
                  (values t the-empty-trie)
                  (values the-empty-trie t))))))))

    (tmatch trie
      (empty (values the-empty-trie the-empty-trie))
      ((branch ? ,m ,l ,r) (guard (fxnegative? m))
       (if (fxnegative? pivot)
           (let-values (((ta tb) (split r)))
             (values ta (trie-union tb l)))
           (let-values (((ta tb) (split l)))
             (values (trie-union r ta) tb))))
      (else (split trie)))))

;;;; Tries as (Integer, *) relations

(define (trie-relation-map proc trie)
  (trie-fold-left (lambda (k v t)
                    (let-values (((k* v*) (proc k v)))
                      (assume (valid-integer? k*))
                      (trie-insert t k* v*)))
                  the-empty-trie
                  trie))

;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; Utility

(define (plist-fold proc nil ps)
  (let loop ((b nil) (ps ps))
    (pmatch ps
      (() b)
      ((,k ,v . ,ps*)
       (loop (proc k v b) ps*))
      (else (error "plist-fold: invalid plist")))))

(define (first-arg _k x _y) x)
(define (second-arg _k _x y) y)

(define (constantly x)
  (lambda (_) x))

;;;; Type

;; Functions to use with the gen:dict struct interface
(define (fx-dict-ref d key [failure (lambda () (raise-arguments-error 'dict-ref "No value found for key" "key" key))])
  (unless (fixnum? key)
    (raise-argument-error 'dict-ref "fixnum?" key))
  (if (procedure? failure)
      (fxmapping-ref d key failure)
      (fxmapping-ref/default d key failure)))
(define (fx-dict-set d key v)
  (unless (fixnum? key)
    (raise-argument-error 'dict-set "fixnum?" key))
  (fxmapping-set d key v))

(define (check-keys kvs [index 0])
  (cond
    ((null? kvs) #f)
    ((fixnum? (car kvs))
     (check-keys (cddr kvs) (+ index 2)))
    (else index)))
(define (fx-dict-set* d . kvs)
  (unless (even? (length kvs))
    (raise-arguments-error 'dict-set* "An even number of key + value arguments is needed."))
  (let ([bad-pos (check-keys kvs 1)])
    (when bad-pos
      (apply raise-argument-error 'dict-set* "fixnum?" bad-pos d kvs)))
  (apply fxmapping-set d kvs))
(define (fx-dict-remove d key)
    (unless (fixnum? key)
      (raise-argument-error 'dict-remove "fixnum?" key))
  (fxmapping-delete d key))
(define (fx-dict-has-key? d key)
  (unless (fixnum? key)
    (raise-argument-error 'dict-has-key? "fixnum?" key))
  (fxmapping-contains? d key))
(define (fx-dict-map d proc) (fxmapping-map->list proc d))
(define (fx-dict-map/copy d proc) (fxmapping-relation-map proc d))
(define (fx-dict-for-each d proc) (fxmapping-for-each proc d))
(define (fx-dict-empty? d) (fxmapping-empty? d))
(define (fx-dict-count d) (fxmapping-size d))
(define (fx-dict-clear d) (fxmapping))
(define (fx-dict-keys d) (fxmapping-keys d))
(define (fx-dict-values d) (fxmapping-values d))
(define (fx-dict->list d) (fxmapping->alist d))

(struct fx-dict-iter (mapping stream))
(define (fx-dict-iterate-first d)
  (if (fxmapping-empty? d)
      #f
      (fx-dict-iter d (generator->stream (fxmapping->generator d)))))
(define (fx-dict-iterate-next d pos)
  (unless (fx-dict-iter? pos)
    (raise-argument-error 'dict-iterate-next "fx-dict-iter?" pos))
  (unless (eq? d (fx-dict-iter-mapping pos))
    (raise-arguments-error 'dict-iterate-next "iterator from a different fxmapping"
                           "fxmapping" d "iterator" pos))
  (let ([nxt (stream-cdr (fx-dict-iter-stream pos))])
    (if (stream-null? nxt)
        #f
        (struct-copy fx-dict-iter pos [stream nxt]))))
(define (fx-dict-iterate-key d pos)
  (unless (fx-dict-iter? pos)
    (raise-argument-error 'dict-iterate-key "fx-dict-iter?" pos))
  (unless (eq? d (fx-dict-iter-mapping pos))
    (raise-arguments-error 'dict-iterate-key "iterator from a different fxmapping"
                           "fxmapping" d "iterator" pos))
  (car (stream-car (fx-dict-iter-stream pos))))
(define (fx-dict-iterate-value d pos)
  (unless (fx-dict-iter? pos)
    (raise-argument-error 'dict-iterate-value "fx-dict-iter?" pos))
  (unless (eq? d (fx-dict-iter-mapping pos))
    (raise-arguments-error 'dict-iterate-value "iterator from a different fxmapping"
                           "fxmapping" d "iterator" pos))
  (cdr (stream-car (fx-dict-iter-stream pos))))

(struct fxmapping (trie)
  #:name <fxmapping>
  #:constructor-name raw-fxmapping
  ; Unfortunately you can't combine prop:dict/contract and gen:dict
  ;#:property prop:dict/contract
  #;(list
   (vector-immutable fx-dict-ref #f fx-dict-set #f fx-dict-remove fx-dict-count
                     fx-dict-iterate-first fx-dict-iterate-next fx-dict-iterate-key fx-dict-iterate-value)
   (vector-immutable fixnum? any/c stream? #f #f #f))
  #:methods gen:dict
  [(define dict-ref fx-dict-ref)
   (define dict-iterate-first fx-dict-iterate-first)
   (define dict-iterate-next fx-dict-iterate-next)
   (define dict-iterate-key fx-dict-iterate-key)
   (define dict-iterate-value fx-dict-iterate-value)
   (define dict-set fx-dict-set)
   (define dict-set* fx-dict-set*)
   (define dict-remove fx-dict-remove)
   (define dict-has-key? fx-dict-has-key?)
   (define dict-map fx-dict-map)
   (define dict-map/copy fx-dict-map/copy)
   (define dict-for-each fx-dict-for-each)
   (define dict-empty? fx-dict-empty?)
   (define dict-clear fx-dict-clear)
   (define dict-keys fx-dict-keys)
   (define dict-values fx-dict-values)
   (define dict->list fx-dict->list)]

  ;;; Compare/hash the keys and contents, not the underlying structure of the trie
  #:methods gen:equal+hash
  [(define (equal-proc a b my-equal?)
     (or (eqv? a b)
         (trie=? (make-comparator (lambda (x) #t) my-equal? #f #f) (fxmapping-trie a) (fxmapping-trie b))))
   (define (hash-proc a my-hash-code)
     (trie-fold-left (lambda (k v hash)
                       (hash-code-combine hash (my-hash-code k) (my-hash-code v))) 0 (fxmapping-trie a)))
   (define (hash2-proc a my-hash2-code)
     (trie-fold-left (lambda (k v hash)
                       (hash-code-combine hash (my-hash2-code k) (my-hash2-code v))) 0 (fxmapping-trie a)))])

;;;; Constructors

(define (fxmapping . args)
  (raw-fxmapping
    (plist-fold (lambda (k v trie) (trie-adjoin trie k v))
                the-empty-trie
                args)))

(define (pair-or-null? x)
  (or (pair? x) (null? x)))

(define (alist->fxmapping/combinator comb as)
  (raw-fxmapping
    (fold (lambda (p trie)
            (assume (pair? p) "alist->fxmapping/combinator: not a pair")
            (trie-insert/combine trie (car p) (cdr p) comb))
          the-empty-trie
          as)))

(define (alist->fxmapping as)
  (alist->fxmapping/combinator second-arg as))

(define fxmapping-unfold
  (case-lambda
    ((stop? mapper successor seed)                ; fast path
     (let lp ((trie the-empty-trie) (seed seed))
       (if (stop? seed)
           (raw-fxmapping trie)
           (let-values (((k v) (mapper seed)))
             (assume (valid-integer? k))
             (lp (trie-adjoin trie k v) (successor seed))))))
    ((stop? mapper successor . seeds)             ; variadic path
     (let lp ((trie the-empty-trie) (seeds seeds))
       (if (apply stop? seeds)
           (raw-fxmapping trie)
           (let-values (((k v) (apply mapper seeds))
                        ((seeds*) (values->list (apply successor seeds))))
             (assume (valid-integer? k))
             (lp (trie-adjoin trie k v) seeds*)))))))

(define (fxmapping-accumulate proc seed . seeds)
  (define prompt-tag (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (lambda ()
     (if (null? seeds)
         ; fast path
         (call-with-current-continuation
          (lambda (k)
            (let lp ((trie the-empty-trie) (seed seed))
              (let-values (((k v seed*)
                            (proc (lambda xs (apply k (raw-fxmapping trie) xs))
                                  seed)))
                (lp (trie-adjoin trie k v) seed*))))
          prompt-tag)
         ; variadic path
         (call-with-current-continuation
          (lambda (k)
            (let lp ((trie the-empty-trie) (seeds (cons seed seeds)))
              (let-values (((k v seeds*)
                            (call-with-values
                             (lambda () (apply proc
                                               (lambda xs (apply k (raw-fxmapping trie) xs))
                                               seeds))
                             (lambda (k v . seeds*) (values k v seeds*)))))
                (lp (trie-adjoin trie k v) seeds*))))
          prompt-tag)))
   prompt-tag))

;;;; Predicates

(define (fxmapping-contains? fxmap n)
  (trie-contains? (fxmapping-trie fxmap) n))

(define (fxmapping-empty? fxmap)
  (eqv? (fxmapping-trie fxmap) the-empty-trie))

(define (fxmapping-disjoint? fxmap1 fxmap2)
  (trie-disjoint? (fxmapping-trie fxmap1) (fxmapping-trie fxmap2)))

;;;; Accessors

(define fxmapping-ref
  (case-lambda
    ((fxmap key)
     (fxmapping-ref fxmap
                    key
                    (lambda () (raise-arguments-error 'fxmapping-ref "key not found" "key" key))
                    values))
    ((fxmap key failure)
     (fxmapping-ref fxmap key failure values))
    ((fxmap key failure success)
     (trie-assoc (fxmapping-trie fxmap) key failure success))))

(define (fxmapping-ref/default fxmap key default)
  (trie-assoc/default (fxmapping-trie fxmap) key default))

(define (fxmapping-min fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-min (fxmapping-trie fxmap)))

(define (fxmapping-max fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-max (fxmapping-trie fxmap)))

;;;; Updaters

(define fxmapping-adjoin/combinator
  (case-lambda
    ((fxmap combine key value)      ; one-assoc fast path
     (raw-fxmapping
      (trie-insert/combine (fxmapping-trie fxmap) key value combine)))
    ((fxmap combine . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t)
                    (trie-insert/combine t k v combine))
                  (fxmapping-trie fxmap)
                  ps)))))

;; Preserve existing associations for keys.
(define fxmapping-adjoin
  (case-lambda
    ((fxmap key value)              ; one-assoc fast path
     (raw-fxmapping
      (trie-adjoin (fxmapping-trie fxmap) key value)))
    ((fxmap . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t) (trie-adjoin t k v))
                  (fxmapping-trie fxmap)
                  ps)))))

;; Replace existing associations for keys.
(define fxmapping-set
  (case-lambda
    ((fxmap key value)      ; one-assoc fast path
     (raw-fxmapping
      (trie-insert (fxmapping-trie fxmap) key value)))
    ((fxmap . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t) (trie-insert t k v))
                  (fxmapping-trie fxmap)
                  ps)))))

(define (fxmapping-adjust fxmap key proc)
  (raw-fxmapping (trie-adjust (fxmapping-trie fxmap) key proc)))

(define fxmapping-delete
  (case-lambda
    ((fxmap key)      ; fast path
     (raw-fxmapping (trie-delete (fxmapping-trie fxmap) key)))
    ((fxmap . keys)
     (fxmapping-delete-all fxmap keys))))

(define (fxmapping-delete-all fxmap keys)
  (assume (or (pair? keys) (null? keys)))
  (let ((key-map (fxmapping-trie
                  (fxmapping-unfold null?
                                    (lambda (ks) (values (car ks) #t))
                                    cdr
                                    keys))))
    (fxmapping-remove (lambda (k _) (trie-contains? key-map k))
                      fxmap)))

(define fxmapping-update
  (case-lambda
    ((fxmap key success)
     (fxmapping-update fxmap
                       key
                       success
                       (lambda ()
                         (raise-arguments-error 'fxmapping-update "key not found" "key" key))))
    ((fxmap key success failure)
     (trie-update (fxmapping-trie fxmap) key success failure raw-fxmapping))))

(define (fxmapping-alter fxmap key failure success)
  (trie-alter (fxmapping-trie fxmap) key failure success raw-fxmapping))

(define (fxmapping-delete-min fxmap)
  (fxmapping-update-min fxmap
                        (lambda (_k _v _rep delete)
                          (delete))))

(define (fxmapping-update-min fxmap success)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-update-min (fxmapping-trie fxmap) success raw-fxmapping))

(define (fxmapping-pop-min fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (let-values (((k v trie) (trie-pop-min (fxmapping-trie fxmap))))
    (values k v (raw-fxmapping trie))))

(define (fxmapping-delete-max fxmap)
  (fxmapping-update-max fxmap
                        (lambda (_k _v _rep delete)
                          (delete))))

(define (fxmapping-update-max fxmap success)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-update-max (fxmapping-trie fxmap) success raw-fxmapping))

(define (fxmapping-pop-max fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (let-values (((k v trie) (trie-pop-max (fxmapping-trie fxmap))))
    (values k v (raw-fxmapping trie))))

;;;; The whole fxmapping

(define (fxmapping-size fxmap)
  (trie-size (fxmapping-trie fxmap)))

(define fxmapping-find
  (case-lambda
    ((pred fxmap failure)
     (fxmapping-find pred fxmap failure values))
    ((pred fxmap failure success)
     (trie-find pred (fxmapping-trie fxmap) failure success))))

(define (fxmapping-count pred fxmap)
  (fxmapping-fold (lambda (k v acc)
                    (if (pred k v) (+ 1 acc) acc))
                  0
                  fxmap))

(define (fxmapping-any? pred fxmap)
  (call-with-escape-continuation
   (lambda (return)
     (fxmapping-fold (lambda (k v _)
                       (and (pred k v) (return #t)))
                     #f
                     fxmap))))

(define (fxmapping-every? pred fxmap)
  (call-with-escape-continuation
   (lambda (return)
     (fxmapping-fold (lambda (k v _)
                       (or (pred k v) (return #f)))
                     #t
                     fxmap))))

;;;; Mapping and folding

;; Map proc over the assocs. of fxmap, inserting result values under
;; the same key.
;; This is *not* the same as SRFI 146's mapping-map.
(define (fxmapping-map proc fxmap)
  (raw-fxmapping (trie-map proc (fxmapping-trie fxmap))))

(define (unspecified)
  (void))

(define (fxmapping-for-each proc fxmap)
  (fxmapping-fold (lambda (k v _)
                    (proc k v)
                    (unspecified))
                  (unspecified)
                  fxmap))

(define (fxmapping-fold proc nil fxmap)
  (let ((trie (fxmapping-trie fxmap)))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (negative? m))
       (trie-fold-left proc (trie-fold-left proc nil r) l))
      ((branch ? ? ,l ,r)
       (trie-fold-left proc (trie-fold-left proc nil l) r))
      (else (trie-fold-left proc nil trie)))))

(define (fxmapping-fold-right proc nil fxmap)
  (let ((trie (fxmapping-trie fxmap)))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (negative? m))
       (trie-fold-right proc (trie-fold-right proc nil l) r))
      ((branch ? ? ,l ,r)
       (trie-fold-right proc (trie-fold-right proc nil r) l))
      (else (trie-fold-right proc nil trie)))))

(define (fxmapping-map->list proc fxmap)
  (fxmapping-fold-right (lambda (k v us)
                          (cons (proc k v) us))
                        '()
                        fxmap))

(define (fxmapping-filter pred fxmap)
  (raw-fxmapping (trie-filter pred (fxmapping-trie fxmap))))

(define (fxmapping-remove pred fxmap)
  (fxmapping-filter (lambda (k v) (not (pred k v))) fxmap))

(define (fxmapping-partition pred fxmap)
  (let-values (((tin tout)
                (trie-partition pred (fxmapping-trie fxmap))))
    (values (raw-fxmapping tin) (raw-fxmapping tout))))

;;;; Conversion

(define (fxmapping->alist fxmap)
  (fxmapping-fold-right (lambda (k v as) (cons (cons k v) as))
                        '()
                        fxmap))

(define (fxmapping->decreasing-alist fxmap)
  (fxmapping-fold (lambda (k v as) (cons (cons k v) as))
                  '()
                  fxmap))

(define (fxmapping-keys fxmap)
  (fxmapping-fold-right (lambda (k _ ks) (cons k ks)) '() fxmap))

(define (fxmapping-values fxmap)
  (fxmapping-fold-right (lambda (_ v vs) (cons v vs)) '() fxmap))

(define (fxmapping->generator fxmap)
  (make-coroutine-generator
   (lambda (yield)
     (fxmapping-fold (lambda (k v _) (yield (cons k v)))
                     #f
                     fxmap))))

(define (fxmapping->decreasing-generator fxmap)
  (make-coroutine-generator
   (lambda (yield)
     (fxmapping-fold-right (lambda (k v _) (yield (cons k v)))
                           #f
                           fxmap))))

;;;; Comparison

(define (fxmapping=? comp fxmap1 fxmap2 . imaps)
  (let ((fxmap-eq1 (lambda (fxmap)
                     (or (eqv? fxmap1 fxmap)
                         (trie=? comp
                                 (fxmapping-trie fxmap1)
                                 (fxmapping-trie fxmap))))))
    (and (fxmap-eq1 fxmap2)
         (or (null? imaps)
             (every fxmap-eq1 imaps)))))

(define (fxmapping<? comp fxmap1 fxmap2 . imaps)
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (trie-proper-subset? comp t1 t2)
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping>? comp fxmap1 fxmap2 . imaps)
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (trie-proper-subset? comp t2 t1)
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping<=? comp fxmap1 fxmap2 . imaps)
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (memv (trie-subset-compare comp t1 t2) '(less equal))
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping>=? comp fxmap1 fxmap2 . imaps)
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (memv (trie-subset-compare comp t2 t1) '(less equal))
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

;;;; Set theory operations

(define (fxmapping-union . args)
  (apply fxmapping-union/combinator first-arg args))

(define (fxmapping-intersection . args)
  (apply fxmapping-intersection/combinator first-arg args))

(define fxmapping-difference
  (case-lambda
    ((fxmap1 fxmap2)
     (raw-fxmapping
      (trie-difference (fxmapping-trie fxmap1)
                       (fxmapping-trie fxmap2))))
    ((fxmap . rest)
     (raw-fxmapping
      (trie-difference (fxmapping-trie fxmap)
                       (fxmapping-trie
                        (apply fxmapping-union rest)))))))

(define (fxmapping-xor fxmap1 fxmap2)
  (raw-fxmapping
   (trie-xor (fxmapping-trie fxmap1) (fxmapping-trie fxmap2))))

(define (fxmapping-union/combinator proc fxmap . rest)
  (raw-fxmapping
   (fold (lambda (im t)
           (trie-merge proc t (fxmapping-trie im)))
         (fxmapping-trie fxmap)
         rest)))

(define (fxmapping-intersection/combinator proc fxmap . rest)
  (raw-fxmapping
   (fold (lambda (im t)
           (trie-intersection proc t (fxmapping-trie im)))
         (fxmapping-trie fxmap)
         rest)))

;;;; Subsets

(define (fxsubmapping= fxmap key)
  (fxmapping-ref fxmap
                 key
                 fxmapping
                 (lambda (v) (fxmapping key v))))

(define (fxmapping-open-interval fxmap low high)
  (assume (fx>= high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #f #f)))

(define (fxmapping-closed-interval fxmap low high)
  (assume (fx>= high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #t #t)))

(define (fxmapping-open-closed-interval fxmap low high)
  (assume (fx>= high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #f #t)))

(define (fxmapping-closed-open-interval fxmap low high)
  (assume (fx>= high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #t #f)))

(define (fxsubmapping< fxmap key)
  (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #f)))

(define (fxsubmapping<= fxmap key)
  (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #t)))

(define (fxsubmapping> fxmap key)
  (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #f)))

(define (fxsubmapping>= fxmap key)
  (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #t)))

(define (fxmapping-split fxmap k)
  (let-values (((trie-low trie-high)
                (trie-split (fxmapping-trie fxmap) k)))
    (values (raw-fxmapping trie-low) (raw-fxmapping trie-high))))

;;;; fxmappings as relations

(define (fxmapping-relation-map proc fxmap)
  (raw-fxmapping (trie-relation-map proc (fxmapping-trie fxmap))))

(module+ test
  (define-syntax-rule (test-equal expected tst)
    (check-equal? tst expected))
  (define-syntax-rule (test-eqv expected tst)
    (check-eqv? tst expected))
  (define-syntax-rule (test-group name tests ...)
    (begin tests ...))


  ;;;; Utility

  (define default-comp (make-default-comparator))

  (define (constantly x) (lambda _ x))

  (define (first-arg _k x _y) x)
  (define (second-arg _k _x y) y)
  (define (nth n) (lambda args (list-ref args n)))

  (define (square x) (* x x))

  (define (list->dup-alist xs)
    (map cons xs xs))

  ;; From SRFI 210, reduced.
  (define-syntax value/mv
    (syntax-rules ()
      ((value/mv i producer)
       (let ([vs (values->list producer)])
         (list-ref vs i)))))

  (define (generator->list g)
    (let rec ((x (g)))
      (if (eof-object? x)
          '()
          (cons x (rec (g))))))

  ;;;; Test fxmappings

  (define empty-fxmap (fxmapping))

  (define letter-fxmap
    (let* ((cs "abcdefghijklmnopqrstuvwxyz")
           (len (string-length cs)))
      (fxmapping-unfold
       (lambda (i) (= i len))
       (lambda (i) (values i (string->symbol (string (string-ref cs i)))))
       (lambda (i) (+ i 1))
       0)))

  ;; (-100 . -100), (-75 . -75), ..., (0 . 0), ..., (100 . 100)
  (define mixed-seq
    (unfold (lambda (i) (> i 100))
            (lambda (i) (cons i i))
            (lambda (i) (+ i 25))
            -100))

  (define mixed-fxmap (alist->fxmapping mixed-seq))

  ;; From -65536 to 65536 in steps of 4096.  Key = value.
  (define sparse-seq
    (unfold (lambda (i) (> i 65536))
            (lambda (i) (cons i i))
            (lambda (i) (+ i 4096))
            -65536))

  (define sparse-fxmap (alist->fxmapping sparse-seq))

  (define all-test-fxmaps
    (list empty-fxmap mixed-fxmap letter-fxmap sparse-fxmap))

  ;;; fxmapping=? and the alist conversions are used in many other tests,
  ;;; so we test these first.  These also test the basic fxmapping
  ;;; constructor.

  (test-group "Equality"
              (test-equal? "Custom equal? empty maps" empty-fxmap (fxmapping))
              (test-equal? "Custom equal? populated maps" (fxmapping 10 'a) (fxmapping 10 'a))
              (test-false "Custom equal? fail" (equal? (fxmapping 10 'a) (fxmapping 10 'b)))
              (test-eqv #t (fxmapping=? default-comp empty-fxmap (fxmapping)))
              (test-eqv #t (fxmapping=? default-comp (fxmapping 10 'a) (fxmapping 10 'a)))
              (test-eqv #f (fxmapping=? default-comp empty-fxmap (fxmapping 10 'a)))
              (test-eqv #t (fxmapping=? default-comp mixed-fxmap mixed-fxmap))
              (test-eqv #t (fxmapping=? default-comp letter-fxmap letter-fxmap))
              )

  (test-group "Conversion"
              (test-eqv #t (null? (fxmapping->alist empty-fxmap)))
              (test-equal '((10 . a)) (fxmapping->alist (fxmapping 10 'a)))
              (test-equal mixed-seq (fxmapping->alist mixed-fxmap))
              (test-equal sparse-seq (fxmapping->alist sparse-fxmap))

              (test-eqv #t (null? (fxmapping->decreasing-alist empty-fxmap)))
              (test-equal '((10 . a)) (fxmapping->decreasing-alist (fxmapping 10 'a)))
              (test-equal (reverse mixed-seq) (fxmapping->decreasing-alist mixed-fxmap))
              (test-equal (reverse sparse-seq) (fxmapping->decreasing-alist sparse-fxmap))

              (test-eqv #t (null? (fxmapping-keys empty-fxmap)))
              (test-equal (map car mixed-seq) (fxmapping-keys mixed-fxmap))
              (test-equal (map car sparse-seq) (fxmapping-keys sparse-fxmap))

              (test-eqv #t (null? (fxmapping-values empty-fxmap)))
              (test-equal (map cdr mixed-seq) (fxmapping-values mixed-fxmap))
              (test-equal (map cdr sparse-seq) (fxmapping-values sparse-fxmap))

              (test-eqv #t
                        (every
                         (lambda (im)
                           (equal? (fxmapping->alist im)
                                   (generator->list (fxmapping->generator im))))
                         all-test-fxmaps))
              (test-eqv #t
                        (every
                         (lambda (im)
                           (equal? (fxmapping->decreasing-alist im)
                                   (generator->list (fxmapping->decreasing-generator im))))
                         all-test-fxmaps))
              )

  (test-group "Constructors"
              (test-equal '((1 . a) (2 . b) (3 . c))
                          (fxmapping->alist (fxmapping 1 'a 2 'b 3 'c)))

              ;;; unfolds

              (test-eqv #t (null? (fxmapping->alist (fxmapping-unfold
                                                     values
                                                     (lambda (b) (values 1 b))
                                                     (lambda (b) (not b))
                                                     #t))))
              (test-equal '((1 . #f)) (fxmapping->alist (fxmapping-unfold
                                                         values
                                                         (lambda (b) (values 1 b))
                                                         (lambda (b) (not b))
                                                         #f)))
              (test-equal '((-3 . -3) (-2 . -2) (-1 . -1))
                          (fxmapping->alist (fxmapping-unfold
                                             (lambda (i) (< i -3))
                                             (lambda (i) (values i i))
                                             (lambda (i) (- i 1))
                                             -1)))
              ;; Multiple seeds.
              (test-equal '((1 . 2) (2 . 8) (3 . 18) (4 . 32))
                          (fxmapping->alist
                           (fxmapping-unfold
                            (lambda (a _) (> a 4))
                            (lambda (a b) (values a (* a b)))
                            (lambda (a b) (values (+ a 1) (+ b 2)))
                            1
                            2)))

              (test-eqv #t (null? (fxmapping->alist (fxmapping-accumulate
                                                     (lambda (abort b)
                                                       (if b (abort) (values 1 b (not b))))
                                                     #t))))
              (test-equal '((1 . #f))
                          (fxmapping->alist (fxmapping-accumulate
                                             (lambda (abort b)
                                               (if b (abort) (values 1 b (not b))))
                                             #f)))
              (test-equal '((-3 . -3) (-2 . -2) (-1 . -1))
                          (fxmapping->alist
                           (fxmapping-accumulate
                            (lambda (abort i)
                              (if (< i -3) (abort) (values i i (- i 1))))
                            -1)))
              ;; Multiple seeds.
              (test-equal '((1 . 2) (2 . 8) (3 . 18) (4 . 32))
                          (fxmapping->alist
                           (fxmapping-accumulate
                            (lambda (abort a b)
                              (if (> a 4)
                                  (abort)
                                  (values a (* a b) (+ a 1) (+ b 2))))
                            1
                            2)))
              ;; Return additional values via the abort continuation.
              (test-equal '(((-3 . -3) (-2 . -2) (-1 . -1)) x y)
                          (let-values
                              (((fxm u v)
                                (fxmapping-accumulate
                                 (lambda (abort i)
                                   (if (< i -3) (abort 'x 'y) (values i i (- i 1))))
                                 -1)))
                            (list (fxmapping->alist fxm) u v)))

              ;;; alist->fxmapping

              (test-eqv #t (null? (fxmapping->alist (alist->fxmapping '()))))
              (test-equal mixed-seq (fxmapping->alist (alist->fxmapping mixed-seq)))
              (test-equal sparse-seq (fxmapping->alist (alist->fxmapping sparse-seq)))

              (test-equal '((0 . a) (1 . b) (2 . c))
                          (fxmapping->alist
                           (alist->fxmapping '((0 . a) (1 . b) (2 . c) (2 . #t)))))

              (test-equal '((0 . a) (1 . b) (2 . #t))
                          (fxmapping->alist
                           (alist->fxmapping/combinator
                            first-arg
                            '((0 . a) (1 . b) (2 . c) (2 . #t)))))
              )

  (test-group "Predicates"
              (test-eqv #f (fxmapping-contains? empty-fxmap 1))
              (test-eqv #t (fxmapping-contains? letter-fxmap 0))
              (test-eqv #f (fxmapping-contains? letter-fxmap 100))
              (test-eqv #t (fxmapping-contains? sparse-fxmap 4096))
              (test-eqv #f (fxmapping-contains? sparse-fxmap -4097))

              (test-eqv #t (fxmapping-empty? empty-fxmap))
              (test-eqv #f (fxmapping-empty? letter-fxmap))
              (test-eqv #f (fxmapping-empty? mixed-fxmap))
              (test-eqv #f (fxmapping-empty? sparse-fxmap))

              (test-eqv #t (fxmapping-disjoint? empty-fxmap letter-fxmap))
              (test-eqv #t (fxmapping-disjoint? (fxmapping 1 'a) (fxmapping 2 'b)))
              (test-eqv #f (fxmapping-disjoint? (fxmapping 1 'a) (fxmapping 1 'b)))
              )


  (test-group "Accessors"
              (test-eqv -50 (fxmapping-ref mixed-fxmap -50))
              (test-eqv 36864 (fxmapping-ref sparse-fxmap 36864))
              (test-eqv 'z (fxmapping-ref sparse-fxmap 17 (lambda () 'z)))
              (test-eqv 625 (fxmapping-ref mixed-fxmap 25 (lambda () #f) square))

              (test-eqv 'z (fxmapping-ref/default empty-fxmap 1 'z))
              (test-eqv 'a (fxmapping-ref/default letter-fxmap 0 #f))
              (test-eqv -50 (fxmapping-ref/default mixed-fxmap -50 #f))
              (test-eqv 'z (fxmapping-ref/default mixed-fxmap -51 'z))
              (test-eqv 36864 (fxmapping-ref/default sparse-fxmap 36864 #f))
              (test-eqv 'z (fxmapping-ref/default sparse-fxmap 36800 'z))

              ;;; min/max

              (test-equal '(0 a)
                          (values->list (fxmapping-min letter-fxmap)))
              (test-equal '(-100 -100)
                          (values->list (fxmapping-min mixed-fxmap)))

              (test-equal '(25 z)
                          (values->list (fxmapping-max letter-fxmap)))
              (test-equal '(100 100)
                          (values->list(fxmapping-max mixed-fxmap)))
              )

  (test-group "Updaters"
              ;;; adjoins

              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a)
                                        (fxmapping-adjoin empty-fxmap 0 'a)))
              (test-eqv #t (fxmapping-contains? (fxmapping-adjoin mixed-fxmap 200 #t) 200))
              (test-eqv #t (fxmapping-contains? (fxmapping-adjoin sparse-fxmap -200 #t)
                                                -200))
              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a 1 'b 2 'c)
                                        (fxmapping-adjoin empty-fxmap 0 'a 1 'b 2 'c)))
              (test-eqv (fxmapping-ref sparse-fxmap -4096)
                        (fxmapping-ref (fxmapping-adjoin sparse-fxmap -4096 'z) -4096))

              (test-eqv 'U (fxmapping-ref/default
                            (fxmapping-adjoin/combinator letter-fxmap first-arg 20 'U)
                            20
                            #f))
              (test-eqv 'u (fxmapping-ref/default
                            (fxmapping-adjoin/combinator letter-fxmap second-arg 20 'U)
                            20
                            #f))
              (test-eqv #t
                        (fxmapping=?
                         default-comp
                         (fxmapping 0 'a 1 'b 2 'c)
                         (fxmapping-adjoin/combinator empty-fxmap
                                                      first-arg
                                                      0 'a 1 'b 2 'c)))

              ;;; set

              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a)
                                        (fxmapping-set empty-fxmap 0 'a)))
              (test-eqv #t (fxmapping-contains? (fxmapping-set mixed-fxmap 200 #t) 200))
              (test-eqv #t (fxmapping-contains? (fxmapping-set sparse-fxmap -200 #t) -200))
              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a 1 'b 2 'c)
                                        (fxmapping-set empty-fxmap 0 'a 1 'b 2 'c)))
              (test-eqv 'z
                        (fxmapping-ref (fxmapping-set sparse-fxmap -4096 'z) -4096))

              ;;; adjusts

              (test-equal '(20 u) (fxmapping-ref/default
                                   (fxmapping-adjust letter-fxmap 20 list)
                                   20
                                   #f))
              (test-eqv 16384 (fxmapping-ref/default
                               (fxmapping-adjust sparse-fxmap
                                                 8192
                                                 (lambda (k v) (+ k v)))
                               8192
                               #f))
              (test-eqv #t (fxmapping-empty? (fxmapping-adjust empty-fxmap 1 list)))

              ;;; delete & delete-all

              (test-eqv #f (fxmapping-contains? (fxmapping-delete letter-fxmap 10) 10))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete mixed-fxmap 50) 50))
              (test-eqv #t (fxmapping=? default-comp
                                        mixed-fxmap
                                        (fxmapping-delete mixed-fxmap 1)))
              (let* ((ks '(4096 8192 16384))
                     (sm (apply fxmapping-delete sparse-fxmap ks)))
                (test-eqv #f (ormap (lambda (k) (fxmapping-contains? sm k)) ks)))

              (test-eqv #f (fxmapping-contains? (fxmapping-delete-all letter-fxmap '(10))
                                                10))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete-all mixed-fxmap '(50))
                                                50))
              (test-eqv #t (fxmapping=? default-comp
                                        mixed-fxmap
                                        (fxmapping-delete-all mixed-fxmap '(1))))
              (let* ((ks '(4096 8192 16384))
                     (sm (fxmapping-delete-all sparse-fxmap ks)))
                (test-eqv #f (ormap (lambda (k) (fxmapping-contains? sm k)) ks)))

              ;;; update

              (test-eqv #t (fxmapping=?
                            default-comp
                            (fxmapping 0 '(0 a))
                            (fxmapping-update (fxmapping 0 'a)
                                              0
                                              (lambda (k v replace _del)
                                                (replace (list k v))))))
              (test-eqv 'U (fxmapping-ref/default
                            (fxmapping-update letter-fxmap
                                              20
                                              (lambda (_k _v replace _del)
                                                (replace 'U)))
                            20
                            #f))
              (test-eqv #f (fxmapping-contains?
                            (fxmapping-update letter-fxmap
                                              20
                                              (lambda (_k _v _rep delete)
                                                (delete)))
                            20))
              (test-eqv #f (fxmapping-contains?
                            (fxmapping-update sparse-fxmap
                                              -8192
                                              (lambda (_k _v _rep delete)
                                                (delete)))
                            -8192))
              ;; Passing out an additional value.
              (test-equal '(() #t)
                        (fxmapping-update (fxmapping 0 'a)
                                          0
                                          (lambda (_k _v _r delete)
                                            (list (fxmapping->alist (delete))
                                                  #t))))
              (test-equal '(((0 . z)) #t)
                        (fxmapping-update (fxmapping 0 'a)
                                          0
                                          (lambda (_k _v replace _d)
                                            (list (fxmapping->alist (replace 'z))
                                                  #t))))
              ;; Return the original fxmapping on failure.
              (test-eqv #t
                        (fxmapping=? default-comp
                                     mixed-fxmap
                                     (fxmapping-update mixed-fxmap
                                                       5
                                                       (lambda (_k _v replace _d)
                                                         (replace 'z))
                                                       (lambda () mixed-fxmap))))

              ;;; alter

              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a)
                                        (fxmapping-alter (fxmapping 0 'a)
                                                         1
                                                         (lambda (_ins ignore) (ignore))
                                                         (lambda (_k v replace _del)
                                                           (replace v)))))
              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 'a 1 'b)
                                        (fxmapping-alter (fxmapping 0 'a)
                                                         1
                                                         (lambda (insert _ig) (insert 'b))
                                                         (lambda (_k v replace _del)
                                                           (replace 'b)))))
              (test-eqv 101 (fxmapping-ref/default
                             (fxmapping-alter mixed-fxmap
                                              101
                                              (lambda (insert _ig) (insert 101))
                                              (lambda (_k _v replace _del)
                                                (replace 101)))
                             101
                             #f))
              (test-eqv 101 (fxmapping-ref/default
                             (fxmapping-alter mixed-fxmap
                                              100
                                              (lambda (insert _ig) (insert 101))
                                              (lambda (_k v replace _del)
                                                (replace (+ v 1))))
                             100
                             #f))
              (test-eqv 'z (fxmapping-ref/default
                            (fxmapping-alter mixed-fxmap
                                             100
                                             (lambda (_ins ignore) (ignore))
                                             (lambda (_k _v _rep delete) (delete)))
                            100
                            'z))
              (test-eqv -16383 (fxmapping-ref/default
                                (fxmapping-alter sparse-fxmap
                                                 -16384
                                                 (lambda (insert _ig) (insert -16383))
                                                 (lambda (_k v replace _del)
                                                   (replace (+ v 1))))
                                -16384
                                #f))
              (test-eqv 'z (fxmapping-ref/default
                            (fxmapping-alter sparse-fxmap
                                             -16384
                                             (lambda (_ins ignore) (ignore))
                                             (lambda (_k _v _rep delete) (delete)))
                            -16384
                            'z))
              ;; Passing out an additional value.
              (test-equal '(() #t)
                        (fxmapping-alter (fxmapping 0 'a)
                                         0
                                         (lambda (_in ignore) (list #f #f))
                                         (lambda (_k _v _r delete)
                                           (list (fxmapping->alist (delete))
                                                 #t))))
              (test-equal '(((0 . a) (1 . b)) #t)
                        (fxmapping-alter (fxmapping 0 'a)
                                         1
                                         (lambda (insert _ig)
                                           (list (fxmapping->alist (insert 'b))
                                                 #t))
                                         (lambda (_k _v _r delete)
                                           (list #f #f))))

              ;;; delete-min/-max

              (test-eqv #t (fxmapping=? default-comp
                                        empty-fxmap
                                        (fxmapping-delete-min (fxmapping 0 'a))))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete-min letter-fxmap) 0))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete-min sparse-fxmap) -65536))

              (test-eqv #t (fxmapping=? default-comp
                                        empty-fxmap
                                        (fxmapping-delete-max (fxmapping 0 'a))))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete-max letter-fxmap) 25))
              (test-eqv #f (fxmapping-contains? (fxmapping-delete-max sparse-fxmap) 65536))

              ;;; min updaters

              (test-eqv -200 (fxmapping-ref/default
                              (fxmapping-update-min mixed-fxmap
                                                    (lambda (k v replace _del)
                                                      (replace (+ k v))))
                              -100
                              #f))
              (test-equal '(0 a)
                          (fxmapping-ref/default
                           (fxmapping-update-min letter-fxmap
                                                 (lambda (k v replace _del)
                                                   (replace (list k v))))
                           0
                           #f))
              ;; Passing out an additional value.
              (test-equal '(((0 . a)) #t)
                        (fxmapping-update-min (fxmapping -3 'q 0 'a)
                                              (lambda (k v _rep delete)
                                                (list (fxmapping->alist (delete))
                                                      #t))))
              (test-equal '(((-3 . z) (0 . a)) #t)
                        (fxmapping-update-min (fxmapping -3 'q 0 'a)
                                              (lambda (k v replace _del)
                                                (list (fxmapping->alist (replace 'z))
                                                      #t))))

              ;;; max updaters

              (test-eqv 200 (fxmapping-ref/default
                             (fxmapping-update-max mixed-fxmap
                                                   (lambda (k v replace _del)
                                                     (replace (+ k v))))
                             100
                             #f))
              (test-equal '(25 z)
                          (fxmapping-ref/default
                           (fxmapping-update-max letter-fxmap
                                                 (lambda (k v replace _del)
                                                   (replace (list k v))))
                           25
                           #f))
              ;; Passing out an additional value.
              (test-equal '(((0 . a)) #t)
                        (fxmapping-update-max (fxmapping 3 'd 0 'a)
                                              (lambda (k v _rep delete)
                                                (list (fxmapping->alist (delete))
                                                      #t))))
              (test-equal '(((0 . a) (3 . z)) #t)
                        (fxmapping-update-max (fxmapping 3 'd 0 'a)
                                              (lambda (k v replace _del)
                                                (list (fxmapping->alist (replace 'z))
                                                      #t))))

              ;;; pop-min

              (test-eqv #t
                        (every
                         (lambda (im)
                           (let-values (((k v im*) (fxmapping-pop-min im))
                                        ((test-k test-v) (fxmapping-min im)))
                             (and (= k test-k)
                                  (eqv? v test-v)
                                  (fxmapping=? default-comp
                                               (fxmapping-delete-min im)
                                               im*))))
                         (list mixed-fxmap letter-fxmap sparse-fxmap)))  ; non-empty only

              ;;; pop-max

              (test-eqv #t
                        (every
                         (lambda (im)
                           (let-values (((k v im*) (fxmapping-pop-max im))
                                        ((test-k test-v) (fxmapping-max im)))
                             (and (= k test-k)
                                  (eqv? v test-v)
                                  (fxmapping=? default-comp
                                               (fxmapping-delete-max im)
                                               im*))))
                         (list mixed-fxmap letter-fxmap sparse-fxmap)))  ; non-empty only
              )

  (test-group "Whole fxmappings"
              (test-eqv 0 (fxmapping-size empty-fxmap))
              (test-eqv 26 (fxmapping-size letter-fxmap))

              ;;; find

              (test-eqv 'z (fxmapping-find even? empty-fxmap (constantly 'z)))
              (test-equal '(0 a)
                          (fxmapping-find (lambda (_ v) (symbol? v))
                                          letter-fxmap
                                          (lambda () '(#f #f))
                                          list))
              (let ((ss '(f r o b)))
                (test-equal '(1 b)
                            (fxmapping-find (lambda (_ s) (memv s ss))
                                            letter-fxmap
                                            (lambda () '(#f #f))
                                            list)))
              (test-equal '(4096 4096)
                          (fxmapping-find (lambda (_ v) (positive? v))
                                          sparse-fxmap
                                          (lambda () '(#f #f))
                                          list))
              ;; Ensure negative-keyed associations are tested first.
              (test-equal '(-65536 -65536)
                          (fxmapping-find (lambda (_ v) (integer? v))
                                          sparse-fxmap
                                          (lambda () '(#f #f))
                                          list))
              (test-equal '(z z)
                          (fxmapping-find eqv?
                                          letter-fxmap
                                          (lambda () '(z z))
                                          list))

              ;;; count

              (test-eqv 0 (fxmapping-count (lambda (_ v) (even? v)) empty-fxmap))
              (test-eqv 26 (fxmapping-count (lambda (_ v) (symbol? v)) letter-fxmap))
              (let ((ss '(f r o b)))
                (test-eqv (length ss)
                          (fxmapping-count (lambda (_ s) (memv s ss)) letter-fxmap))
                (test-eqv (- (fxmapping-size letter-fxmap) (length ss))
                          (fxmapping-count (lambda (_ s) (not (memv s ss))) letter-fxmap)))
              (test-eqv 4 (fxmapping-count (lambda (_ v) (positive? v)) mixed-fxmap))
              (test-eqv 2 (fxmapping-count (lambda (k v) (and (even? k) (positive? v)))
                                           mixed-fxmap))

              ;;; any?/every?

              (test-eqv #f (fxmapping-any? (lambda (_ v) (even? v)) empty-fxmap))
              (test-eqv #t (fxmapping-any? (lambda (_ v) (positive? v)) mixed-fxmap))
              (test-eqv #f (fxmapping-any? (lambda (_ v) (odd? v)) sparse-fxmap))
              (test-eqv #t (fxmapping-any? (lambda (_ v) (negative? v)) sparse-fxmap))

              (test-eqv #t (fxmapping-every? (lambda (_ v) (even? v)) empty-fxmap))
              (test-eqv #f (fxmapping-every? (lambda (_ v) (positive? v)) mixed-fxmap))
              (test-eqv #t (fxmapping-every? (lambda (_ v) (even? v)) sparse-fxmap))
              (test-eqv #f (fxmapping-every? (lambda (_ v) (negative? v)) sparse-fxmap))
              )

  (test-group "Iterators"
              ;;; map

              (test-eqv #t (fxmapping=? default-comp
                                        empty-fxmap
                                        (fxmapping-map (constantly #t) empty-fxmap)))
              (test-eqv #t (fxmapping=? default-comp
                                        mixed-fxmap
                                        (fxmapping-map (nth 1) mixed-fxmap)))
              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 0 "" 1 "b" 2 "cc")
                                        (fxmapping-map make-string
                                                       (fxmapping 0 #\a 1 #\b 2 #\c))))

              ;;; for-each

              (test-eqv 26
                        (let ((size 0))
                          (fxmapping-for-each (lambda (_k _v) (set! size (+ size 1)))
                                              letter-fxmap)
                          size))
              (test-equal '(c b a)
                          (let ((xs '()))
                            (fxmapping-for-each (lambda (_ x) (set! xs (cons x xs)))
                                                (fxmapping 0 'a 1 'b 2 'c))
                            xs))
              (test-equal '((2 . c) (1 . b) (0 . a))
                          (let ((xs '()))
                            (fxmapping-for-each
                             (lambda (k x) (set! xs (cons (cons k x) xs)))
                             (fxmapping 0 'a 1 'b 2 'c))
                            xs))

              ;;; fold

              (test-eqv 'z (fxmapping-fold (nth 2) 'z empty-fxmap))
              (test-equal (reverse '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
                          (fxmapping-fold (lambda (_ v vs) (cons v vs))
                                          '()
                                          letter-fxmap))
              (test-equal (reverse (iota 9 -100 25))
                          (fxmapping-fold (lambda (_ v vs) (cons v vs))
                                          '()
                                          mixed-fxmap))
              (test-eqv (fold + 0 (iota 9 -100 25))
                        (fxmapping-fold (lambda (_ v sum) (+ v sum))
                                        0
                                        mixed-fxmap))
              (test-equal (reverse '((0 . "") (1 . "b") (2 . "cc")))
                          (fxmapping-fold (lambda (k c as)
                                            (cons (cons k (make-string k c)) as))
                                          '()
                                          (fxmapping 0 #\a 1 #\b 2 #\c)))

              ;;; fold-right

              (test-eqv 'z (fxmapping-fold-right (nth 2) 'z empty-fxmap))
              (test-equal '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                          (fxmapping-fold-right (lambda (_ v vs) (cons v vs))
                                                '()
                                                letter-fxmap))
              (test-equal (iota 9 -100 25)
                          (fxmapping-fold-right (lambda (_ v vs) (cons v vs))
                                                '()
                                                mixed-fxmap))
              (test-eqv (fold + 0 (iota 9 -100 25))
                        (fxmapping-fold-right (lambda (_ v sum) (+ v sum))
                                              0
                                              mixed-fxmap))
              (test-equal '((0 . "") (1 . "b") (2 . "cc"))
                          (fxmapping-fold-right (lambda (k c as)
                                                  (cons (cons k (make-string k c)) as))
                                                '()
                                                (fxmapping 0 #\a 1 #\b 2 #\c)))

              ;;; map->list

              (test-eqv #t (null? (fxmapping-map->list (constantly #t) empty-fxmap)))
              (test-equal '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                          (fxmapping-map->list (nth 1) letter-fxmap))
              (test-equal (map square (iota 9 -100 25))
                          (fxmapping-map->list (lambda (_ v) (square v)) mixed-fxmap))
              (test-equal '("" "a" "aa")
                          (fxmapping-map->list (lambda (_ n) (make-string n #\a))
                                               (fxmapping 0 0 1 1 2 2)))
              (test-equal (iota 26) (fxmapping-map->list (nth 0) letter-fxmap))
              (test-equal '((0 . "") (1 . "b") (2 . "cc"))
                          (fxmapping-map->list (lambda (k c) (cons k (make-string k c)))
                                               (fxmapping 0 #\a 1 #\b 2 #\c)))
              )

  (test-group "Filters"
              (test-eqv #t
                        (every values
                               (map (lambda (m)
                                      (fxmapping=? default-comp
                                                   m
                                                   (fxmapping-filter (constantly #t) m)))
                                    all-test-fxmaps)))
              (test-eqv #t
                        (every fxmapping-empty?
                               (map (lambda (m) (fxmapping-filter (constantly #f) m))
                                    all-test-fxmaps)))
              (test-eqv #t (fxmapping=? default-comp
                                        (fxmapping 25 25 75 75)
                                        (fxmapping-filter (lambda (k v)
                                                            (and (odd? k) (positive? v)))
                                                          mixed-fxmap)))

              ;;; remove

              (test-eqv #t
                        (every (lambda (m)
                                 (fxmapping=? default-comp
                                              m
                                              (fxmapping-remove (constantly #f) m)))
                               all-test-fxmaps))
              (test-eqv #t
                        (every fxmapping-empty?
                               (map (lambda (m) (fxmapping-remove (constantly #t) m))
                                    all-test-fxmaps)))
              (test-eqv #t
                        (fxmapping=? default-comp
                                     (fxmapping -100 -100 -50 -50 0 0)
                                     (fxmapping-remove (lambda (k v)
                                                         (or (odd? k) (positive? v)))
                                                       mixed-fxmap)))

              ;;; partition

              (test-eqv #t
                        (every (lambda (m)
                                 (fxmapping=?
                                  default-comp
                                  m
                                  (value/mv 0 (fxmapping-partition (constantly #t) m))))
                               all-test-fxmaps))
              (test-equal (values->list (partition even? (map car mixed-seq)))
                          (let-values (((em om)
                                        (fxmapping-partition (lambda (_ v) (even? v))
                                                             mixed-fxmap)))
                            (list (fxmapping-values em) (fxmapping-values om))))
              (test-eqv #t
                        (let-values (((zm not-zm)
                                      (fxmapping-partition (lambda (_ s) (eqv? s 'z))
                                                           letter-fxmap)))
                          (and (fxmapping=? default-comp zm (fxmapping 25 'z))
                               (fxmapping=? default-comp
                                            not-zm
                                            (fxmapping-delete letter-fxmap 25)))))
              (test-equal (unfold (lambda (i) (= i 26))
                                  (lambda (i)
                                    (string->symbol (string (integer->char (+ i #x61)))))
                                  (lambda (i) (+ i 2))
                                  0)
                          (let-values (((em _)
                                        (fxmapping-partition (lambda (k _) (even? k))
                                                             letter-fxmap)))
                            (fxmapping-values em)))
              )

  (test-group "Comparison"
              (let ((subfxmap (fxmapping-filter (lambda (_ v) (even? v)) mixed-fxmap)))
                (test-eqv #t (fxmapping<? default-comp (fxmapping) mixed-fxmap))
                (test-eqv #t (fxmapping<? default-comp subfxmap mixed-fxmap))
                (test-eqv #f (fxmapping<? default-comp mixed-fxmap subfxmap))
                (test-eqv #f (fxmapping<? default-comp mixed-fxmap mixed-fxmap))
                (test-eqv #f (fxmapping<? default-comp
                                          (fxmapping 0 'z)
                                          (fxmapping 0 'a 1 'b)))

                (test-eqv #t (fxmapping>? default-comp mixed-fxmap (fxmapping)))
                (test-eqv #f (fxmapping>? default-comp subfxmap mixed-fxmap))
                (test-eqv #t (fxmapping>? default-comp mixed-fxmap subfxmap))
                (test-eqv #f (fxmapping>? default-comp mixed-fxmap mixed-fxmap))
                (test-eqv #f (fxmapping>? default-comp
                                          (fxmapping 0 'z 1 'b)
                                          (fxmapping 0 'a)))

                (test-eqv #t (fxmapping<=? default-comp (fxmapping) mixed-fxmap))
                (test-eqv #t (fxmapping<=? default-comp subfxmap mixed-fxmap))
                (test-eqv #f (fxmapping<=? default-comp mixed-fxmap subfxmap))
                (test-eqv #t (fxmapping<=? default-comp mixed-fxmap mixed-fxmap))
                (test-eqv #f (fxmapping<=? default-comp
                                           (fxmapping 0 'z 1 'b)
                                           (fxmapping 0 'a 1 'b)))

                (test-eqv #t (fxmapping>=? default-comp mixed-fxmap (fxmapping)))
                (test-eqv #f (fxmapping>=? default-comp subfxmap mixed-fxmap))
                (test-eqv #t (fxmapping>=? default-comp mixed-fxmap subfxmap))
                (test-eqv #t (fxmapping>=? default-comp mixed-fxmap mixed-fxmap))
                (test-eqv #f (fxmapping<=? default-comp
                                           (fxmapping 0 'z 1 'b)
                                           (fxmapping 0 'a 1 'b)))

                ;; Variadic comparisons.
                (let ((subfxmap1 (fxmapping-filter (lambda (_ v) (positive? v)) subfxmap)))
                  (test-eqv #t (fxmapping<? default-comp subfxmap1 subfxmap mixed-fxmap))
                  (test-eqv #f (fxmapping<? default-comp subfxmap1 empty-fxmap mixed-fxmap))

                  (test-eqv #t (fxmapping>? default-comp mixed-fxmap subfxmap subfxmap1))
                  (test-eqv #f (fxmapping>? default-comp mixed-fxmap empty-fxmap subfxmap1))

                  (test-eqv #t (fxmapping<=? default-comp
                                             subfxmap1
                                             subfxmap
                                             subfxmap
                                             mixed-fxmap))
                  (test-eqv #f
                            (fxmapping<=? default-comp subfxmap1 empty-fxmap mixed-fxmap))

                  (test-eqv #t (fxmapping>=? default-comp
                                             mixed-fxmap
                                             subfxmap
                                             subfxmap
                                             subfxmap1))
                  (test-eqv #f
                            (fxmapping>=? default-comp mixed-fxmap empty-fxmap subfxmap1))
                  ))
              )

  (test-group "Set theory"
              (let ((fxmap1 (fxmapping -5 'a -2 'b 1 'c))
                    (fxmap2 (fxmapping -2 'b 3 'd 5 'e))
                    (fxmap3 (fxmapping 3 'd 5 'g 7 'f)))  ; assoc for 5 differs from fxmap2
                (test-eqv #t (fxmapping=? default-comp
                                          sparse-fxmap
                                          (fxmapping-union sparse-fxmap empty-fxmap)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -5 'a -2 'b 1 'c 3 'd 5 'e)
                                          (fxmapping-union fxmap1 fxmap2)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -2 'b 3 'd 5 'e 7 'f)
                                          (fxmapping-union fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -5 'a -2 'b 1 'c 3 'd 5 'e 7 'f)
                                          (fxmapping-union fxmap1 fxmap2 fxmap3)))

                (test-eqv #t (fxmapping-empty?
                              (fxmapping-intersection sparse-fxmap empty-fxmap)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -2 'b)
                                          (fxmapping-intersection fxmap1 fxmap2)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping 3 'd 5 'e)
                                          (fxmapping-intersection fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping -2 'b)
                              (fxmapping-intersection fxmap1 fxmap2 (fxmapping -2 'b))))

                (test-eqv #t (fxmapping=? default-comp
                                          sparse-fxmap
                                          (fxmapping-difference sparse-fxmap empty-fxmap)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -5 'a 1 'c)
                                          (fxmapping-difference fxmap1 fxmap2)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -2 'b)
                                          (fxmapping-difference fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -5 'a 1 'c)
                                          (fxmapping-difference fxmap1 fxmap2 fxmap3)))

                (test-eqv #t (fxmapping=? default-comp
                                          sparse-fxmap
                                          (fxmapping-xor sparse-fxmap empty-fxmap)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -5 'a 1 'c 3 'd 5 'e)
                                          (fxmapping-xor fxmap1 fxmap2)))
                (test-eqv #t (fxmapping=? default-comp
                                          (fxmapping -2 'b 7 'f)
                                          (fxmapping-xor fxmap2 fxmap3)))

                ;;; /combinator variants

                (test-eqv #t (fxmapping=? default-comp
                                          sparse-fxmap
                                          (fxmapping-union/combinator second-arg
                                                                      sparse-fxmap
                                                                      empty-fxmap)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping -2 'b 3 'd 5 'g 7 'f)
                              (fxmapping-union/combinator second-arg fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping -5 'a -2 'b 1 'c 3 'd 5 'g 7 'f)
                              (fxmapping-union/combinator second-arg fxmap1 fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping 0 "abc")
                              (fxmapping-union/combinator (lambda (_ s t)
                                                            (string-append s t))
                                                          (fxmapping 0 "a")
                                                          (fxmapping 0 "b")
                                                          (fxmapping 0 "c"))))

                (test-eqv #t (fxmapping=? default-comp
                                          empty-fxmap
                                          (fxmapping-intersection/combinator second-arg
                                                                             sparse-fxmap
                                                                             empty-fxmap)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping 3 'd 5 'g)
                              (fxmapping-intersection/combinator second-arg fxmap2 fxmap3)))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping -2 'z)
                              (fxmapping-intersection/combinator second-arg
                                                                 fxmap1
                                                                 fxmap2
                                                                 (fxmapping -2 'z))))
                (test-eqv #t (fxmapping=?
                              default-comp
                              (fxmapping 0 "abc")
                              (fxmapping-intersection/combinator (lambda (_ s t)
                                                                   (string-append s t))
                                                                 (fxmapping 0 "a")
                                                                 (fxmapping 0 "b")
                                                                 (fxmapping 0 "c"))))
                ))

  (test-group "Intervals"
              (test-equal (list->dup-alist '(-25 0 25))
                          (fxmapping->alist
                           (fxmapping-open-interval mixed-fxmap -50 50)))
              (test-equal '((6 . g) (7 . h) (8 . i) (9 . j))
                          (fxmapping->alist
                           (fxmapping-open-interval letter-fxmap 5 10)))
              (test-equal (list->dup-alist '(-8192 -4096 0 4096 8192))
                          (fxmapping->alist
                           (fxmapping-open-interval sparse-fxmap -12288 12288)))

              (test-equal (list->dup-alist '(-50 -25 0 25 50))
                          (fxmapping->alist
                           (fxmapping-closed-interval mixed-fxmap -50 50)))
              (test-equal '((5 . f) (6 . g) (7 . h) (8 . i) (9 . j) (10 . k))
                          (fxmapping->alist
                           (fxmapping-closed-interval letter-fxmap 5 10)))
              (test-equal (list->dup-alist '(-12288 -8192 -4096 0 4096 8192 12288))
                          (fxmapping->alist
                           (fxmapping-closed-interval sparse-fxmap -12288 12288)))

              (test-equal (list->dup-alist '(-25 0 25 50))
                          (fxmapping->alist
                           (fxmapping-open-closed-interval mixed-fxmap -50 50)))
              (test-equal '((6 . g) (7 . h) (8 . i) (9 . j) (10 . k))
                          (fxmapping->alist
                           (fxmapping-open-closed-interval letter-fxmap 5 10)))
              (test-equal (list->dup-alist '(-8192 -4096 0 4096 8192 12288))
                          (fxmapping->alist
                           (fxmapping-open-closed-interval sparse-fxmap -12288 12288)))

              (test-equal (list->dup-alist '(-50 -25 0 25))
                          (fxmapping->alist
                           (fxmapping-closed-open-interval mixed-fxmap -50 50)))
              (test-equal '((5 . f) (6 . g) (7 . h) (8 . i) (9 . j))
                          (fxmapping->alist
                           (fxmapping-closed-open-interval letter-fxmap 5 10)))
              (test-equal (list->dup-alist '(-12288 -8192 -4096 0 4096 8192))
                          (fxmapping->alist
                           (fxmapping-closed-open-interval sparse-fxmap -12288 12288)))
              )

  (test-group "Submappings"
              (test-equal '((100 . 100)) (fxmapping->alist (fxsubmapping= mixed-fxmap 100)))
              (test-equal '((7 . h)) (fxmapping->alist (fxsubmapping= letter-fxmap 7)))
              (test-equal '((16384 . 16384))
                          (fxmapping->alist (fxsubmapping= sparse-fxmap 16384)))
              (test-eqv #t (fxmapping-empty? (fxsubmapping= sparse-fxmap 1)))

              (test-equal (list->dup-alist '(-100 -75 -50 -25))
                          (fxmapping->alist (fxsubmapping< mixed-fxmap 0)))
              (test-equal '((0 . a) (1 . b) (2 . c))
                          (fxmapping->alist (fxsubmapping< letter-fxmap 3)))
              (test-equal (list->dup-alist '(-65536 -61440 -57344))
                          (fxmapping->alist (fxsubmapping< sparse-fxmap -53248)))

              (test-equal (list->dup-alist '(25 50 75 100))
                          (fxmapping->alist (fxsubmapping> mixed-fxmap 0)))
              (test-equal '((23 . x) (24 . y) (25 . z))
                          (fxmapping->alist (fxsubmapping> letter-fxmap 22)))
              (test-equal (list->dup-alist '(57344 61440 65536))
                          (fxmapping->alist (fxsubmapping> sparse-fxmap 53248)))

              (test-equal (list->dup-alist '(-100 -75 -50 -25 0))
                          (fxmapping->alist (fxsubmapping<= mixed-fxmap 0)))
              (test-equal '((0 . a) (1 . b) (2 . c) (3 . d))
                          (fxmapping->alist (fxsubmapping<= letter-fxmap 3)))
              (test-equal (list->dup-alist '(-65536 -61440 -57344 -53248))
                          (fxmapping->alist (fxsubmapping<= sparse-fxmap -53248)))

              (test-equal (list->dup-alist '(0 25 50 75 100))
                          (fxmapping->alist (fxsubmapping>= mixed-fxmap 0)))
              (test-equal '((22 . w) (23 . x) (24 . y) (25 . z))
                          (fxmapping->alist (fxsubmapping>= letter-fxmap 22)))
              (test-equal (list->dup-alist '(53248 57344 61440 65536))
                          (fxmapping->alist (fxsubmapping>= sparse-fxmap 53248)))

              (test-equal (list (list->dup-alist (iota 5 -100 25))
                                (list->dup-alist (iota 4 25 25)))
                          (let ([fxmaps (values->list (fxmapping-split mixed-fxmap 0))])
                            (map fxmapping->alist fxmaps)))
              (test-equal (list '() sparse-seq)
                          (let*-values (((min-key _) (fxmapping-min sparse-fxmap))
                                        ((fxmaps) (values->list (fxmapping-split sparse-fxmap (- min-key 1)))))
                            (map fxmapping->alist fxmaps)))
              (test-equal (list sparse-seq '())
                          (let*-values (((max-key _) (fxmapping-max sparse-fxmap))
                                        ((fxmaps) (values->list (fxmapping-split sparse-fxmap (+ max-key 1)))))
                            (map fxmapping->alist fxmaps)))
              )

  (test-group "Relation map"
              (test-eqv #t
                        (fxmapping=? default-comp
                                     (fxmapping 0 #t)
                                     (fxmapping-relation-map (lambda (_k _v) (values 0 #t))
                                                             letter-fxmap)))
              (test-eqv #t
                        (fxmapping=? default-comp
                                     letter-fxmap
                                     (fxmapping-relation-map values letter-fxmap)))
              (test-equal '((0 . a) (1 . b) (2 . c))
                          (fxmapping->alist
                           (fxmapping-relation-map (lambda (k v) (values (- k) v))
                                                   (fxmapping 0 'a -1 'b -2 'c))))
              ))
