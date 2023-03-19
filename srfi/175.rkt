#lang racket/base
;;; SRFI-175 ASCII character library

;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT

;; With modifications/optimizations for Racket by Shawn Wagner

(require racket/require racket/contract
         (for-syntax racket/base (only-in racket/string string-prefix?)))
(require (filtered-in
          (lambda (name)
            (and (or (string-prefix? name "unsafe-fx")
                     (string-prefix? name "unsafe-string")
                     (string=? name "unsafe-char->integer")
                     (string=? name "unsafe-integer->char"))
                 (substring name 7)))
            racket/unsafe/ops))
(module+ test (require rackunit))

(define char-or-cp-pred/c (-> (or/c char? fixnum?) boolean?))
(define char-or-cp2-pred/c (-> (or/c char? fixnum?) (or/c char? fixnum?) boolean?))
(define char-or-cp-conv/c (->i ([x (or/c char? fixnum?)]) () [_ (x) (if (char? x) char? fixnum?)]))
(define char-or-cp-conv-opt/c (->i ([x (or/c char? fixnum?)]) () [_ (x) (if (char? x) (or/c char? #f) (or/c fixnum? #f))]))

(provide
 (contract-out
  [ascii-codepoint? predicate/c]
  [ascii-char? predicate/c]
  [ascii-string? predicate/c]
  [ascii-bytevector? predicate/c]
  [ascii-control? char-or-cp-pred/c]
  [ascii-non-control? char-or-cp-pred/c]
  [ascii-whitespace? char-or-cp-pred/c]
  [ascii-space-or-tab? char-or-cp-pred/c]
  [ascii-other-graphic? char-or-cp-pred/c]
  [ascii-upper-case? char-or-cp-pred/c]
  [ascii-lower-case? char-or-cp-pred/c]
  [ascii-alphabetic? char-or-cp-pred/c]
  [ascii-alphanumeric? char-or-cp-pred/c]
  [ascii-numeric? char-or-cp-pred/c]
  [ascii-ci=? char-or-cp2-pred/c]
  [ascii-ci<? char-or-cp2-pred/c]
  [ascii-ci>? char-or-cp2-pred/c]
  [ascii-ci<=? char-or-cp2-pred/c]
  [ascii-ci>=? char-or-cp2-pred/c]
  [ascii-string-ci=? (-> string? string? boolean?)]
  [ascii-string-ci<? (-> string? string? boolean?)]
  [ascii-string-ci>? (-> string? string? boolean?)]
  [ascii-string-ci<=? (-> string? string? boolean?)]
  [ascii-string-ci>=? (-> string? string? boolean?)]
  [ascii-upcase char-or-cp-conv/c]
  [ascii-downcase char-or-cp-conv/c]
  [ascii-control->graphic char-or-cp-conv-opt/c]
  [ascii-graphic->control char-or-cp-conv-opt/c]
  [ascii-mirror-bracket char-or-cp-conv-opt/c]
  [ascii-nth-digit (-> exact-integer? (or/c char? #f))]
  [ascii-nth-upper-case (-> exact-integer? char?)]
  [ascii-nth-lower-case (-> exact-integer? char?)]
  [ascii-digit-value (-> (or/c char? fixnum?) (integer-in 2 10) (or/c fixnum? #f))]
  [ascii-upper-case-value (-> (or/c char? fixnum?) exact-integer? (integer-in 0 26) (or/c exact-integer? #f))]
  [ascii-lower-case-value (-> (or/c char? fixnum?) exact-integer? (integer-in 0 26) (or/c exact-integer? #f))]))

(define (ensure-int x)
  (if (char? x) (char->integer x) x))

(define (base-offset-limit x base offset limit)
  (let ((cc (ensure-int x)))
    (and (fx>= cc base) (fx< cc (fx+ base limit))
         (+ offset (fx- cc base)))))

(define (char->int->char map-int char)
  (let ((int (map-int (char->integer char))))
    (and int (integer->char int))))

;;

(define (ascii-codepoint? x)
  (and (exact-integer? x) (<= 0 x #x7f)))

(define (ascii-char? x)
  (and (char? x) (fx< (char->integer x) #x80)))

#;(define (ascii-bytevector? x)
  (and (bytevector? x)
       (let check ((i (- (bytevector-length x) 1)))
         (or (< i 0) (and (< (bytevector-u8-ref x i) #x80)
                          (check (- i 1)))))))

(define (ascii-bytevector? x)
  (and (bytes? x)
       (for/and ([b (in-bytes x)])
         (fx< b #x80))))

#;(define (ascii-string? x)
  (and (string? x)
       (call-with-port
        (open-input-string x)
        (lambda (in)
          (let check ()
            (let ((char (read-char in)))
              (or (eof-object? char)
                  (and (< (char->integer char) #x80) (check)))))))))

(define (ascii-string? x)
  (and (string? x)
       (for/and ([ch (in-string x)])
         (fx< (char->integer ch) #x80))))

(define (ascii-control? x)
  (let ((cc (ensure-int x)))
    (or (fx<= 0 cc #x1f) (fx= cc #x7f))))

(define (ascii-non-control? x)
  (let ((cc (ensure-int x)))
    (fx<= #x20 cc #x7e)))

(define (ascii-whitespace? x)
  (let ((cc (ensure-int x)))
    (cond ((fx< cc #x09) #f)
          ((fx< cc #x0e) #t)
          (else (fx= cc #x20)))))

(define (ascii-space-or-tab? x)
  (let ((cc (ensure-int x)))
    (case cc ((#x09 #x20) #t) (else #f))))

(define (ascii-other-graphic? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x21 cc #x2f)
        (fx<= #x3a cc #x40)
        (fx<= #x5b cc #x60)
        (fx<= #x7b cc #x7e))))

(define (ascii-upper-case? x)
  (let ((cc (ensure-int x)))
    (fx<= #x41 cc #x5a)))

(define (ascii-lower-case? x)
  (let ((cc (ensure-int x)))
    (fx<= #x61 cc #x7a)))

(define (ascii-alphabetic? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x41 cc #x5a)
        (fx<= #x61 cc #x7a))))

(define (ascii-alphanumeric? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x30 cc #x39)
        (fx<= #x41 cc #x5a)
        (fx<= #x61 cc #x7a))))

(define (ascii-numeric? x)
  (let ((cc (ensure-int x)))
    (fx<= #x30 cc #x39)))

;;

(define (ascii-digit-value x limit)
  (base-offset-limit x #x30 0 (min limit 10)))

(define (ascii-upper-case-value x offset limit)
  (base-offset-limit x #x41 offset (min limit 26)))

(define (ascii-lower-case-value x offset limit)
  (base-offset-limit x #x61 offset (fxmin limit 26)))

(define (ascii-nth-digit n)
  (and (exact-integer? n) (<= 0 n 9) (integer->char (fx+ #x30 n))))

(define (ascii-nth-upper-case n)
  (integer->char (fx+ #x41 (modulo n 26))))

(define (ascii-nth-lower-case n)
  (integer->char (fx+ #x61 (modulo n 26))))

(define (ascii-upcase x)
  (if (char? x)
      (integer->char (ascii-upcase (char->integer x)))
      (or (ascii-lower-case-value x #x41 26) x)))

(define (ascii-downcase x)
  (if (char? x)
      (integer->char (ascii-downcase (char->integer x)))
      (or (ascii-upper-case-value x #x61 26) x)))

(define (ascii-control->graphic x)
  (if (char? x)
      (char->int->char ascii-control->graphic x)
      (or (and (<= 0 x #x1f) (fx+ x #x40))
          (and (= x #x7f) #x3f))))

(define (ascii-graphic->control x)
  (if (char? x)
      (char->int->char ascii-graphic->control x)
      (or (and (<= #x40 x #x5f) (fx- x #x40))
          (and (= x #x3f) #x7f))))

(define (ascii-mirror-bracket x)
  (if (char? x)
      (case x
        ((#\() #\))
        ((#\)) #\()
        ((#\[) #\])
        ((#\]) #\[)
        ((#\{) #\})
        ((#\}) #\{)
        ((#\<) #\>)
        ((#\>) #\<)
        (else #f))
      (let ((x (ascii-mirror-bracket (integer->char x))))
        (and x (char->integer x)))))

(define (ascii-ci-cmp char1 char2)
  (let ((cc1 (ensure-int char1))
        (cc2 (ensure-int char2)))
    (when (fx<= #x41 cc1 #x5a) (set! cc1 (fx+ cc1 #x20)))
    (when (fx<= #x41 cc2 #x5a) (set! cc2 (fx+ cc2 #x20)))
    (cond ((fx< cc1 cc2) -1)
          ((fx> cc1 cc2) 1)
          (else 0))))

(define (ascii-ci=? char1 char2)
  (fx= (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci<? char1 char2)
  (fx< (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci>? char1 char2)
  (fx> (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci<=? char1 char2)
  (fx<= (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci>=? char1 char2)
  (fx>= (ascii-ci-cmp char1 char2) 0))

#;(define (ascii-string-ci-cmp string1 string2)
  (call-with-port
   (open-input-string string1)
   (lambda (in1)
     (call-with-port
      (open-input-string string2)
      (lambda (in2)
        (let loop ()
          (let ((char1 (read-char in1))
                (char2 (read-char in2)))
            (cond ((eof-object? char1) (if (eof-object? char2) 0 -1))
                  ((eof-object? char2) 1)
                  (else (let ((cc1 (char->integer char1))
                              (cc2 (char->integer char2)))
                          (when (<= #x41 cc1 #x5a) (set! cc1 (+ cc1 #x20)))
                          (when (<= #x41 cc2 #x5a) (set! cc2 (+ cc2 #x20)))
                          (cond ((< cc1 cc2) -1)
                                ((> cc1 cc2) 1)
                                (else (loop)))))))))))))

(define (ascii-string-ci-cmp string1 string2)
  (let loop ([i 0]
             [j 0])
    (cond
      ((fx= i (string-length string1))
       (if (fx= j (string-length string2)) 0 -1))
      ((fx= j (string-length string2))
       (if (fx= i (string-length string1)) 0 1))
      (else
       (let ([cc1 (char->integer (string-ref string1 i))]
             [cc2 (char->integer (string-ref string2 j))])
         (when (fx<= #x41 cc1 #x5a) (set! cc1 (fx+ cc1 #x20)))
         (when (fx<= #x41 cc2 #x5a) (set! cc2 (fx+ cc2 #x20)))
         (cond
           ((fx< cc1 cc2) -1)
           ((fx> cc1 cc2) 2)
           (else (loop (fx+ i 1) (fx+ j 1)))))))))

(define (ascii-string-ci=? string1 string2)
  (fx= (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci<? string1 string2)
  (fx< (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci>? string1 string2)
  (fx> (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci<=? string1 string2)
  (fx<= (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci>=? string1 string2)
  (fx>= (ascii-string-ci-cmp string1 string2) 0))

(module+ test
  (define-syntax-rule (want right-answer (proc args ...))
    (check-equal? (proc args ...) right-answer))

  (want #f (ascii-codepoint? -1))
  (want #t (ascii-codepoint? 0))
  (want #t (ascii-codepoint? #x7f))
  (want #f (ascii-codepoint? #x80))

  (want #t (ascii-char? (integer->char 0)))
  (want #t (ascii-char? (integer->char #x7f)))
  (want #f (ascii-char? (integer->char #x80)))

  (want #t (ascii-string? ""))
  (want #t (ascii-string? "a"))
  (want #t (ascii-string? "a b c"))
  (want #f (ascii-string? "å b o"))
  (want #t (ascii-string? (make-string 1 (integer->char #x7f))))
  (want #f (ascii-string? (make-string 1 (integer->char #x80))))

  (want #t (ascii-bytevector? (string->bytes/utf-8 "")))
  (want #t (ascii-bytevector? (string->bytes/utf-8 "a")))
  (want #t (ascii-bytevector? (string->bytes/utf-8 "a b c")))
  (want #f (ascii-bytevector? (string->bytes/utf-8 "å b o")))
  (want #t (ascii-bytevector?
            (string->bytes/utf-8 (make-string 1 (integer->char #x7f)))))
  (want #f (ascii-bytevector?
            (string->bytes/utf-8 (make-string 1 (integer->char #x80)))))

  (want #t (ascii-non-control? #\space))
  (want #f (ascii-non-control? #\tab))
  (want #f (ascii-non-control? #\newline))
  (want #f (ascii-non-control? (integer->char #x0d)))

  (want #t (ascii-space-or-tab? #\space))
  (want #t (ascii-space-or-tab? #\tab))
  (want #f (ascii-space-or-tab? #\newline))
  (want #f (ascii-non-control? (integer->char #x0d)))

  (want #f (ascii-non-control? (integer->char #x00)))
  (want #f (ascii-non-control? (integer->char #x1f)))
  (want #t (ascii-non-control? (integer->char #x20)))
  (want #t (ascii-non-control? (integer->char #x7e)))
  (want #f (ascii-non-control? (integer->char #x7f)))
  (want #f (ascii-non-control? (integer->char #x80)))

  (let ((lowers "abcdefghijklmnopqrstuvwxyz")
        (uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (let loop ((i 0))
      (when (< i 26)
        (let ((lower (string-ref lowers i))
              (upper (string-ref uppers i)))
          (want upper (ascii-upcase upper))
          (want upper (ascii-upcase lower))
          (want lower (ascii-downcase upper))
          (want lower (ascii-downcase lower))
          (loop (+ i 1))))))

  (let loop ((cc 0))
    (when (< cc #x80)
      (unless (ascii-alphabetic? cc)
        (want cc (ascii-upcase cc))
        (want cc (ascii-downcase cc)))
      (loop (+ cc 1))))

  (let loop ((cc 0))
    (when (< cc #x80)
      (want #f (ascii-char? cc))
      (want #t (ascii-char? (integer->char cc)))
      (cond ((ascii-alphabetic? cc)
             (want #t (ascii-upper-case? (ascii-upcase cc)))
             (want #t (ascii-lower-case? (ascii-downcase cc)))
             (want #f (ascii-lower-case? (ascii-upcase cc)))
             (want #f (ascii-upper-case? (ascii-downcase cc)))
             (want #t (ascii-alphanumeric? cc))
             (want #t (ascii-non-control? cc))
             (want #f (ascii-other-graphic? cc))
             (want #f (ascii-control? cc))
             (want #f (ascii-numeric? cc))
             (want #f (ascii-whitespace? cc))
             (want #f (ascii-space-or-tab? cc)))
            ((ascii-control? cc)
             (want #f (ascii-non-control? cc))
             (want #f (ascii-other-graphic? cc))
             (want cc
                   (ascii-graphic->control
                    (ascii-control->graphic cc)))
             (want (integer->char cc)
                   (ascii-graphic->control
                    (ascii-control->graphic (integer->char cc)))))
            ((member cc '(#\( #\) #\[ #\] #\{ #\} #\< #\>))
             (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))))
      (loop (+ cc 1))))

  (let outer ((a 0))
    (when (< a 26)
      (let inner ((b 0))
        (if (= b 26)
            (outer (+ a 1))
            (begin (want (= a b)  (ascii-ci=?
                                   (ascii-nth-lower-case a)
                                   (ascii-nth-upper-case b)))
                   (want (< a b)  (ascii-ci<?
                                   (ascii-nth-lower-case a)
                                   (ascii-nth-upper-case b)))
                   (want (<= a b) (ascii-ci<=?
                                   (ascii-nth-lower-case a)
                                   (ascii-nth-upper-case b)))
                   (want (> a b)  (ascii-ci>?
                                   (ascii-nth-lower-case a)
                                   (ascii-nth-upper-case b)))
                   (want (>= a b) (ascii-ci>=?
                                   (ascii-nth-lower-case a)
                                   (ascii-nth-upper-case b)))
                   (inner (+ b 1)))))))

  (want #t (ascii-ci>? #\A #\_))
  (want #t (ascii-ci>? #\Z #\_))

  (want #f (ascii-char? -1))
  (want #f (ascii-char? #x80))
  (want #f (ascii-char? (integer->char #x80)))

  (want #f (ascii-control? -1))
  (want #t (ascii-control? #x00))
  (want #t (ascii-control? #x1f))
  (want #f (ascii-control? #x20))
  (want #f (ascii-control? #x7e))
  (want #t (ascii-control? #x7f))
  (want #f (ascii-control? #x80))

  (want 0 (ascii-digit-value #\0 10))
  (want 0 (ascii-digit-value #\0 1))
  (want #f (ascii-digit-value #\0 0))
  (want #f (ascii-digit-value #\0 -1))
  (want 7 (ascii-digit-value #\7 8))
  (want #f (ascii-digit-value #\7 7))
  (want #f (ascii-digit-value #\: 10))

  (want 0 (ascii-upper-case-value #\A 0 26))
  (want 25 (ascii-upper-case-value #\Z 0 26))
  (want #f (ascii-upper-case-value #\Z 0 25))

  (want 0 (ascii-lower-case-value #\a 0 26))
  (want 25 (ascii-lower-case-value #\z 0 26))
  (want #f (ascii-lower-case-value #\z 0 25))

  (want 0 (ascii-lower-case-value #\a 0 1))
  (want #f (ascii-lower-case-value #\a 0 0))
  (want #f (ascii-lower-case-value #\a 0 -1))
  (want 9001 (ascii-lower-case-value #\b 9000 2))

  (want #f (ascii-nth-digit -1))
  (want #\0 (ascii-nth-digit 0))
  (want #\9 (ascii-nth-digit 9))
  (want #f (ascii-nth-digit 10))

  (want #\Z (ascii-nth-upper-case -1))
  (want #\A (ascii-nth-upper-case 0))
  (want #\Z (ascii-nth-upper-case 25))
  (want #\A (ascii-nth-upper-case 26))

  (want #\z (ascii-nth-lower-case -1))
  (want #\a (ascii-nth-lower-case 0))
  (want #\z (ascii-nth-lower-case 25))
  (want #\a (ascii-nth-lower-case 26))

  (define (count-matching predicates value)
    (let loop ((ps predicates) (n 0))
      (if (null? ps) n (loop (cdr ps) (if ((car ps) value) (+ n 1) n)))))

  (define (union? whole . parts)
    (let check ((cc 0))
      (or (= cc #x80)
          (if (and (whole cc) (not (= 1 (count-matching parts cc))))
              #f (check (+ cc 1))))))

  (define (subset? small-set . bigger-sets)
    (let check ((cc 0))
      (or (= cc #x80)
          (if (and (small-set cc) (= 0 (count-matching bigger-sets cc)))
              #f (check (+ cc 1))))))

  (define (disjoint? . predicates)
    (let check ((cc 0))
      (or (= cc #x80) (and (<= (count-matching predicates cc) 1)
                           (check (+ cc 1))))))

  (want #t (union? ascii-alphanumeric? ascii-alphabetic? ascii-numeric?))
  (want #t (union? ascii-alphabetic? ascii-upper-case? ascii-lower-case?))

  (want #t (subset? ascii-space-or-tab? ascii-whitespace?))
  (want #t (subset? ascii-other-graphic? ascii-non-control?))
  (want #t (subset? ascii-upper-case?   ascii-alphabetic? ascii-non-control?))
  (want #t (subset? ascii-lower-case?   ascii-alphabetic? ascii-non-control?))
  (want #t (subset? ascii-alphabetic?   ascii-alphanumeric? ascii-non-control?))
  (want #t (subset? ascii-numeric?      ascii-alphanumeric? ascii-non-control?))
  (want #t (subset? ascii-alphanumeric? ascii-non-control?))

  (want #t (disjoint? ascii-control? ascii-non-control?))
  (want #t (disjoint? ascii-whitespace?
                      ascii-other-graphic?
                      ascii-upper-case?
                      ascii-lower-case?
                      ascii-numeric?))
  (want #t (disjoint? ascii-control?
                      ascii-other-graphic?
                      ascii-upper-case?
                      ascii-lower-case?
                      ascii-numeric?))

  (define (check-string-ci a b cmp)
    (want (= cmp 0) (ascii-string-ci=? a b))
    (want (< cmp 0) (ascii-string-ci<? a b))
    (want (> cmp 0) (ascii-string-ci>? a b))
    (want (<= cmp 0) (ascii-string-ci<=? a b))
    (want (>= cmp 0) (ascii-string-ci>=? a b)))

  (check-string-ci "" "" 0)
  (check-string-ci "a" "a" 0)
  (check-string-ci "A" "a" 0)
  (check-string-ci "a" "A" 0)

  (check-string-ci "a" "b" -1)
  (check-string-ci "b" "a" 1)

  (check-string-ci "a" "B" -1)
  (check-string-ci "B" "a" 1)

  (check-string-ci "aa" "aa" 0)
  (check-string-ci "aa" "ab" -1)
  (check-string-ci "ab" "aa" 1)
  (check-string-ci "aa" "aaa" -1)
  (check-string-ci "aaa" "aa" 1))
