#lang racket/base

;;; SRFI-207 String-notated bytevectors

;;; The textual representation #u8"..." and related functions are not
;;; supported; use standard Racket bytestring #"..." syntax.

(require (for-syntax racket/base (only-in racket/string string-prefix?))
         racket/contract (only-in racket/list add-between)
         racket/port racket/require
         (only-in srfi/1 append-reverse)
         srfi/13 srfi/14 (only-in "133.rkt" vector-unfold)
         (only-in "160/u8.rkt" u8vector-for-each)
         "190.rkt")
(require (filtered-in
          (lambda (name)
            (and (string-prefix? name "unsafe-fx")
                 (substring name 7)))
          racket/unsafe/ops))
(require (only-in racket/unsafe/ops unsafe-string-ref unsafe-string-set! unsafe-string-length
                  unsafe-vector-ref unsafe-vector-set!
                  unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-length))
(module+ test (require rackunit))

(define bytestring-args/c (or/c byte? bytes? char? string?))

(provide
 (contract-out
  [bytestring-error? predicate/c]
  [bytestring (-> bytestring-args/c ... bytes?)]
  [make-bytestring (-> (listof bytestring-args/c) bytes?)]
  [make-bytestring! (-> bytes? exact-nonnegative-integer? (listof bytestring-args/c) void?)]
  [bytevector->hex-string (-> bytes? string?)]
  [hex-string->bytevector (-> string? bytes?)]
  [bytevector->base64 (->* (bytes?) (string?) string?)]
  [base64->bytevector (->* (string?) (string?) bytes?)]
  [bytestring->list (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) (listof (or/c char? byte?)))]
  [make-bytestring-generator (-> (or/c byte? char? bytes? string?) ... (-> (or/c byte? eof-object?)))]
  [bytestring-pad (-> bytes? exact-nonnegative-integer? (or/c byte? (char-in #\nul #\u7F)) bytes?)]
  [bytestring-pad-right (-> bytes? exact-nonnegative-integer? (or/c byte? (char-in #\nul #\u7F)) bytes?)]
  [bytestring-trim (-> bytes? (-> byte? any/c) bytes?)]
  [bytestring-trim-right (-> bytes? (-> byte? any/c) bytes?)]
  [bytestring-trim-both (-> bytes? (-> byte? any/c) bytes?)]
  [bytestring-replace (->* (bytes? bytes? exact-nonnegative-integer? exact-nonnegative-integer?) (exact-nonnegative-integer? exact-nonnegative-integer?) bytes?)]
  [bytestring<? (-> bytes? bytes? boolean?)]
  [bytestring<=? (-> bytes? bytes? boolean?)]
  [bytestring>? (-> bytes? bytes? boolean?)]
  [bytestring>=? (-> bytes? bytes? boolean?)]
  [bytestring-index (->* (bytes? (-> byte? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [bytestring-index-right (->* (bytes? (-> byte? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [bytestring-break (-> bytes? (-> byte? any/c) (values bytes? bytes?))]
  [bytestring-span (-> bytes? (-> byte? any/c) (values bytes? bytes?))]
  [bytestring-join (->* ((listof bytes?) bytestring-args/c) ((or/c 'infix 'strict-infix 'suffix 'prefix)) bytes?)]
  [bytestring-split (->* (bytes? (or/c byte? (char-in #\nul #\u7F))) ((or/c 'infix 'strict-infix 'suffix 'prefix)) (listof bytes?))]
  [write-textual-bytestring (->* (bytes?) (output-port?) void?)]
  [write-binary-bytestring (->* (output-port?) () #:rest (listof bytestring-args/c) void?)]
  [read-textual-bytestring (->* (boolean?) (input-port?) bytes?)]
  ))

(struct bytestring-error exn:fail:contract () #:extra-constructor-name make-bytestring-error #:transparent)
(define (raise-bytestring-error name msg)
  (raise (make-bytestring-error (format "~A: ~A" name msg) (current-continuation-marks))))

(define (format-args args)
  (let loop ([args args]
             [result '()])
    (if (null? args)
        (reverse result)
        (loop (cddr args) (cons (format "~%~%  ~A: ~S" (car args) (cadr args)) result)))))

(define (format-error-msg msg . args)
  (if (null? args)
      msg
      (apply string-append-immutable msg (format-args args))))

(define-syntax (check-bytestring-range stx)
  (syntax-case stx ()
    [(_ name bs idx)
     #'(when (or (< idx 0) (>= idx (bytes-length bs)))
          (raise-range-error name "bytestring" "" idx bs 0 (- (bytes-length bs) 1)))]
    [(_ name bs start end)
     #'(cond
         ((< start 0)
          (raise-range-error name "bytestring" "starting " start bs 0 (bytes-length bs)))
         ((or (< end start) (> end (bytes-length bs)))
          (raise-range-error name "bytestring" "ending " end bs start (bytes-length bs) 0))
         (else (void)))]))

(define (validate-bytestring-args name args)
  (if (null? args)
      #t
      (let ([arg (car args)])
        (cond
          ((byte? arg)
           (validate-bytestring-args name (cdr args)))
          ((bytes? arg)
           (validate-bytestring-args name (cdr args)))
          ((and (char? arg) (char-set-contains? char-set:ascii arg))
           (validate-bytestring-args name (cdr args)))
          ((and (string? arg) (string-every char-set:ascii arg))
           (validate-bytestring-args name (cdr args)))
          (else
           (raise-bytestring-error name (format-error-msg "value out of range" "value" arg)))))))

(define (bytestring . args)
  (%make-bytestring 'bytestring args))

(define (make-bytestring args)
  (%make-bytestring 'make-bytestring args))

(define (%make-bytestring name args)
  (let loop ([res '()]
             [args args])
    (if (null? args)
        (list->bytes (reverse res))
        (let ([arg (car args)])
          (cond
            ((byte? arg)
             (loop (cons arg res) (cdr args)))
            ((bytes? arg)
             (loop (append-reverse (bytes->list arg) res) (cdr args)))
            ((and (char? arg) (char-set-contains? char-set:ascii arg))
             (loop (cons (char->integer arg) res) (cdr args)))
            ((and (string? arg) (string-every char-set:ascii arg))
             (loop (append-reverse (map char->integer (string->list arg)) res) (cdr args)))
            (else
             (raise-bytestring-error 'make-bytestring (format-error-msg "value out of range" "value" arg))))))))

(define (make-bytestring! bs at args)
  (check-bytestring-range 'make-bytestring! bs at)
  (let loop ([at at]
             [args args])
    (if (null? args)
        (void)
        (let ([arg (car args)])
          (cond
            ((byte? arg)
             (bytes-set! bs at arg)
             (loop (+ at 1) (cdr args)))
            ((bytes? arg)
             (bytes-copy! bs at arg)
             (loop (+ at (bytes-length arg)) (cdr args)))
            ((and (char? arg) (char-set-contains? char-set:ascii arg))
             (bytes-set! bs at (char->integer arg))
             (loop (+ at 1) (cdr args)))
            ((and (string? arg) (string-every char-set:ascii arg))
             (for ([ch (in-string arg)]
                   [n (in-naturals at)])
               (bytes-set! bs n (char->integer ch)))
             (loop (+ at (string-length arg)) (cdr args)))
            (else
             (raise-bytestring-error 'make-bytestring! (format-error-msg "value out of range" "value" arg))))))))

(define (hex-string? s)
  (and (even? (string-length s))
       (string-every char-set:hex-digit s)))

(define hex-chars #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
(define (bytevector->hex-string bs)
  (let ([s (make-string (* (unsafe-bytes-length bs) 2))])
    (do ([i 0 (fx+ i 2)]
         [n 0 (fx+ n 1)])
        ((fx= i (unsafe-string-length s)) s)
      (let ([b (unsafe-bytes-ref bs n)])
        (unsafe-string-set! s i (unsafe-vector-ref hex-chars (fxrshift (fxand b #xF0) 4)))
        (unsafe-string-set! s (fx+ i 1) (unsafe-vector-ref hex-chars (fxand b #x0F)))))))

(define (hex-string->bytevector hs)
  (unless (hex-string? hs)
    (raise-bytestring-error 'hex-string->bytevector "argument is not a valid base-16 string"))
  (let ([bs (make-bytes (fxquotient (unsafe-string-length hs) 2))]
        [buffer (make-string 2)])
    (do ([i 0 (fx+ i 2)]
         [n 0 (fx+ n 1)])
        ((fx= i (unsafe-string-length hs)) bs)
      (string-set! buffer 0 (unsafe-string-ref hs i))
      (string-set! buffer 1 (unsafe-string-ref hs (fx+ i 1)))
      (bytes-set! bs n (string->number buffer 16)))))

;;;; Reduced and heavily modified base64 library from chibi-scheme.
;;;
;;; Copyright (c) 2009-2018 Alex Shinn
;;; All rights reserved.
;;; Converted to Racket by Shawn Wagner
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Constants and tables

(define outside-char 99) ; luft-balloons
(define pad-char 101)    ; dalmations

(define (outside-char? x) (eqv? x outside-char))
(define (pad-char? x) (eqv? x pad-char))

(define (make-base64-decode-table digits)
  (let ((extra-1 (char->integer (string-ref digits 0)))
        (extra-2 (char->integer (string-ref digits 1))))
    (vector-unfold
     (lambda (i)
       (cond ((and (fx>= i 48) (fx< i 58)) (fx+ i 4))   ; numbers
             ((and (fx>= i 65) (fx< i 91)) (fx- i 65))  ; upper case letters
             ((and (fx>= i 97) (fx< i 123)) (fx- i 71)) ; lower case letters
             ((fx= i extra-1) 62)
             ((fx= i extra-2) 63)
             ((fx= i 61) pad-char)                  ; '='
             (else outside-char)))
     #x100)))

(define (base64-decode-u8 table u8)
  (unsafe-vector-ref table u8))

(define (make-base64-encode-table digits)
  (vector-unfold
   (lambda (i)
     (cond ((fx< i 26) (fx+ i 65))  ; upper-case letters
           ((fx< i 52) (fx+ i 71))  ; lower-case letters
           ((fx< i 62) (fx- i 4))   ; numbers
           ((fx= i 62) (char->integer (string-ref digits 0)))
           ((fx= i 63) (char->integer (string-ref digits 1)))
           (else (error "out of range"))))
   64))

;;;; Decoding

(define (decode-base64-string src digits)
  (let ((table (make-base64-decode-table digits)))
    (call-with-output-bytes
     (lambda (out) (decode-base64-to-port src out table)))))

;; Loop through src, writing decoded base64 data to port in chunks
;; of up to three bytes.
(define (decode-base64-to-port src port table)
  (let ((len (unsafe-string-length src)))
    (let lp ((i 0) (b1 outside-char) (b2 outside-char) (b3 outside-char))
      (if (fx= i len)
          (decode-base64-trailing port b1 b2 b3)
          (let* ((c (unsafe-string-ref src i))
                 (b (base64-decode-u8 table (char->integer c))))
            (cond ((pad-char? b) (decode-base64-trailing port b1 b2 b3))
                  ((char-whitespace? c) (lp (fx+ i 1) b1 b2 b3))
                  ((outside-char? b)
                   (raise-bytestring-error 'base64->bytevector (format-error-msg "invalid character in base64 string" "char" c "src" src)))
                  ((outside-char? b1) (lp (fx+ i 1) b b2 b3))
                  ((outside-char? b2) (lp (fx+ i 1) b1 b b3))
                  ((outside-char? b3) (lp (fx+ i 1) b1 b2 b))
                  (else
                   (write-byte (fxior (fxlshift b1 2)
                                      (bitwise-bit-field b2 4 6))
                               port)
                   (write-byte (fxior
                                (fxlshift (bitwise-bit-field b2 0 4) 4)
                                (bitwise-bit-field b3 2 6))
                               port)
                   (write-byte (fxior
                                (fxlshift (bitwise-bit-field b3 0 2) 6)
                                b)
                               port)
                   (lp (fx+ i 1) outside-char outside-char outside-char))))))))

;; Flush any trailing bits accumulated in the decode loop to the
;; bytevector port `out', then return the finalized bytestring.
(define (decode-base64-trailing out b1 b2 b3)
  (cond ((outside-char? b1) #t)
        ((outside-char? b2) (write-byte (fxlshift b1 2) out))
        (else
         (write-byte (fxior (fxlshift b1 2) (bitwise-bit-field b2 4 6))
                   out)
         (unless (outside-char? b3)
           (write-byte (fxior (fxlshift (bitwise-bit-field b2 0 4) 4)
                                    (bitwise-bit-field b3 2 6))
                       out)))))

;;;; Encoding

(define (base64-encode-bytevector bv digits)
  (let* ((len (bytes-length bv))
         (quot (fxquotient len 3))
         (rem (fx- len (fx* quot 3)))
         (res-len (fxlshift (fx+ quot (if (zero? rem) 0 1)) 2))
         (res (make-bytes res-len))
         (table (make-base64-encode-table digits)))
    (base64-encode-bytevector! bv 0 len res table)
    res))

(define (base64-encode-bytevector! bv start end res table)
  (let ((limit (fx- end 2))
        (enc (lambda (i) (vector-ref table i))))
    (let lp ((i start) (j 0))
      (if (fx>= i limit)
          (case (fx- end i)
            ((1)
             (let ((b1 (bytes-ref bv i)))
               (bytes-set! res j (enc (fxrshift b1 2)))
               (bytes-set!
                res
                (fx+ j 1)
                (enc (fxlshift (fxand #b11 b1) 4)))
               (bytes-set! res (fx+ j 2) (char->integer #\=))
               (bytes-set! res (fx+ j 3) (char->integer #\=))
               (fx+ j 4)))
            ((2)
             (let ((b1 (bytes-ref bv i))
                   (b2 (bytes-ref bv (fx+ i 1))))
               (bytes-set! res j (enc (fxrshift b1 2)))
               (bytes-set!
                res
                (fx+ j 1)
                (enc (fxior
                      (fxlshift (fxand #b11 b1) 4)
                      (bitwise-bit-field b2 4 8))))
               (bytes-set!
                res
                (fx+ j 2)
                (enc (fxlshift (bitwise-bit-field b2 0 4) 2)))
               (bytes-set! res (fx+ j 3) (char->integer #\=))
               (fx+ j 4)))
            (else
             j))
          (let ((b1 (bytes-ref bv i))
                (b2 (bytes-ref bv (fx+ i 1)))
                (b3 (bytes-ref bv (fx+ i 2))))
            (bytes-set! res j (enc (fxrshift b1 2)))
            (bytes-set!
             res
             (fx+ j 1)
             (enc (fxior
                   (fxlshift (fxand #b11 b1) 4)
                   (bitwise-bit-field b2 4 8))))
            (bytes-set!
             res
             (fx+ j 2)
             (enc (fxior
                   (fxlshift (bitwise-bit-field b2 0 4) 2)
                   (bitwise-bit-field b3 6 8))))
            (bytes-set! res (fx+ j 3) (enc (fxand #b111111 b3)))
            (lp (fx+ i 3) (fx+ j 4)))))))

(define (bytevector->base64 bs [digits "+/"])
  (bytes->string/utf-8 (base64-encode-bytevector bs digits)))

(define (base64->bytevector s64 [digits "+/"])
  (decode-base64-string s64 digits))

(define (bytestring->list bs [start 0] [end (bytes-length bs)])
  (check-bytestring-range 'bytestring->list bs start end)
  (for/list ([b (in-bytes bs start end)])
    (if (fx<= 32 b 127)
        (integer->char b)
        b)))

(define (make-bytestring-generator . args)
  (when (validate-bytestring-args 'make-bytestring-generator args)
    (coroutine-generator
     (for ([arg (in-list args)])
       (cond
         ((byte? arg)
          (yield arg))
         ((bytes? arg)
          (for ([b (in-bytes arg)])
            (yield b)))
         ((char? arg)
          (yield (char->integer arg)))
         ((string? arg)
          (for ([ch (in-string arg)])
            (yield (char->integer ch)))))))))

(define (bytestring-pad bs len fill)
  (let ([fill (if (char? fill) (char->integer fill) fill)])
    (if (>= (bytes-length bs) len)
        (bytes-copy bs)
        (bytes-append (make-bytes (- len (bytes-length bs)) fill) bs))))

(define (bytestring-pad-right bs len fill)
  (let ([fill (if (char? fill) (char->integer fill) fill)])
    (if (>= (bytes-length bs) len)
        (bytes-copy bs)
        (bytes-append bs (make-bytes (- len (bytes-length bs)) fill)))))

(define (bytestring-trim bs pred?)
  (let ([start (bytestring-index bs (lambda (b) (not (pred? b))))])
    (if start
        (subbytes bs start)
        #"")))

(define (bytestring-trim-right bs pred?)
  (let ([end (bytestring-index-right bs (lambda (b) (not (pred? b))))])
    (if end
        (subbytes bs 0 (fx+ end 1))
        #"")))

(define (bytestring-trim-both bs pred?)
  (let* ([not-pred? (lambda (b) (not (pred? b)))]
         [start (bytestring-index bs not-pred?)]
         [end (bytestring-index-right bs not-pred?)])
    (if (and start end (fx<= start end))
        (subbytes bs start (fx+ end 1))
        #"")))

(define (bytestring-replace bs1 bs2 start1 end1 [start2 0] [end2 (bytes-length bs2)])
  (check-bytestring-range 'bytestring-replace bs1 start1 end1)
  (check-bytestring-range 'bytestring-replace bs2 start2 end2)
  (bytes-append (subbytes bs1 0 start1) (subbytes bs2 start2 end2) (subbytes bs1 end1)))

(define (bytestring<? a b) (bytes<? a b))

(define (bytestring<=? a b)
  (let ([a-len (bytes-length a)]
        [b-len (bytes-length b)])
    (let loop ([n 0])
      (cond
        ((and (fx= a-len n)
              (fx= b-len n))
         #t)
        ((fx= a-len n) #t)
        ((fx= b-len n) #f)
        ((fx<= (unsafe-bytes-ref a n) (unsafe-bytes-ref b n))
         (loop (fx+ n 1)))
        (else #f)))))

(define (bytestring>? a b) (bytes>? a b))

(define (bytestring>=? a b)
  (let ([a-len (unsafe-bytes-length a)]
        [b-len (unsafe-bytes-length b)])
    (let loop ([n 0])
      (cond
        ((and (fx= a-len n)
              (fx= b-len n))
         #t)
        ((fx= a-len n) #f)
        ((fx= b-len n) #t)
        ((fx>= (unsafe-bytes-ref a n) (unsafe-bytes-ref b n))
         (loop (fx+ n 1)))
        (else #f)))))

(define (bytestring-index bs pred? [start 0] [end (bytes-length bs)])
  (check-bytestring-range 'bytestring-index bs start end)
  (for/or ([i (in-range start end)])
    (if (pred? (unsafe-bytes-ref bs i))
        i
        #f)))

(define (bytestring-index-right bs pred? [start 0] [end (bytes-length bs)])
  (check-bytestring-range 'bytestring-index-right bs start end)
  (for/or ([i (in-inclusive-range (- end 1) start -1)])
    (if (pred? (unsafe-bytes-ref bs i))
        i
        #f)))

(define (bytestring-break bs pred?)
  (let ([i (bytestring-index bs pred?)])
    (if i
        (values (subbytes bs 0 i) (subbytes bs i))
        (values (bytes-copy bs) #""))))

(define (bytestring-span bs pred?)
  (let ([i (bytestring-index bs (lambda (b) (not (pred? b))))])
    (if i
        (values (subbytes bs 0 i) (subbytes bs i))
        (values (bytes-copy bs) #""))))

(define (bytestring-join bs-list delim [grammar 'infix])
  (when (and (eq? grammar 'strict-infix)
             (null? bs-list))
    (raise-bytestring-error 'bytestring-join "given empty list and strict-infix grammar"))
  (make-bytestring
   (case grammar
     ((infix strict-infix)
      (add-between bs-list delim))
     ((suffix)
      (let loop ([res '()]
                 [bs-list bs-list])
        (if (null? bs-list)
            (reverse res)
            (loop
             (cons delim (cons (car bs-list) res))
             (cdr bs-list)))))
     ((prefix)
      (let loop ([res '()]
                 [bs-list bs-list])
        (if (null? bs-list)
            (reverse res)
            (loop
             (cons (car bs-list) (cons delim res))
             (cdr bs-list)))))
     (else (raise-bytestring-error 'bytestring-join (format-error-msg "unknown grammar value" "grammar" grammar))))))

(define (bytestring-split bs delim [grammar 'infix])
  (unless (memq grammar '(infix strict-infix prefix suffix))
    (raise-bytestring-error 'bytestring-split (format-error-msg "unknown grammar value" "grammar" grammar)))
  (if (fx= (unsafe-bytes-length bs) 0)
      '()
      (let* ([delim (if (char? delim) (char->integer delim) delim)]
             [pred? (lambda (b) (fx= b delim))]
             [start (if (and (eq? grammar 'prefix) (fx= (unsafe-bytes-ref bs 0) delim)) 1 0)]
             [end (if (and (eq? grammar 'suffix) (fx= (unsafe-bytes-ref bs (- (unsafe-bytes-length bs) 1)) delim)) (- (unsafe-bytes-length bs) 1) (unsafe-bytes-length bs))])
        (let loop ([i start]
                   [res '()])
          (let ([pos (bytestring-index bs pred? i end)])
            (if pos
                (loop (+ pos 1) (cons (subbytes bs i pos) res))
                (reverse (cons (subbytes bs i end) res))))))))

;;;; I/O mostly from reference implementation

(define backslash-codepoints
  (make-immutable-hasheqv '((7 . #\a) (8 . #\b) (9 . #\t) (10 . #\n) (13 . #\r)
                                      (34 . #\") (92 . #\\) (124 . #\|))))

(define (write-bytestring-byte port b)
  (cond
    ((hash-has-key? backslash-codepoints b)
     (write-char #\\ port)
     (write-char (hash-ref backslash-codepoints b) port))
    ((and (fx>= b #x20) (fx<= b #x7e))
     (write-char (integer->char b) port))
    (else
     (fprintf port "\\x~x;" b))))

(define (write-textual-bytestring bstring [port (current-output-port)])
  (write-string "#u8\"" port)
  (u8vector-for-each (lambda (b) (write-bytestring-byte port b)) bstring)
  (write-char #\" port))

(define (write-binary-bytestring port . args)
  (validate-bytestring-args 'write-binary-bytestring args)
  (for ([elem (in-list args)])
    (cond
      ((byte? elem)
       (write-bytestring-byte port elem))
      ((bytes? elem)
       (u8vector-for-each (lambda (b) (write-bytestring-byte port b)) elem))
      ((char? elem)
       (write-bytestring-byte port (char->integer elem)))
      ((string? elem)
       (string-for-each (lambda (ch) (write-bytestring-byte port (char->integer ch))) elem)))))

;;; Simple parser for string-notated bytevectors.

(define (parse prefix)
  (when prefix (consume-prefix))
  (consume-quote)
  (let lp ((c (read-char)))
    (cond ((eof-object? c) (raise-bytestring-error 'read-textual-bytestring "unexpected EOF"))
          ((char=? c #\") (void))  ; terminating quote
          ((char=? c #\\)
           (let ((c* (read-char)))
             (cond ((eof-object? c*)
                    (raise-bytestring-error 'read-textual-bytestring "incomplete escape sequence"))
                   ((escape c*) =>
                    (lambda (b)
                      (write-byte b)
                      (lp (read-char))))
                   (else (lp (read-char))))))
          ((and (char>=? c #\space) (char<=? c #\~))
           (write-byte (char->integer c))
           (lp (read-char)))
          (else (raise-bytestring-error 'read-textual-bytestring (format-error-msg "invalid character" "char" c))))))

(define (consume-quote)
  (let ((c (read-char)))
    (cond ((eof-object? c) (raise-bytestring-error 'read-textual-bytestring "unexpected EOF"))
          ((char=? c #\") #t)
          (else
           (raise-bytestring-error 'read-textual-bytestring (format-error-msg "invalid character" "char" c "expected" #\"))))))

(define (consume-prefix)
  (let ((s (read-string 3)))
    (cond ((eof-object? s) (raise-bytestring-error 'read-textual-bytestring "unexpected EOF"))
          ((string=? s "#u8") #t)
          (else (raise-bytestring-error 'read-textual-bytestring (format-error-msg "invalid bytestring prefix" "prefix" s "expected" "#u8"))))))

(define (escape c)
  (case c
    ((#\a) 7)
    ((#\b) 8)
    ((#\t) 9)
    ((#\n) 10)
    ((#\r) 13)
    ((#\") 34)
    ((#\\) 92)
    ((#\|) 124)
    ((#\x) (parse-hex))
    ((#\newline)
     (skip-horizontal-whitespace)
     #f)                              ; skip
    (else
     (cond ((char-whitespace? c)
            (skip-horizontal-whitespace)
            (skip-line-break)
            #f)
           (else (raise-bytestring-error 'read-textual-bytestring (format-error-msg "invalid escaped character" "char" c)))))))

(define (parse-hex)
  (let* ((hex1 (read-char))
         (hex2 (read-char)))
    (when (or (eof-object? hex1) (eof-object? hex2))
      (raise-bytestring-error 'read-textual-bytestring "incomplete hexadecimal sequence"))
    (if (char=? hex2 #\;)
        (or (string->number (string hex1) 16)
            (raise-bytestring-error 'read-textual-bytestring "invalid hexadecimal sequence"))
        (let ((term (read-char)))
          (if (eqv? term #\;)
              (or (string->number (string hex1 hex2) 16)
                  (raise-bytestring-error 'read-textual-bytestring "invalid hexadecimal sequence"))
              (raise-bytestring-error 'read-textual-bytestring
                                      "overlong or unterminated hexadecimal sequence"))))))

(define (skip-line-break)
  (let ((c (read-char)))
    (unless (eqv? #\newline c)
      (raise-bytestring-error 'read-textual-bytestring (format-error-msg "expected newline" "char" c))))
  (skip-horizontal-whitespace))

(define (skip-horizontal-whitespace)
  (let lp ((c (peek-char)))
    (when (and (char-whitespace? c) (not (char=? c #\newline)))
      (read-char)
      (lp (peek-char)))))

(define (read-textual-bytestring prefix [in (current-input-port)])
  (call-with-output-bytes
   (lambda (out)
     (parameterize ((current-input-port in)
                    (current-output-port out))
       (parse prefix)))))

(module+ test
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

  (require racket/function
           (only-in "158.rkt" generator->list)
           (only-in srfi/1 every list-tabulate))

  (define-syntax check
    (syntax-rules (=>)
      ((check expr => expected)
       (check-equal? expr expected))))

  (define (print-header msg) #;(displayln msg) (void))

  ;;;; Utility

  (define always (const #t))
  (define never (const #f))

  ;; Returns a list of the values produced by expr.
  (define-syntax values~>list
    (syntax-rules ()
      ((_ expr)
       (call-with-values (lambda () expr) list))))

  ;; If expr causes an exception to be raised, return 'bytestring-error
  ;; if the raised object satisfies bytestring-error?, and #f otherwise.
  (define-syntax catch-bytestring-error
    (syntax-rules ()
      ((_ expr)
       (with-handlers ([bytestring-error? (lambda (e) 'bytestring-error)]
                       [exn? (lambda (e) #f)])
         expr))))

  ;; Testing shorthand for write-binary-bytestring.
  (define (%bytestring/IO . args)
    (call-with-output-bytes
     (lambda (port)
       (apply write-binary-bytestring port args))))

  ;; Testing shorthands for SNB I/O.  Coverage library fans, eat your
  ;; hearts out.
  (define (parse-SNB/prefix s)
    (call-with-input-string s
                            (lambda (p)
                              (read-textual-bytestring #t p))))

  (define (parse-SNB s)
    (call-with-input-string s
                            (lambda (p)
                              (read-textual-bytestring #f p))))

  (define (%bytestring->SNB bstring)
    (call-with-output-string
     (lambda (port)
       (write-textual-bytestring bstring port))))


  (define test-bstring (bytestring "lorem"))

  (define homer
    (bytestring "The Man, O Muse, informe, who many a way / Wound in his wisedome to his wished stay;"))

  (define homer64
    "VGhlIE1hbiwgTyBNdXNlLCBpbmZvcm1lLCB3aG8gbWFueSBhIHdheSAvIFdvdW5kIGluIGhpcyB3aXNlZG9tZSB0byBoaXMgd2lzaGVkIHN0YXk7")

  (define homer64-w
    "VGhlIE1hb iwgTyBNdXNlL CBpbmZvcm1lL\nCB3aG8gbWF\tueSBhIH\rdheSAvIFdvdW5kIGluI   GhpcyB    3aXNlZ\t\t\nG9tZSB0b    yBoaXMgd\t2lzaGVkIHN0YXk7")

  ;;;; Constructors

  (define (check-constructor)
    (print-header "Running constructor tests...")
    (check (bytestring "lo" #\r #x65 (bytes #x6d)) => test-bstring)
    (check (bytestring)                         => (bytes))

    (check (catch-bytestring-error (bytestring #x100)) => 'bytestring-error)
    (check (catch-bytestring-error (bytestring "λ"))   => 'bytestring-error))

  (define (check-conversion)
    (print-header "Running conversion tests...")

    (check (bytevector->hex-string test-bstring) => "6c6f72656d")
    (check (hex-string->bytevector "6c6f72656d") => test-bstring)
    (check (catch-bytestring-error
            (hex-string->bytevector "c6f72656d"))
           => 'bytestring-error)
    (check (catch-bytestring-error
            (hex-string->bytevector "6czf72656d"))
           => 'bytestring-error)
    (check (equal? (hex-string->bytevector (bytevector->hex-string homer))
                   homer)
           => #t)

    (check (hex-string->bytevector (bytevector->hex-string (bytes ))) => (bytes ))


    (check (bytevector->base64 test-bstring)             => "bG9yZW0=")
    (check (bytevector->base64 (bytes #xff #xef #xff))      => "/+//")
    (check (bytevector->base64 (bytes #xff #xef #xff) "*@") => "@*@@")
    (check (equal? (bytevector->base64 homer) homer64)   => #t)
    (check (bytevector->base64 (bytes 1))                   => "AQ==")
    (check (bytevector->base64 (bytes ))                    => "")
    (check (base64->bytevector "bG9yZW0=")               => test-bstring)
    (check (base64->bytevector "/+//")                   => (bytes #xff #xef #xff))
    (check (base64->bytevector "@*@@" "*@")              => (bytes #xff #xef #xff))
    (check (equal? (base64->bytevector homer64) homer)   => #t)
    (check (equal? (base64->bytevector homer64-w) homer) => #t)
    (check (base64->bytevector "AQ==")                   => (bytes 1))
    (check (base64->bytevector "")                       => (bytes ))
    (check (base64->bytevector "\n\n\n==\t\r\n")         => (bytes ))
    (check (catch-bytestring-error
            (base64->bytevector "bG9@frob"))             => 'bytestring-error)


    (check (bytestring->list (bytes )) => '())
    (check (bytestring->list (bytestring 70 82 0 66)) => '(#\F #\R 0 #\B))
    (check (bytestring->list (bytestring "\a\t\t\n" 200)) => '(7 9 9 10 200))
    (check (make-bytestring (bytestring->list test-bstring)) => test-bstring)
    (check (make-bytestring (bytestring->list test-bstring 2))
           => (bytestring "rem"))
    (check (make-bytestring (bytestring->list test-bstring 1 3))
           => (bytestring "or"))

    (let ((bvec (make-bytes 5)))
      (check (begin
               (make-bytestring! bvec 0 '(#x6c #x6f #x72 #x65 #x6d))
               bvec)
             => test-bstring))
    (let ((bvec (make-bytes 9 #x20)))
      (check (begin (make-bytestring! bvec 2 `("lo" #\r #x65 ,(bytes #x6d)))
                    bvec)
             => (bytestring "  lorem  ")))
    (check (catch-bytestring-error (make-bytestring '("λ")))
           => 'bytestring-error)
    (check (catch-bytestring-error (make-bytestring '(#x100)))
           => 'bytestring-error)

    (let ((s (list-tabulate (bytes-length test-bstring)
                            (lambda (i)
                              (bytes-ref test-bstring i)))))
      (check (let ((g (make-bytestring-generator "lo" #\r #x65 (bytes #x6d))))
               (generator->list g))
             => s))
    (check (catch-bytestring-error (make-bytestring-generator "λ" #\m #\u))
           => 'bytestring-error)
    (check (catch-bytestring-error (make-bytestring-generator 89 90 300))
           => 'bytestring-error)
    )

  (define (check-selection)
    (print-header "Running selection tests...")

    (check (bytestring-pad test-bstring (bytes-length test-bstring) #x7a)
           => test-bstring)
    (check (bytes->string/utf-8 (bytestring-pad test-bstring 8 #x7a))
           => "zzzlorem")
    (check (equal? (bytestring-pad test-bstring 8 #\z)
                   (bytestring-pad test-bstring 8 (char->integer #\z)))
           => #t)
    (check (bytestring-pad-right test-bstring
                                 (bytes-length test-bstring)
                                 #x7a)
           => test-bstring)
    (check (bytes->string/utf-8 (bytestring-pad-right test-bstring 8 #x7a))
           => "loremzzz")
    (check (equal? (bytestring-pad-right test-bstring 8 #\z)
                   (bytestring-pad-right test-bstring 8 (char->integer #\z)))
           => #t)

    (check (bytestring-trim test-bstring always) => (bytes ))
    (check (bytestring-trim test-bstring never)  => test-bstring)
    (check (bytestring-trim test-bstring (lambda (u8) (< u8 #x70)))
           => (bytes #x72 #x65 #x6d))
    (check (bytestring-trim-right test-bstring always) => (bytes ))
    (check (bytestring-trim-right test-bstring never)  => test-bstring)
    (check (bytestring-trim-right test-bstring (lambda (u8) (< u8 #x70)))
           => (bytes #x6c #x6f #x72))
    (check (bytestring-trim-both test-bstring always) => (bytes ))
    (check (bytestring-trim-both test-bstring never)  => test-bstring)
    (check (bytestring-trim-both test-bstring (lambda (u8) (< u8 #x70)))
           => (bytes #x72)))

  (define (check-replacement)
    (print-header "Running bytestring-replace tests...")

    (check (bytestring-replace test-bstring (bytestring "mists") 1 5 1 5)
           => (bytestring "lists"))
    (check (bytestring-replace test-bstring (bytestring "faded") 2 5 1 5)
           => (bytestring "loaded"))
    (check (bytestring-replace (make-bytes 5)
                               test-bstring
                               0
                               (bytes-length test-bstring))
           => test-bstring)

    (let ((bv1 (bytestring "food")) (bv2 (bytestring "od fo")))
      (check (bytestring-replace bv1 bv2 2 2 0 5) => (bytestring "food food")))
    (let ((bv1 (bytestring "food food")))
      (check (bytestring-replace bv1 (bytes) 2 7 0 0)
             => (bytestring "food")))
    )

  (define (check-comparison)
    (define short-bstring (bytestring "lore"))
    (define long-bstring (bytestring "lorem "))
    (define mixed-case-bstring (bytestring "loreM"))
    (print-header "Runnng comparison tests...")

    (check (bytestring<? test-bstring test-bstring)        => #f)
    (check (bytestring<? short-bstring test-bstring)       => #t)
    (check (bytestring<? mixed-case-bstring test-bstring)  => #t)
    (check (bytestring>? test-bstring test-bstring)        => #f)
    (check (bytestring>? test-bstring short-bstring)       => #t)
    (check (bytestring>? test-bstring mixed-case-bstring)  => #t)
    (check (bytestring<=? test-bstring test-bstring)       => #t)
    (check (bytestring<=? short-bstring test-bstring)      => #t)
    (check (bytestring<=? mixed-case-bstring test-bstring) => #t)
    (check (bytestring<=? test-bstring mixed-case-bstring) => #f)
    (check (bytestring<=? long-bstring test-bstring)       => #f)
    (check (bytestring>=? test-bstring test-bstring)       => #t)
    (check (bytestring>=? test-bstring short-bstring)      => #t)
    (check (bytestring>=? test-bstring mixed-case-bstring) => #t)
    (check (bytestring>=? mixed-case-bstring test-bstring) => #f)
    (check (bytestring>=? short-bstring test-bstring)      => #f)
    )

  (define (check-searching)
    (define (eq-r? b) (= b #x72))
    (define (lt-r? b) (< b #x72))
    (print-header "Running search tests...")

    (check (bytestring-index test-bstring always)     => 0)
    (check (bytestring-index test-bstring never)      => #f)
    (check (bytestring-index test-bstring always 3)   => 3)
    (check (bytestring-index test-bstring eq-r?) => 2)

    (check (bytestring-index-right test-bstring always)     => 4)
    (check (bytestring-index-right test-bstring never)      => #f)
    (check (bytestring-index-right test-bstring always 3)   => 4)
    (check (bytestring-index-right test-bstring eq-r?) => 2)

    (check (values~>list (bytestring-span test-bstring always))
           => (list test-bstring (bytes)))
    (check (values~>list (bytestring-span test-bstring never))
           => (list (bytes) test-bstring))
    (check (values~>list (bytestring-span test-bstring lt-r?))
           => (list (bytestring "lo") (bytestring "rem")))

    (check (values~>list (bytestring-break test-bstring always))
           => (list (bytes) test-bstring))
    (check (values~>list (bytestring-break test-bstring never))
           => (list test-bstring (bytes)))
    (check (values~>list (bytestring-break test-bstring eq-r?))
           => (list (bytestring "lo") (bytestring "rem"))))

  (define (check-join-and-split)
    (define test-segments (list (bytes 1) (bytes 2) (bytes 3)))
    (print-header "Running joining and splitting tests...")

    (check (bytestring-join test-segments (bytes 0))         => (bytes 1 0 2 0 3))
    (check (bytestring-join test-segments (bytes 0) 'prefix) => (bytes 0 1 0 2 0 3))
    (check (bytestring-join test-segments (bytes 0) 'suffix) => (bytes 1 0 2 0 3 0))
    (check (bytestring-join '() (bytes 0))                   => (bytes ))
    (check (bytestring-join test-segments #\space)        => (bytes 1 32 2 32 3))
    (check (bytestring-join test-segments 0)              => (bytes 1 0 2 0 3))
    (check (bytestring-join test-segments "AB")
           => (bytes 1 65 66 2 65 66 3))
    (check (bytestring-join test-segments (bytes 7 8))       => (bytes 1 7 8 2 7 8 3))
    (check (catch-bytestring-error
            (bytestring-join test-segments 300))          => 'bytestring-error)
    (check (catch-bytestring-error
            (bytestring-join test-segments "λ"))          => 'bytestring-error)
    (check (catch-bytestring-error
            (bytestring-join '() (bytes 0) 'strict-infix))  => 'bytestring-error)
    (check (catch-bytestring-error
            (bytestring-join '() (bytes 0) 'foofix))        => 'bytestring-error)

    (check (bytestring-split (bytes 1 0 2 0 3) 0 'infix)    => test-segments)
    (check (bytestring-split (bytes 0 1 0 2 0 3) 0 'prefix) => test-segments)
    (check (bytestring-split (bytes 1 0 2 0 3 0) 0 'suffix) => test-segments)
    (check (bytestring-split (bytes 0 0) 0)                 => (list (bytes ) (bytes ) (bytes )))
    (check (bytestring-split (bytes ) 0)                    => '())
    (check (catch-bytestring-error
            (bytestring-split (bytes ) 0 'foofix))         => 'bytestring-error))

  (define (check-io)
    (print-header "Running I/O tests...")

    (check (%bytestring/IO "lo" #\r #x65 (bytes #x6d)) => test-bstring)
    (check (%bytestring/IO) => #"")
    (check (catch-bytestring-error (%bytestring/IO #x100)) => 'bytestring-error)
    (check (catch-bytestring-error (%bytestring/IO "λ")) => 'bytestring-error)

    ;;; read-textual-bytestring

    (check (parse-SNB/prefix "#u8\"\"") => #"")
    (check (parse-SNB/prefix "#u8\"lorem\"") => test-bstring)
    (check (parse-SNB/prefix "#u8\"\\xde;\\xad;\\xf0;\\x0d;\"")
           => (bytes #xde #xad #xf0 #x0d))
    (check (parse-SNB/prefix "#u8\"\\\"\\\\\\a\\b\\t\\n\\r\\|\"")
           => (bytestring #\" #\\ #\007 #\backspace #\tab #\newline #\return #\|))
    (check (parse-SNB/prefix "#u8\"lor\\\n\te\\   \r\n\tm\"")
           => test-bstring)
    (check (parse-SNB "\"lorem\"") => test-bstring)

    ;; Invalid SNB detection.
    (check (catch-bytestring-error (parse-SNB/prefix "#u\"lorem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8lorem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"lorem"))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"lorem"))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\orem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\    orem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\x6frem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"l\\x6z;rem\""))
           => 'bytestring-error)
    (check (catch-bytestring-error (parse-SNB/prefix "#u8\"α equivalence\""))
           => 'bytestring-error)

    ;;; write-textual-bytestring

    (check (%bytestring->SNB #"") => "#u8\"\"")
    (check (%bytestring->SNB test-bstring) => "#u8\"lorem\"")
    (check (%bytestring->SNB (bytes #xde #xad #xbe #xef))
           => "#u8\"\\xde;\\xad;\\xbe;\\xef;\"")
    (check (%bytestring->SNB
            (bytestring #\" #\\ #\007 #\backspace #\tab #\newline #\return #\|))
           => "#u8\"\\\"\\\\\\a\\b\\t\\n\\r\\|\"")

    (let ((test-bstrings
           (list (bytes 124 199 173 212 209 232 249 16 198 32 123 111 130 92 64 155)
                 (bytes 50 133 193 27 177 105 10 186 61 149 177 105 96 70 223 190)
                 (bytes 0 117 226 155 110 0 66 216 27 129 187 81 17 210 71 152)
                 (bytes 123 31 159 25 100 135 246 47 249 137 243 241 45 241 240 221)
                 (bytes 207 186 70 110 118 231 79 195 153 253 93 101 126 198 70 235)
                 (bytes 138 176 92 152 208 107 28 236 198 254 111 37 241 116 191 206)
                 (bytes 221 254 214 90 0 155 132 92 157 246 199 224 224 142 91 114)
                 (bytes 228 216 233 80 142 15 158 54 5 85 174 101 111 75 126 209)
                 (bytes 191 16 83 245 45 98 72 212 148 202 135 19 213 150 141 121)
                 (bytes 41 169 182 96 47 184 16 116 196 251 243 93 81 162 175 140)
                 (bytes 85 49 218 138 132 11 27 11 182 27 120 71 254 169 132 166)
                 (bytes 89 216 175 23 97 10 237 112 208 195 112 80 198 154 241 254)
                 (bytes 187 54 6 57 250 137 129 89 188 19 225 217 168 178 174 129)
                 (bytes 88 164 89 40 175 194 108 56 12 124 109 96 148 149 119 109)
                 (bytes 241 66 32 115 203 71 128 154 240 111 194 137 73 44 146 3)
                 (bytes 177 185 177 233 18 14 178 106 110 109 222 147 111 157 216 208))))
      (check
       (every (lambda (bvec)
                (equal? bvec (parse-SNB/prefix (%bytestring->SNB bvec))))
              test-bstrings)
       => #t))
    )

  (define (check-all)
    (check-constructor)
    (check-conversion)
    (check-selection)
    (check-replacement)
    (check-comparison)
    (check-searching)
    (check-join-and-split)
    (check-io))
  (check-all))
