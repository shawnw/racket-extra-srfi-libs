#lang racket/base

;;; SRFI-207 String-notated bytevectors

;;; The textual representation #u8"..." and related functions are not
;;; supported; use standard Racket bytestring #"..." syntax.

(require (for-syntax racket/base (only-in racket/string string-prefix?))
         racket/contract (only-in racket/list add-between)
         racket/port racket/require
         (only-in srfi/1 append-reverse)
         srfi/13 srfi/14 (only-in srfi/43 vector-unfold)
         "190.rkt")
(require (filtered-in
          (lambda (name)
            (and (string-prefix? name "unsafe-fx")
                 (substring name 7)))
            racket/unsafe/ops))
(module+ test (require rackunit))

(provide
 (contract-out
  [bytestring-error? predicate/c]
  [bytestring (-> (or/c byte? char? bytes? string?) ... bytes?)]
  [make-bytestring (-> (listof (or/c byte? char? bytes? string?)) bytes?)]
  [make-bytestring! (-> bytes? exact-nonnegative-integer? (listof (or/c byte? char? bytes? string?)) void?)]
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
  [bytestring-join (->* ((listof bytes?) (or/c byte? char? bytes? string?)) ((or/c 'infix 'strict-infix 'suffix 'prefix)) bytes?)]
  [bytestring-split (->* (bytes? (or/c byte? (char-in #\nul #\u7F))) ((or/c 'infix 'strict-infix 'suffix 'prefix)) (listof bytes?))]
  ))

(struct bytestring-error exn:fail:contract () #:extra-constructor-name make-bytestring-error #:transparent)
(define (raise-bytestring-error name msg)
  (raise (make-bytestring-error (format "~A: ~A" name msg) (current-continuation-marks))))

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
             (raise-bytestring-error 'make-bytestring (format "value out of range~%~%  value: ~S" arg))))))))

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
             (raise-bytestring-error 'make-bytestring! (format "value out of range~%~%  value: ~S" arg))))))))

(define (hex-string? s)
  (and (even? (string-length s))
       (string-every char-set:hex-digit s)))

(define hex-chars #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
(define (bytevector->hex-string bs)
  (let ([s (make-string (* (bytes-length bs) 2))])
    (do ([i 0 (fx+ i 2)]
         [n 0 (fx+ n 1)])
        ((fx= i (string-length s)) s)
      (let ([b (bytes-ref bs n)])
        (string-set! s i (vector-ref hex-chars (fxrshift (fxand b #xF0) 4)))
        (string-set! s (fx+ i 1) (vector-ref hex-chars (fxand b #x0F)))))))

(define (hex-string->bytevector hs)
  (unless (hex-string? hs)
    (raise-bytestring-error 'hex-string->bytevector "argument is not a valid base-16 string"))
  (let ([bs (make-bytes (fxquotient (string-length hs) 2))]
        [buffer (make-string 2)])
    (do ([i 0 (fx+ i 2)]
         [n 0 (fx+ n 1)])
        ((fx= i (string-length hs)) bs)
      (string-set! buffer 0 (string-ref hs i))
      (string-set! buffer 1 (string-ref hs (fx+ i 1)))
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
  (vector-ref table u8))

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
  (let ((len (string-length src)))
    (let lp ((i 0) (b1 outside-char) (b2 outside-char) (b3 outside-char))
      (if (fx= i len)
          (decode-base64-trailing port b1 b2 b3)
          (let* ((c (string-ref src i))
                 (b (base64-decode-u8 table (char->integer c))))
            (cond ((pad-char? b) (decode-base64-trailing port b1 b2 b3))
                  ((char-whitespace? c) (lp (fx+ i 1) b1 b2 b3))
                  ((outside-char? b)
                   (raise-bytestring-error 'base64->bytevector (format "invalid character in base64 string~%~%  char: ~A~%  src: ~A" c src)))
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
  (unless (andmap (lambda (arg)
                    (or (byte? arg)
                        (bytes? arg)
                        (and (char? arg) (char-set-contains? char-set:ascii arg))
                        (and (string? arg) (string-every char-set:ascii arg))))
                  args)
    (raise-bytestring-error 'make-bytestring-generator (format "value out of range")))
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
          (yield (char->integer ch))))))))

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
        ((fx<= (bytes-ref a n) (bytes-ref b n))
         (loop (fx+ n 1)))
        (else #f)))))

(define (bytestring>? a b) (bytes>? a b))

(define (bytestring>=? a b)
  (let ([a-len (bytes-length a)]
        [b-len (bytes-length b)])
    (let loop ([n 0])
      (cond
        ((and (fx= a-len n)
              (fx= b-len n))
         #t)
        ((fx= a-len n) #f)
        ((fx= b-len n) #t)
        ((fx>= (bytes-ref a n) (bytes-ref b n))
         (loop (fx+ n 1)))
        (else #f)))))

(define (bytestring-index bs pred? [start 0] [end (bytes-length bs)])
  (check-bytestring-range 'bytestring-index bs start end)
  (for/or ([i (in-range start end)])
    (if (pred? (bytes-ref bs i))
        i
        #f)))

(define (bytestring-index-right bs pred? [start 0] [end (bytes-length bs)])
  (check-bytestring-range 'bytestring-index-right bs start end)
  (for/or ([i (in-inclusive-range (- end 1) start -1)])
    (if (pred? (bytes-ref bs i))
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
     (else (raise-bytestring-error 'bytestring-join (format "unknown grammar value~%~%  grammar: ~S" grammar))))))

(define (bytestring-split bs delim [grammar 'infix])
  (unless (memq grammar '(infix strict-infix prefix suffix))
    (raise-bytestring-error 'bytestring-split (format "unknown grammar value~%~%  grammar: ~S" grammar)))
  (if (fx= (bytes-length bs) 0)
      '()
      (let* ([delim (if (char? delim) (char->integer delim) delim)]
             [pred? (lambda (b) (fx= b delim))]
             [start (if (and (eq? grammar 'prefix) (fx= (bytes-ref bs 0) delim)) 1 0)]
             [end (if (and (eq? grammar 'suffix) (fx= (bytes-ref bs (- (bytes-length bs) 1)) delim)) (- (bytes-length bs) 1) (bytes-length bs))])
        (let loop ([i start]
                   [res '()])
          (let ([pos (bytestring-index bs pred? i end)])
            (if pos
                (loop (+ pos 1) (cons (subbytes bs i pos) res))
                (reverse (cons (subbytes bs i end) res))))))))


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

  (require (only-in "158.rkt" generator->list)
           (only-in ffi/vector [u8vector bytevector] [make-u8vector make-bytevector] [u8vector-length bytevector-length]
                    [u8vector-ref bytevector-u8-ref])
           (only-in srfi/1 list-tabulate))

  (define-syntax check
    (syntax-rules (=>)
      ((check expr => expected)
       (check-equal? expr expected))))

  (define (print-header . args) (void))

  ;;;; Utility

  (define-syntax constantly
    (syntax-rules ()
      ((_ obj) (lambda _ obj))))

  (define always (constantly #t))
  (define never (constantly #f))

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
  (check (bytestring "lo" #\r #x65 (bytevector #x6d)) => test-bstring)
  (check (bytestring)                         => (bytevector))

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

  (check (hex-string->bytevector (bytevector->hex-string (bytevector ))) => (bytevector ))


  (check (bytevector->base64 test-bstring)             => "bG9yZW0=")
  (check (bytevector->base64 (bytevector #xff #xef #xff))      => "/+//")
  (check (bytevector->base64 (bytevector #xff #xef #xff) "*@") => "@*@@")
  (check (equal? (bytevector->base64 homer) homer64)   => #t)
  (check (bytevector->base64 (bytevector 1))                   => "AQ==")
  (check (bytevector->base64 (bytevector ))                    => "")
  (check (base64->bytevector "bG9yZW0=")               => test-bstring)
  (check (base64->bytevector "/+//")                   => (bytevector #xff #xef #xff))
  (check (base64->bytevector "@*@@" "*@")              => (bytevector #xff #xef #xff))
  (check (equal? (base64->bytevector homer64) homer)   => #t)
  (check (equal? (base64->bytevector homer64-w) homer) => #t)
  (check (base64->bytevector "AQ==")                   => (bytevector 1))
  (check (base64->bytevector "")                       => (bytevector ))
  (check (base64->bytevector "\n\n\n==\t\r\n")         => (bytevector ))
  (check (catch-bytestring-error
          (base64->bytevector "bG9@frob"))             => 'bytestring-error)


  (check (bytestring->list (bytevector )) => '())
  (check (bytestring->list (bytestring 70 82 0 66)) => '(#\F #\R 0 #\B))
  (check (bytestring->list (bytestring "\a\t\t\n" 200)) => '(7 9 9 10 200))
  (check (make-bytestring (bytestring->list test-bstring)) => test-bstring)
  (check (make-bytestring (bytestring->list test-bstring 2))
   => (bytestring "rem"))
  (check (make-bytestring (bytestring->list test-bstring 1 3))
   => (bytestring "or"))

  (let ((bvec (make-bytevector 5)))
    (check (begin
            (make-bytestring! bvec 0 '(#x6c #x6f #x72 #x65 #x6d))
            bvec)
     => test-bstring))
  (let ((bvec (make-bytevector 9 #x20)))
    (check (begin (make-bytestring! bvec 2 `("lo" #\r #x65 ,(bytevector #x6d)))
                  bvec)
     => (bytestring "  lorem  ")))
  (check (catch-bytestring-error (make-bytestring '("λ")))
   => 'bytestring-error)
  (check (catch-bytestring-error (make-bytestring '(#x100)))
   => 'bytestring-error)

  (let ((s (list-tabulate (bytevector-length test-bstring)
                          (lambda (i)
                            (bytevector-u8-ref test-bstring i)))))
    (check (let ((g (make-bytestring-generator "lo" #\r #x65 (bytevector #x6d))))
             (generator->list g))
     => s))
  (check (catch-bytestring-error (make-bytestring-generator "λ" #\m #\u))
   => 'bytestring-error)
  (check (catch-bytestring-error (make-bytestring-generator 89 90 300))
   => 'bytestring-error)
)

(define (check-selection)
  (print-header "Running selection tests...")

  (check (bytestring-pad test-bstring (bytevector-length test-bstring) #x7a)
   => test-bstring)
  (check (bytes->string/utf-8 (bytestring-pad test-bstring 8 #x7a))
   => "zzzlorem")
  (check (equal? (bytestring-pad test-bstring 8 #\z)
                 (bytestring-pad test-bstring 8 (char->integer #\z)))
   => #t)
  (check (bytestring-pad-right test-bstring
                               (bytevector-length test-bstring)
                               #x7a)
   => test-bstring)
  (check (bytes->string/utf-8 (bytestring-pad-right test-bstring 8 #x7a))
   => "loremzzz")
  (check (equal? (bytestring-pad-right test-bstring 8 #\z)
                 (bytestring-pad-right test-bstring 8 (char->integer #\z)))
   => #t)

  (check (bytestring-trim test-bstring always) => (bytevector ))
  (check (bytestring-trim test-bstring never)  => test-bstring)
  (check (bytestring-trim test-bstring (lambda (u8) (< u8 #x70)))
   => (bytevector #x72 #x65 #x6d))
  (check (bytestring-trim-right test-bstring always) => (bytevector ))
  (check (bytestring-trim-right test-bstring never)  => test-bstring)
  (check (bytestring-trim-right test-bstring (lambda (u8) (< u8 #x70)))
   => (bytevector #x6c #x6f #x72))
  (check (bytestring-trim-both test-bstring always) => (bytevector ))
  (check (bytestring-trim-both test-bstring never)  => test-bstring)
  (check (bytestring-trim-both test-bstring (lambda (u8) (< u8 #x70)))
   => (bytevector #x72)))

(define (check-replacement)
  (print-header "Running bytestring-replace tests...")

  (check (bytestring-replace test-bstring (bytestring "mists") 1 5 1 5)
   => (bytestring "lists"))
  (check (bytestring-replace test-bstring (bytestring "faded") 2 5 1 5)
   => (bytestring "loaded"))
  (check (bytestring-replace (make-bytevector 5)
                             test-bstring
                             0
                             (bytevector-length test-bstring))
   => test-bstring)

  (let ((bv1 (bytestring "food")) (bv2 (bytestring "od fo")))
    (check (bytestring-replace bv1 bv2 2 2 0 5) => (bytestring "food food")))
  (let ((bv1 (bytestring "food food")))
    (check (bytestring-replace bv1 (bytevector) 2 7 0 0)
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
   => (list test-bstring (bytevector)))
  (check (values~>list (bytestring-span test-bstring never))
   => (list (bytevector) test-bstring))
  (check (values~>list (bytestring-span test-bstring lt-r?))
   => (list (bytestring "lo") (bytestring "rem")))

  (check (values~>list (bytestring-break test-bstring always))
   => (list (bytevector) test-bstring))
  (check (values~>list (bytestring-break test-bstring never))
   => (list test-bstring (bytevector)))
  (check (values~>list (bytestring-break test-bstring eq-r?))
   => (list (bytestring "lo") (bytestring "rem"))))

(define (check-join-and-split)
  (define test-segments (list (bytevector 1) (bytevector 2) (bytevector 3)))
  (print-header "Running joining and splitting tests...")

  (check (bytestring-join test-segments (bytevector 0))         => (bytevector 1 0 2 0 3))
  (check (bytestring-join test-segments (bytevector 0) 'prefix) => (bytevector 0 1 0 2 0 3))
  (check (bytestring-join test-segments (bytevector 0) 'suffix) => (bytevector 1 0 2 0 3 0))
  (check (bytestring-join '() (bytevector 0))                   => (bytevector ))
  (check (bytestring-join test-segments #\space)        => (bytevector 1 32 2 32 3))
  (check (bytestring-join test-segments 0)              => (bytevector 1 0 2 0 3))
  (check (bytestring-join test-segments "AB")
   => (bytevector 1 65 66 2 65 66 3))
  (check (bytestring-join test-segments (bytevector 7 8))       => (bytevector 1 7 8 2 7 8 3))
  (check (catch-bytestring-error
          (bytestring-join test-segments 300))          => 'bytestring-error)
  (check (catch-bytestring-error
          (bytestring-join test-segments "λ"))          => 'bytestring-error)
  (check (catch-bytestring-error
           (bytestring-join '() (bytevector 0) 'strict-infix))  => 'bytestring-error)
  (check (catch-bytestring-error
           (bytestring-join '() (bytevector 0) 'foofix))        => 'bytestring-error)

  (check (bytestring-split (bytevector 1 0 2 0 3) 0 'infix)    => test-segments)
  (check (bytestring-split (bytevector 0 1 0 2 0 3) 0 'prefix) => test-segments)
  (check (bytestring-split (bytevector 1 0 2 0 3 0) 0 'suffix) => test-segments)
  (check (bytestring-split (bytevector 0 0) 0)                 => (list (bytevector ) (bytevector ) (bytevector )))
  (check (bytestring-split (bytevector ) 0)                    => '())
  (check (catch-bytestring-error
           (bytestring-split (bytevector ) 0 'foofix))         => 'bytestring-error))


  (define (check-all)
    (check-constructor)
    (check-conversion)
    (check-selection)
    (check-replacement)
    (check-comparison)
    (check-searching)
    (check-join-and-split))
  (check-all))
