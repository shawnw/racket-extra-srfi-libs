#lang racket/base

;;; SRFI-74 Octet-Addressed Binary Blocks
;;; For Typed Racket, mostly original code, a bit bits lifted from the reference.

(require racket/contract racket/mutability
  (only-in racket/base
           [bytes? blob?]
           [bytes-length blob-length]
           [bytes-ref blob-u8-ref]
           [bytes-set! blob-u8-set!]
           [bytes=? blob=?]
           [bytes-copy blob-copy]
           [bytes->list blob->u8-list]
           [list->bytes u8-list->blob])
  (for-syntax racket/base racket/syntax syntax/parse))
(module+ test (require rackunit))

(provide
 endianness big little native
 blob? blob-length blob=? blob-copy 
 blob-u8-ref blob-u8-set!
 blob->u8-list u8-list->blob
 
 (contract-out
  [make-blob (-> exact-positive-integer? bytes?)]
  [blob-copy! (-> mutable-bytes? exact-nonnegative-integer? bytes? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [blob-s8-ref (-> bytes? exact-nonnegative-integer? integer?)]
  [blob-s8-set! (-> mutable-bytes? exact-nonnegative-integer? integer? void?)]
  [blob-uint-ref (-> exact-positive-integer? endian? bytes? integer? exact-nonnegative-integer?)]
  [blob-sint-ref (-> exact-positive-integer? endian? bytes? exact-nonnegative-integer? exact-integer?)]
  [blob-uint-set! (-> exact-positive-integer? endian? mutable-bytes? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [blob-sint-set! (-> exact-positive-integer? endian? mutable-bytes? exact-nonnegative-integer? integer? void?)]
  [blob->uint-list (-> exact-positive-integer? endian? bytes? list?)]
  [blob->sint-list (-> exact-positive-integer? endian? bytes? list?)]
  [uint-list->blob (-> exact-positive-integer? endian? (listof exact-nonnegative-integer?) bytes?)]
  [sint-list->blob (-> exact-positive-integer? endian? (listof exact-integer?) bytes?)]
  ))
 
(define (make-blob n)
  (make-bytes n 0))

(struct endian (big)
  #:sealed
  #:methods gen:custom-write
  [(define (write-proc obj out style)
     (fprintf out "(endianness ~A)" (if (endian-big obj) 'big 'little)))])
(define *big-endian* (endian #t))
(define *little-endian* (endian #f))
(define *native-endian* (if (system-big-endian?) *big-endian* *little-endian*))

(define-syntax big
  (lambda (stx) (raise-syntax-error #f "Use outside of endianness directive" stx)))
(define-syntax little
  (lambda (stx) (raise-syntax-error #f "Use outside of endianness directive" stx)))
(define-syntax native
  (lambda (stx) (raise-syntax-error #f "Use outside of endianness directive" stx)))

(define-syntax (endianness stx)
  (syntax-parse stx
    #:literals (big little native)
    ((_ big) #'*big-endian*)
    ((_ little) #'*little-endian*)
    ((_ native) #'*native-endian*)))

(define (blob-s8-ref blob k)
  (integer-bytes->integer blob #t #t k (+ k 1)))

(define (blob-s8-set! blob k num)
  (void (integer->integer-bytes num 1 #t #t blob k)))

;;; Macro to auto-generate all the fixed-width ref and set! functions.
(begin-for-syntax
  (define (make-getter-names stx nbits sign)
    (list
     (format-id stx "blob-~A~A-ref" sign nbits)
     (format-id stx "blob-~A~A-native-ref" sign nbits)))
  (define (make-setter-names stx nbits sign)
    (list
     (format-id stx "blob-~A~A-set!" sign nbits)
     (format-id stx "blob-~A~A-native-set!" sign nbits)))

  (define (make-getters stx nbytes signed?)
    (let ([names (make-getter-names stx (* nbytes 8) (if signed? #\s #\u))])
      (list
       #`(provide #,@names)
       #`(define-values (#,@names)
           (values
            (lambda (endianness blob k)
              (integer-bytes->integer blob #,signed? (eq? endianness *big-endian*) k (+ k #,nbytes)))
            (lambda (blob k) 
              (integer-bytes->integer blob #,signed? #,(system-big-endian?) k (+ k #,nbytes))))))))

  (define (make-setters stx nbytes signed?)
    (let ([names (make-setter-names stx (* nbytes 8) (if signed? #\s #\u))])
      (list
       #`(provide #,@names)
       #`(define-values (#,@names)
           (values
            (lambda (endianness blob k num)
              (void (integer->integer-bytes num #,nbytes #,signed? (eq? endianness *big-endian*) blob k)))
            (lambda (blob  k num)
              (void (integer->integer-bytes num #,nbytes #,signed? #,(system-big-endian?) blob k)))))))))

(define-syntax (define-int-ops stx)
  (syntax-case stx ()
      [(_ nbytes)
       (let ([bytelen (syntax-e #'nbytes)])
         #`(begin
             #,@(make-getters stx bytelen #t)
             #,@(make-getters stx bytelen #f)
             #,@(make-setters stx bytelen #t)
             #,@(make-setters stx bytelen #f)))]))

(define-int-ops 2)
(define-int-ops 4)
(define-int-ops 8)

;;; Arbitrary-width versions taken from reference implementation

;(: index-iterate : Integer Integer Boolean Integer (Integer Integer -> Integer) -> Integer)
(define (index-iterate start count low-first?
		       unit proc)
  (if low-first?
      (let loop ((index 0)
		 (acc unit))
	(if (>= index count)
	    acc
	    (loop (+ index 1)
		  (proc (+ start index) acc))))
      (let loop ((index (- (+ start count) 1))
		 (acc unit))
	(if (< index start)
	    acc
	    (loop (- index 1)
		  (proc index acc))))))

(define (blob-uint-ref size endness blob index)
  (case size
    [(1 2 4 8)
     (integer-bytes->integer blob #f (eq? (endianness big) endness) index (+ index size))]
    [else
     (let ([num (index-iterate index size
                               (eq? (endianness big) endness)
                               0
                               (lambda (index acc) 
                                 (+ (bytes-ref blob index) (arithmetic-shift acc 8))))])
       (if (>= num 0)
           num
           (error "Invalid negative result")))]))

(define (blob-sint-ref size endness blob index)
  (case size
    [(1 2 4 8)
     (integer-bytes->integer blob #t (eq? (endianness big) endness) index (+ index size))]
    [else
     (let ((high-byte (bytes-ref blob
                                 (if (eq? endness (endianness big))
                                     index
                                     (- (+ index size) 1)))))

       (if (> high-byte 127)
           (- (+ 1
                 (index-iterate index size
                                (eq? (endianness big) endness)
                                0
                                (lambda (index  acc) 
                                  (+ (- 255 (bytes-ref blob index))
                                     (arithmetic-shift acc 8))))))
           (index-iterate index size
                          (eq? (endianness big) endness)
                          0
                          (lambda (Integer acc)
                            (+ (bytes-ref blob index) (arithmetic-shift acc 8))))))]))

(define (blob-uint-set! size endness blob index val)
  (void
   (case size
     [(1 2 4 8)
      (integer->integer-bytes val size #f (eq? (endianness big) endness) blob index)]
     [else
      (index-iterate index size (eq? (endianness little) endness)
                     val
                     (lambda (index acc) 
                       (let-values ([(quo rem) (quotient/remainder acc 256)])
                         (bytes-set! blob index rem)
                         quo)))])))

(define (blob-sint-set! size endness blob index val)
  (void
   (case size
     [(1 2 4 8)
      (integer->integer-bytes val size #t (eq? (endianness big) endness) blob index)]
     [else
      (if (negative? val)
          (index-iterate index size (eq? (endianness little) endness)
                         (- -1 val)
                         (lambda (index acc) 
                           (let-values ([(quo rem) (quotient/remainder acc 256)])
                             (bytes-set! blob index (- 255 rem))
                             quo)))
          (index-iterate index size (eq? (endianness little) endness)
                         val
                         (lambda (index acc)
                           (let-values ([(quo rem) (quotient/remainder acc 256)])
                             (bytes-set! blob index rem)
                             quo))))])))

;; Back to original code

(define (blob-copy! source source-start target target-start n)
  (bytes-copy! target target-start source source-start (+ source-start n)))

(define (blob->uint-list size endianness blob)
  (unless (= (remainder (bytes-length blob) size) 0)
    (raise-arguments-error 'blob->uint-list "bytestring length not evenly divisible by integer size"
                           "bytestring length" (bytes-length blob)
                           "size" size))
  (case size
    ((1 2 4 8)
     (for/list ([n (in-range 0 (bytes-length blob) size)])
       (integer-bytes->integer blob #f (eq? endianness *big-endian*) n (+ n size))))
    (else
     (for/list ([n (in-range 0 (bytes-length blob) size)])
       (blob-uint-ref size endianness blob n)))))

(define (blob->sint-list size endianness blob)
  (unless (= (remainder (bytes-length blob) size) 0)
    (raise-arguments-error 'blob->sint-list "bytestring length not evenly divisible by integer size"
                           "bytestring length" (bytes-length blob)
                           "size" size))
  (case size
    ((1 2 4 8)
     (for/list ([n (in-range 0 (bytes-length blob) size)])
       (integer-bytes->integer blob #t (eq? endianness *big-endian*) n (+ n size))))
    (else
     (for/list ([n (in-range 0 (bytes-length blob) size)])
       (blob-sint-ref size endianness blob n)))))

(define (uint-list->blob size endianness lst)
  (let ([bytes (make-bytes (* (length lst) size))])
    (case size
      ((1 2 4 8)
       (for ([num (in-list lst)]
             [k (in-range 0 (bytes-length bytes) size)])
         (integer->integer-bytes num size #f (eq? endianness *big-endian*) bytes k)))
      (else
       (for ([num (in-list lst)]
             [k (in-range 0 (bytes-length bytes) size)])
         (blob-uint-set! size endianness bytes k num))))
    bytes))

(define (sint-list->blob size endianness lst)
  (let ([bytes (make-bytes (* (length lst) size))])
    (case size
      ((1 2 4 8)
       (for ([num (in-list lst)]
             [k (in-range 0 (bytes-length bytes) size)])
         (integer->integer-bytes num size #t (eq? endianness *big-endian*) bytes k)))
      (else
       (for ([num (in-list lst)]
             [k (in-range 0 (bytes-length bytes) size)])
         (blob-sint-set! size endianness bytes k num))))
    bytes))

(module+ test
  ; Examples for octet-addressed binary objects

; Copyright (C) Michael Sperber (2005). All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(define-syntax check
  (syntax-rules (=>)
    ((check ec => desired-result)
     (check-equal? ec desired-result))))

  (define b1 (make-blob 16))

  (check (blob-length b1) => 16)

  (blob-u8-set! b1 0 223)
  (blob-s8-set! b1 1 123)
  (blob-s8-set! b1 2 -123)
  (blob-u8-set! b1 3 15)

  (check (list (blob-u8-ref b1 0)
               (blob-s8-ref b1 1)
               (blob-u8-ref b1 1)
               (blob-s8-ref b1 2)
               (blob-u8-ref b1 2)
               (blob-u8-ref b1 3))
         => '(223 123 123 -123 133 15))

  (blob-uint-set! 16 (endianness little)
                  b1 0 (- (expt 2 128) 3))

  (check (blob-uint-ref 16 (endianness little) b1 0)
         =>  (- (expt 2 128) 3))

  (check (blob-sint-ref 16 (endianness little) b1 0)
         =>  -3)

  (check (blob->u8-list b1)
         => '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))

  (blob-uint-set! 16 (endianness big)
                  b1 0 (- (expt 2 128) 3))

  (check (blob-uint-ref 16 (endianness big) b1 0)
         =>  (- (expt 2 128) 3))

  (check (blob-sint-ref 16 (endianness big) b1 0)
         =>  -3)

  (check (blob->u8-list b1)
         => '(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253))

  (check (blob-u16-ref (endianness little) b1 14)
         => 65023)
  (check (blob-s16-ref (endianness little) b1 14)
         => -513)
  (check (blob-u16-ref (endianness big) b1 14)
         => 65533)
  (check (blob-s16-ref (endianness big) b1 14)
         => -3)

  (blob-u16-set! (endianness little) b1 0 12345)

  (blob-u16-native-set! b1 0 12345)

  (check (blob-u16-native-ref b1 0)
         => 12345)

  (check (blob-u32-ref (endianness little) b1 12)
         => 4261412863)
  (check (blob-s32-ref (endianness little) b1 12)
         => -33554433)
  (check (blob-u32-ref (endianness big) b1 12)
         => 4294967293)
  (check (blob-s32-ref (endianness big) b1 12)
         => -3)

  (blob-u32-set! (endianness little) b1 0 12345)

  (blob-u32-native-set! b1 0 12345)

  (check (blob-u32-native-ref b1 0)
         => 12345)

  (check (blob-u64-ref (endianness little) b1 8)
         => 18302628885633695743)
  (check (blob-s64-ref (endianness little) b1 8)
         => -144115188075855873)
  (check (blob-u64-ref (endianness big) b1 8)
         => 18446744073709551613)
  (check (blob-s64-ref (endianness big) b1 8)
         => -3)

  (blob-u64-set! (endianness little) b1 0 12345)

  (blob-u64-native-set! b1 0 12345)

  (check (blob-u64-native-ref b1 0)
         => 12345)

  (define b2 (u8-list->blob '(1 2 3 4 5 6 7 8)))
  (define b3 (blob-copy b2))

  (check (blob=? b2 b3) => #t)
  (check (blob=? b1 b2) => #f)

  (blob-copy! b3 0 b3 4 4)

  (check (blob->u8-list b3) => '(1 2 3 4 1 2 3 4))

  (blob-copy! b3 0 b3 2 6)

  (check (blob->u8-list b3) => '(1 2 1 2 3 4 1 2))

  (blob-copy! b3 2 b3 0 6)

  (check (blob->u8-list b3) => '(1 2 3 4 1 2 1 2))

  (check (blob->uint-list 1 (endianness little) b3)
         => '(1 2 3 4 1 2 1 2))

  (check (blob->uint-list 2 (endianness little) b3)
         => '(513 1027 513 513))

  (define b4 (u8-list->blob '(0 0 0 0 0 0 48 57 255 255 255 255 255 255 255 253)))

  (check (blob->sint-list 2 (endianness little) b4)
         => '(0 0 0 14640 -1 -1 -1 -513)))
