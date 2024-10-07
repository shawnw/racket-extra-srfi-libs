#lang racket/base

;;; SRFI-140 immutable strings
;;; From scratch implementation

(require racket/contract racket/fixnum (only-in racket/list drop take argmin) (only-in racket/mutability mutable-string? immutable-string?)
         (only-in racket/sequence empty-sequence) racket/unsafe/ops
         syntax/parse/define (for-syntax racket/base syntax/for-body)
         (only-in racket/base [string rkt:string] [list->string rkt:list->string] [substring rkt:substring] [make-string rkt:make-string]
                  [string-upcase rkt:string-upcase] [string-downcase rkt:string-downcase]
                  [string-foldcase rkt:string-foldcase] [string-titlecase rkt:string-titlecase]
                  [read-line rkt:read-line] [read-string rkt:read-string] [number->string rkt:number->string])
         (only-in racket/string string-append* [string-join rkt:string-join])
         (only-in racket/symbol [symbol->immutable-string symbol->string])
         (only-in srfi/1 append-reverse) srfi/74 "141.rkt"
         )
(module+ test (require rackunit))

(provide
 string? string-length string-ref
 string=? string<? string<=? string>? string>=?
 string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
 string-set! string-copy!
 symbol->string
 (rename-out
  [string-append-immutable string-append]
  [immutable-string? istring?]
  )
 (contract-out
  [string-null? (-> string? boolean?)]
  [string-every (->* ((-> char? any/c) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) any/c)]
  [string-any (->* ((-> char? any/c) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) any/c)]

  [string->vector (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector->string (->* ((vectorof char?)) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string->list (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) list?)]
  [list->string (->* ((listof char?)) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [reverse-list->string (-> (listof char?) immutable-string?)]

  [string->utf8 (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) bytes?)]
  [string->utf16 (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) bytes?)]
  [string->utf16be (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) bytes?)]
  [string->utf16le (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) bytes?)]
  [utf8->string (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [utf16->string (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [utf16be->string (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [utf16le->string (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-utf-16-length (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [bytes-utf-16-length (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer? #:big-endian? (or/c boolean? 'check-bom)) exact-nonnegative-integer?)]
  [bytes-utf-16-endianness (->* (bytes?) (exact-nonnegative-integer? exact-nonnegative-integer?) (values boolean? boolean?))]
  
  [string (-> char? ... immutable-string?)]
  [string-tabulate (-> (-> exact-nonnegative-integer? char?) exact-nonnegative-integer? immutable-string?)]
  [string-unfold (->* ((-> any/c any/c) (-> any/c (or/c char? string?)) (-> any/c any/c) any/c) ((or/c char? string?) (-> any/c (or/c char? string?))) immutable-string?)]
  [string-unfold-right (->* ((-> any/c any/c) (-> any/c (or/c char? string?)) (-> any/c any/c) any/c) ((or/c char? string?) (-> any/c (or/c char? string?))) immutable-string?)]
  [substring (-> string? exact-nonnegative-integer? exact-nonnegative-integer? immutable-string?)]

  [string-take (-> string? exact-nonnegative-integer? immutable-string?)]
  [string-drop (-> string? exact-nonnegative-integer? immutable-string?)]
  [string-take-right (-> string? exact-nonnegative-integer? immutable-string?)]
  [string-drop-right (-> string? exact-nonnegative-integer? immutable-string?)]
  [string-pad (->* (string? exact-nonnegative-integer?) (char? exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-pad-right (->* (string? exact-nonnegative-integer?) (char? exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-trim (->* (string?) ((-> char? any/c) exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-trim-right (->* (string?) ((-> char? any/c) exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-trim-both (->* (string?) ((-> char? any/c) exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]

  [string-replace (->* (string? string? exact-nonnegative-integer? exact-nonnegative-integer?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]

  [string-prefix-length
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [string-suffix-length
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [string-prefix?
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) boolean?)]
  [string-suffix?
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) boolean?)]

  [string-index (->* (string? (-> char? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [string-index-right (->* (string? (-> char? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [string-skip (->* (string? (-> char? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [string-skip-right (->* (string? (-> char? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [string-contains
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]
  [string-contains-right
   (->* (string? string?) (exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?) (or/c exact-nonnegative-integer? #f))]

  [string-upcase (-> string? immutable-string?)]
  [string-downcase (-> string? immutable-string?)]
  [string-foldcase (-> string? immutable-string?)]
  [string-titlecase (-> string? immutable-string?)]

  [string-concatenate (-> (listof string?) immutable-string?)]
  [string-concatenate-reverse (->* ((listof string?)) (string? exact-nonnegative-integer?) immutable-string?)]
  [string-join (->* ((listof string?)) (string? (or/c 'infix 'strict-infix 'suffix 'prefix)) immutable-string?)]

  [string-fold (->* ((-> char? any/c any/c) any/c string?) (exact-nonnegative-integer? exact-nonnegative-integer?) any/c)]
  [string-fold-right (->* ((-> char? any/c any/c) any/c string?) (exact-nonnegative-integer? exact-nonnegative-integer?) any/c)]
  [string-map (-> (-> char? (or/c char? string?)) string? string? ... immutable-string?)]
  [string-for-each (-> (-> char? any/c) string? string? ... void?)]
  [string-map-index (->* ((-> exact-nonnegative-integer? (or/c char? string?)) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-for-each-index (->* ((-> exact-nonnegative-integer? any/c) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [string-count (->* (string? (-> char? any/c)) (exact-nonnegative-integer? exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [string-filter (->* ((-> char? any/c) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]
  [string-remove (->* ((-> char? any/c) string?) (exact-nonnegative-integer? exact-nonnegative-integer?) immutable-string?)]

  [string-repeat (-> (or/c string? char?) exact-nonnegative-integer? immutable-string?)]
  [xsubstring (-> (-> string? immutable-string?)
                  (-> string? exact-integer? exact-integer? immutable-string?)
                  (-> string? exact-integer? exact-integer? exact-nonnegative-integer? exact-nonnegative-integer? immutable-string?))]
  [string-split
   (->* (string? string?) ((or/c 'infix 'strict-infix 'suffix 'prefix) (or/c exact-nonnegative-integer? #f) exact-nonnegative-integer? exact-nonnegative-integer?) list?)]

  [make-string (->* () (exact-nonnegative-integer? char?) mutable-string?)]
  [string-copy (->* (string?) (exact-nonnegative-integer? exact-nonnegative-integer?) mutable-string?)]
  [string-fill! (->* (mutable-string? char?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]

  [read-string (->* (exact-nonnegative-integer?) (input-port?) (or/c immutable-string? eof-object?))]
  [read-line (->* (input-port? (or/c 'linefeed 'return 'return-linefeed 'any 'any-one)) (or/c immutable-string? eof-object?))]
  [number->string (->* (number?) ((or/c 2 8 10 16)) immutable-string?)]

  ))


(define-syntax-parse-rule (for/istring (~optional (~seq #:length slen:expr)) clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  #:with rlen #'(~? slen 32)
  (for/fold/derived original ([chars (rkt:make-string rlen)]
                              [len rlen]
                              [n 0]
                              #:result (unsafe-string->immutable-string! (if (= n len) chars (substring chars 0 n))))
    clauses
    pre-body ...
    (let ([ch (let () post-body ...)])
      (cond
        ((char? ch)
         (cond
           ((unsafe-fx< n len)
            (unsafe-string-set! chars n ch)
            (values chars len (unsafe-fx+ n 1)))
           (else
            (let ([new-chars (rkt:make-string (fl->fx (* len 1.5)))])
              (string-copy! new-chars 0 chars)
              (unsafe-string-set! new-chars n ch)
              (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n 1))))))
        ((string? ch)
         (let ([xlen (unsafe-string-length ch)])
           (cond
             ((unsafe-fx< (unsafe-fx+ n xlen) len)
              (string-copy! chars n ch)
              (values chars len (unsafe-fx+ n xlen)))
             (else
              (let ([new-chars (rkt:make-string (unsafe-fx+ (fl->fx (* len 1.5)) xlen))])
                (string-copy! new-chars 0 chars)
                (string-copy! new-chars n ch)
                (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n xlen)))))))
        (else
         (raise-arguments-error 'for/string "body must return a string or character" "value" ch))))))

(define (string-null? s) (= (string-length s) 0))

(define (string-every pred s [start 0] [end (string-length s)])
  (for/and ([ch (in-string s start end)]) (pred ch)))

(define (string-any pred s [start 0] [end (string-length s)])
  (for/or ([ch (in-string s start end)]) (pred ch)))

(define (string->vector s [start 0] [end (string-length s)])
  (for/vector #:length (- end start) ([ch (in-string s start end)]) ch))

(define (vector->string v [start 0] [end (vector-length v)])
  (unsafe-string->immutable-string!
   (build-string (- end start) (lambda (i) (vector-ref v (+ start i))))))

(define (string->list s [start 0] [end (string-length s)])
  (for/list ([ch (in-string s start end)]) ch))

(define list->string
  (case-lambda
    [(lst) (unsafe-string->immutable-string! (rkt:list->string lst))]
    [(lst start)
     (list->string lst start (length lst))]
    ([lst start end]
     (unsafe-string->immutable-string!
      (rkt:list->string (drop (take lst end) start))))))

(define (reverse-list->string lst)
  (define len (length lst))
  (define s (rkt:make-string len))
  (for ([ch (in-list lst)]
        [i (in-inclusive-range (sub1 len) 0 -1)])
    (unsafe-string-set! s i ch))
  (unsafe-string->immutable-string! s))

(define (string->utf8 s [start 0] [end (string-length s)])
  (string->bytes/utf-8 s #f start end))

(define (in-bmp? ch)
  (char<? ch #\U010000))

(define (string-utf-16-length s [start 0] [end (string-length s)])
  (for/sum ([ch (in-string s start end)])
    (if (in-bmp? ch) 2 4)))

(define +bom+ #xFEFF)

(define (bytes-utf-16-endianness bs [start 0] [end (bytes-length bs)])
  (cond
    [(fx< (fx- end start) 2) (values (system-big-endian?) #f)]
    [(and (fx= (bytes-ref bs start) #xFE)
          (fx= (bytes-ref bs (add1 start)) #xFF))
     (values #t #t)]
    [(and (fx= (bytes-ref bs start) #xFF)
          (fx= (bytes-ref bs (add1 start)) #xFE))
     (values #f #t)]
    [else (values (system-big-endian?) #f)]))

(define (bytes-utf-16-length bs [start 0] [end (bytes-length bs)] #:big-endian? [big-endian? 'check-bom])
  (unless (even? (- end start))
    (raise-arguments-error 'bytes-utf-16-length "byte range should be even" "start" start "end" end))
  (when (eq? big-endian? 'check-bom)
    (unless (>= (- start end) 2)
      (raise-arguments-error 'bytes-utf-16-length "Given byte range not long enough to hold a BOM" "start" start "end" end))
    (let-values ([(which-endian found-bom?) (bytes-utf-16-endianness bs start end)])
      (set! big-endian? which-endian)))
  (let loop ([cps 0]
             [i start])
    (cond
      [(>= i end) cps]
      [else
       (define w1 (if big-endian? (blob-u16-ref (endianness big) bs i) (blob-u16-ref (endianness little) bs i)))
       (cond
         [(or (fx<= 0 w1 #xD7FF)
              (fx<= #xE000 w1 #xFFFF))
          (loop (add1 cps) (+ i 2))]
         [(fx<= #xD800 w1 #xDBFF) ; high surrogate
          (define w2
            (if big-endian?
                (blob-u16-ref (endianness big) bs (+ i 2))
                (blob-u16-ref (endianness little) bs (+ i 2))))
          (unless (fx<= #xDC00 w2 #xDFFF)
            (raise-arguments-error 'bytes-utf-16-length "Invalid surrogate pair lower half"
                                   "start index in byte string" (+ i 2)
                                   "upper" (rkt:number->string w1 16)
                                   "lower" (rkt:number->string w2 16)))
          (loop (add1 cps) (+ i 4))]
         [(fx<= #xDC00 w1 #xDFFF)
          (raise-arguments-error 'bytes-utf-16-length "Unpaired surrogate pair lower half"
                                 "start index in byte string" i
                                 "lower" (rkt:number->string w1 16))]
         [else
          (raise-arguments-error 'bytes-utf-16-length "Invalid codepoint in byte string"
                                 "start index in byte string" i
                                 "cp" (rkt:number->string w1 16))])])))

(define (encode-char-as-utf-16! bs i ch big-endian?)
  (define cp (char->integer ch))
  (cond
    [(in-bmp? ch)
     (if big-endian?
         (blob-u16-set! (endianness big) bs i cp)
         (blob-u16-set! (endianness little) bs i cp))
     2]
    [else
     (let* ([u  (fx- cp #x10000)]
            [w1 (fx+ (fxrshift u 10) #xD800)]
            [w2 (fx+ (fxand u #x3FF) #xDC00)])
       (cond
         [big-endian?
          (blob-u16-set! (endianness big) bs i w1)
          (blob-u16-set! (endianness big) bs (+ i 2) w2)]
         [else
          (blob-u16-set! (endianness little) bs i w1)
          (blob-u16-set! (endianness little) bs (+ i 2) w2)]))
     4]))

(define (decode-char-from-utf-16 bs i big-endian?)
  (define w1
    (if big-endian?
        (blob-u16-ref (endianness big) bs i)
        (blob-u16-ref (endianness little) bs i)))
  (cond
    [(or (fx<= 0 w1 #xD7FF)
         (fx<= #xE000 w1 #xFFFF))
     (values (integer->char w1) 2)]
    [(fx<= #xD800 w1 #xDBFF) ; high surrogate
     (define w2
       (if big-endian?
           (blob-u16-ref (endianness big) bs (+ i 2))
           (blob-u16-ref (endianness little) bs (+ i 2))))
     (unless (fx<= #xDC00 w2 #xDFFF)
       (raise-arguments-error 'decode-char-from-utf-16 "Invalid surrogate pair lower half"
                              "start index in byte string" (+ i 2)
                              "upper" (rkt:number->string w1 16)
                              "lower" (rkt:number->string w2 16)))
     (values (integer->char (fx+ (fx* (fx- w1 #xD800) #x400) (fx- w2 #xDC00) #x10000)) 4)]
    [(fx<= #xDC00 w1 #xDFFF)
     (raise-arguments-error 'decode-char-from-utf-16 "Unpaired surrogate pair lower half"
                            "start index in byte string" i
                            "lower" (rkt:number->string w1 16))]

    [else
     (raise-arguments-error 'decode-char-from-utf-16 "Invalid codepoint in byte string"
                            "start index in byte string" i
                            "cp" (rkt:number->string w1 16))]))

(define (convert-to-utf-16 s start end big-endian?)
  (define buffer (make-bytes (string-utf-16-length s start end)))
  (for/fold ([i 0])
            ([ch (in-string s start end)])
    (+ i (encode-char-as-utf-16! buffer i ch big-endian?)))
  buffer)

(define (string->utf16le s [start 0] [end (string-length s)])
  (convert-to-utf-16 s start end #f))
(define (string->utf16be s [start 0] [end (string-length s)])
  (convert-to-utf-16 s start end #t))

(define (string->utf16 s [start 0] [end (string-length s)])
  (define buffer (make-bytes (+ (string-utf-16-length s start end) 2)))
  (define big-endian? (system-big-endian?))
  (if big-endian?
      (blob-u16-set! (endianness big) buffer 0 +bom+)
      (blob-u16-set! (endianness little) buffer 0 +bom+))
  (for/fold ([i 2])
            ([ch (in-string s start end)])
    (+ i (encode-char-as-utf-16! buffer i ch big-endian?)))
  buffer)

(define (utf8->string b [start 0] [end (bytes-length b)])
  (unsafe-string->immutable-string! (bytes->string/utf-8 b #f start end)))

(define (convert-from-utf-16 b start end big-endian?)
  (let loop ([chars '()]
             [i start])
    (if (>= i end)
        (reverse-list->string chars)
        (let-values ([(ch len) (decode-char-from-utf-16 b i big-endian?)])
          (loop (cons ch chars) (+ i len))))))

(define (utf16le->string b [start 0] [end (bytes-length b)])
  (unless (even? (- end start))
    (raise-arguments-error 'utf16le->string "byte range should be even" "start" start "end" end))
  (convert-from-utf-16 b start end #f))
(define (utf16be->string b [start 0] [end (bytes-length b)])
  (unless (even? (- end start))
    (raise-arguments-error 'utf16be->string "byte range should be even" "start" start "end" end))
  (convert-from-utf-16 b start end #t))

(define (utf16->string b [start 0] [end (bytes-length b)])
  (unless (even? (- end start))
    (raise-arguments-error 'utf16->string "byte range should be even" "start" start "end" end))
  (let-values ([(big-endian? bom-present?) (bytes-utf-16-endianness b start end)])
    (convert-from-utf-16 b (+ start (if bom-present? 2 0)) end big-endian?)))

(define (string . chars)
  (cond
    ([null? chars] "")
    [(null? (cdr chars)) (unsafe-string->immutable-string! (rkt:string (car chars)))]
    [else
     (unsafe-string->immutable-string! (rkt:list->string chars))]))

(define (string-tabulate p len)
  (unsafe-string->immutable-string! (build-string len p)))

(define (string-unfold stop? mapper successor seed [base ""] [make-final (lambda (x) "")])
  (unsafe-string->immutable-string!
   (string-append*
    (if (char? base) (string base) base)
    (let loop ([seed seed]
                    [results '()])
           (if (stop? seed)
               (let ([final (make-final seed)])
                 (append-reverse results (list (if (char? final) (rkt:string final) final))))
               (let ([mapped (mapper seed)])
                 (loop (successor seed) (cons (if (char? mapped) (rkt:string mapped) mapped) results))))))))

(define (string-unfold-right stop? mapper successor seed [base ""] [make-final (lambda (x) "")])
  (unsafe-string->immutable-string!
   (string-append*
    (let loop ([seed seed]
               [results (list (if (char? base) (rkt:string base) base))])
      (if (stop? seed)
          (let ([final (make-final seed)])
            (cons (if (char? final) (rkt:string final) final) results))
          (let ([mapped (mapper seed)])
            (loop (successor seed) (cons (if (char? mapped) (rkt:string mapped) mapped) results))))))))

(define (substring s start end)
  (unsafe-string->immutable-string! (rkt:substring s start end)))

(define (string-take s nchars)
  (substring s 0 nchars))

(define (string-drop s nchars)
  (substring s nchars (string-length s)))

(define (string-take-right s nchars)
  (define len (string-length s))
  (substring s (- len nchars) len))

(define (string-drop-right s nchars)
  (define len (string-length s))
  (substring s 0 (- len nchars)))

(define (string-pad s len [pad-char #\space] [start 0] [end (string-length s)])
  (cond
    [(and (immutable-string? s)
          (= len (string-length s))
          (= start 0)
          (= len end))
     s]
    [else
     (define r (make-string len pad-char))
     (do ([i (sub1 end) (sub1 i)]
          [j (sub1 len) (sub1 j)])
       ((or (< j 0) (< i start))
        (unsafe-string->immutable-string! r))
       (string-set! r j (string-ref s i)))]))

(define (string-pad-right s len [pad-char #\space] [start 0] [end (string-length s)])
  (cond
    [(and (immutable-string? s)
          (= len (string-length s))
          (= start 0)
          (= len end))
     s]
    [else
     (define r (make-string len pad-char))
     (do ([i start (add1 i)]
          [j 0 (add1 j)])
       ((or (= j len) (= i end))
        (unsafe-string->immutable-string! r))
       (string-set! r j (string-ref s i)))]))

(define (string-trim s [pred? char-whitespace?] [start 0] [end (string-length s)])
  (let loop ([i start])
    (cond
      [(= i end) ""]
      [(pred? (string-ref s i)) (loop (add1 i))]
      (else (substring s i end)))))

(define (string-trim-right s [pred? char-whitespace?] [start 0] [end (string-length s)])
  (let loop ([i (sub1 end)])
    (cond
      [(< i start) ""]
      [(pred? (string-ref s i)) (loop (sub1 i))]
      [else (substring s start (add1 i))])))

(define (string-trim-both s [pred? char-whitespace?] [start 0] [end (string-length s)])
  (let find-start ([i start])
    (cond
      [(= i end) ""]
      [(pred? (string-ref s i)) (find-start (add1 i))]
      [else
       (let find-end ([j (sub1 end)])
         (cond
           [(< j i) ""]
           [(pred? (string-ref s j)) (find-end (sub1 j))]
           [else (substring s i (add1 j))]))])))

(define (string-replace old new start end [start-new 0] [end-new (string-length new)])
  (define new-len (- end-new start-new))
  (define result (rkt:make-string (+ (- (string-length old) (- end start)) new-len)))
  (string-copy! result 0 old 0 start)
  (string-copy! result start new start-new end-new)
  (string-copy! result (+ start new-len) old end)
  (unsafe-string->immutable-string! result))


(define (in-reverse-string s start end)
  (if (= start end)
      empty-sequence
      (in-string s (sub1 end) (sub1 start) -1)))

(define (string-prefix-length s1 s2 [s1-start 0] [s1-end (string-length s1)] (s2-start 0) [s2-end (string-length s2)])
  (for/fold ([len 0])
            ([ch1 (in-string s1 s1-start s1-end)]
             [ch2 (in-string s2 s2-start s2-end)]
             #:break (not (char=? ch1 ch2)))
    (add1 len)))

(define (string-suffix-length s1 s2 [s1-start 0] [s1-end (string-length s1)] (s2-start 0) [s2-end (string-length s2)])
  (for/fold ([len 0])
            ([ch1 (in-reverse-string s1 s1-start s1-end)]
             [ch2 (in-reverse-string s2 s2-start s2-end)]
             #:break (not (char=? ch1 ch2)))
    (add1 len)))

(define (string-prefix? s1 s2 [s1-start 0] [s1-end (string-length s1)] [s2-start 0] [s2-end (string-length s2)])
  (define s1-len (- s1-end s1-start))
  (define s2-len (- s2-end s2-start))
  (if (> s1-len s2-len)
      #f
      (for/and ([ch1 (in-string s1 s1-start s1-end)]
                [ch2 (in-string s2 s2-start s2-end)])
        (char=? ch1 ch2))))

(define (string-suffix? s1 s2 [s1-start 0] [s1-end (string-length s1)] [s2-start 0] [s2-end (string-length s2)])
  (define s1-len (- s1-end s1-start))
  (define s2-len (- s2-end s2-start))
  (cond
    [(= s1-len 0) #t]
    [(> s1-len s2-len) #f]
    [else
     (for/and ([ch1 (in-reverse-string s1 s1-start s1-end)]
               [ch2 (in-reverse-string s2 s2-start s2-end)])
       (char=? ch1 ch2))]))

(define (string-index s pred? [start 0] [end (string-length s)])
  (for/first ([idx (in-naturals start)]
              [ch (in-string s start end)]
              #:when (pred? ch))
    idx))

(define (string-index-right s pred? [start 0] [end (string-length s)])
  (for/first ([idx (in-inclusive-range (sub1 end) start -1)]
              [ch (in-reverse-string s start end)]
              #:when (pred? ch))
    idx))

(define (string-skip s pred? [start 0] [end (string-length s)])
  (for/first ([idx (in-naturals start)]
              [ch (in-string s start end)]
              #:unless (pred? ch))
    idx))

(define (string-skip-right s pred? [start 0] [end (string-length s)])
  (for/first ([idx (in-inclusive-range (sub1 end) start -1)]
              [ch (in-reverse-string s start end)]
              #:unless (pred? ch))
    idx))

(define (string= s1 s2 i s2-start s2-end)
  (let loop ([i i]
             [j s2-start])
    (cond
      [(= j s2-end) #t]
      [(char=? (string-ref s1 i) (string-ref s2 j))
       (loop (add1 i) (add1 j))]
      [else #f])))

(define (string-contains s1 s2 [s1-start 0] [s1-end (string-length s1)] [s2-start 0] [s2-end (string-length s2)])
  (define s1-len (- s1-end s1-start))
  (define s2-len (- s2-end s2-start))
  (if (= s1-len s2-len 0)
      s1-start
      (let loop ([i s1-start])
        (cond
          [(= i s1-end) #f]
          [(> s2-len (- s1-end i)) #f]
          [(string= s1 s2 i s2-start s2-end) i]
          [else (loop (add1 i))]))))

(define (string-contains-right s1 s2 [s1-start 0] [s1-end (string-length s1)] [s2-start 0] [s2-end (string-length s2)])
  (define s1-len (- s1-end s1-start))
  (define s2-len (- s2-end s2-start))
  (if (= s1-len s2-len 0)
      s1-start
      (let loop ([i (- s1-end s2-len)])
        (cond
          [(< i s1-start) #f]
          [(string= s1 s2 i s2-start s2-end) i]
          [else (loop (sub1 i))]))))

(define (string-upcase s)
  (unsafe-string->immutable-string! (rkt:string-upcase s)))

(define (string-downcase s)
  (unsafe-string->immutable-string! (rkt:string-downcase s)))

(define (string-titlecase s)
  (unsafe-string->immutable-string! (rkt:string-titlecase s)))

(define (string-foldcase s)
  (unsafe-string->immutable-string! (rkt:string-foldcase s)))

(define (string-concatenate strings)
  (unsafe-string->immutable-string! (string-append* strings)))

(define string-concatenate-reverse
  (case-lambda
    [(strings) (string-concatenate (reverse strings))]
    [(strings final-string) (string-concatenate (reverse (cons final-string strings)))]
    [(strings final-string end) (string-concatenate (reverse (cons (substring final-string 0 end) strings)))]))

(define (string-join strings [delim " "] [grammar 'infix])
  (case grammar
    [(infix) (unsafe-string->immutable-string! (rkt:string-join strings delim))]
    [(strict-infix)
     (if (null? strings)
         (raise-argument-error 'string-join "(not/c null?)" strings)
         (unsafe-string->immutable-string! (rkt:string-join strings delim)))]
    [(suffix)
     (let loop ([parts '()]
                [strings strings])
       (if (null? strings)
           (string-concatenate-reverse parts)
           (loop (list* delim (car strings) parts) (cdr strings))))]
    [(prefix)
     (let loop ([parts '()]
                [strings strings])
       (if (null? strings)
           (string-concatenate-reverse parts)
           (loop (list* (car strings) delim parts) (cdr strings))))]))

(define (string-fold kons knil s [start 0] [end (string-length s)])
  (for/fold ([knil knil])
            ([ch (in-string s start end)])
    (kons ch knil)))

(define (string-fold-right kons knil s [start 0] [end (string-length s)])
  (for/fold ([knil knil])
            ([ch (in-reverse-string s start end)])
    (kons ch knil)))

(define (string-map proc s1 . strings)
  (for/istring #:length (string-length (argmin string-length (cons s1 strings)))
    ([chars (in-values-sequence (apply in-parallel (in-string s1) (map in-string strings)))])
    (apply proc chars)))

(define (string-for-each proc s1 . strings)
  (for ([chars (in-values-sequence (apply in-parallel (in-string s1) (map in-string strings)))])
    (apply proc chars)))

(define (string-map-index proc s [start 0] [end (string-length s)])
  (for/istring #:length (- end start)
    ([idx (in-range start end)])
    (proc idx)))

(define (string-for-each-index proc s [start 0] [end (string-length s)])
  (for ([idx (in-range start end)])
    (proc idx)))


;; string-for-each-index


(define (string-count s pred [start 0] [end (string-length s)])
  (for/fold ([count 0])
            ([ch (in-string s start end)]
             #:when (pred ch))
    (add1 count)))

(define (string-filter pred s [start 0] [end (string-length s)])
  (for/istring #:length (- end start)
    ([ch (in-string s start end)]
     #:when (pred ch))
    ch))

(define (string-remove pred s [start 0] [end (string-length s)])
  (for/istring #:length (- end start)
    ([ch (in-string s start end)]
     #:unless (pred ch))
    ch))

(define (string-repeat s-or-c len)
  (if (char? s-or-c)
      (unsafe-string->immutable-string! (rkt:make-string len s-or-c))
      (for/istring #:length (* (string-length s-or-c) len)
        ([n (in-range len)])
        s-or-c)))

;;; Based on the SRFI-13 version
(define (xsubstring s [from 0] [to (string-length s)] [start 0] [end (string-length s)])
  (when (> from to)
    (raise-arguments-error 'xsubstring "from should not be higher than to" "from" from "to" to))
  (when (and (= start end)
             (not (= from to)))
    (raise-arguments-error 'xsubstring "start should not equal end" "start" start "end" end))
  (define slen (- end start))
  (define anslen (- to from))
  (cond
    [(= slen 1) (unsafe-string->immutable-string! (rkt:make-string anslen (string-ref s start)))] ; Fast path for 1-char replication.
    [(= (floor-quotient from slen) (floor-quotient to slen)) ; Selected text falls entirely within one span.
     (substring s (+ start (modulo from slen)) (+ start (modulo to slen)))]
    [else ; Selected text requires multiple spans.
     (let ((ans (make-string anslen)))
       (%multispan-repcopy! ans 0 s from to start end)
       (unsafe-string->immutable-string! ans))]))

;;; This is the core copying loop for XSUBSTRING and STRING-XCOPY!
;;; Internal -- not exported, no careful arg checking.
(define (%multispan-repcopy! target tstart s sfrom sto start end)
  (let* ((slen (- end start))
	 (i0 (+ start (modulo sfrom slen)))
	 (total-chars (- sto sfrom)))

    ;; Copy the partial span @ the beginning
    (string-copy! target tstart s i0 end)

    (let* ((ncopied (- end i0))			; We've copied this many.
	   (nleft (- total-chars ncopied))	; # chars left to copy.
	   (nspans (quotient nleft slen)))	; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ tstart ncopied) (+ i slen))	; Current target index.
	   (nspans nspans (- nspans 1)))	; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (string-copy! target i s start (+ start (- total-chars (- i tstart)))))

	(string-copy! target i s start end))))); Copy a whole span.


(define (string-split s delim [grammar 'infix] [limit #f] [start 0] [end (string-length s)])
  (cond
    [(string-null? s)
     (if (eq? grammar 'strict-infix)
         (raise-arguments-error 'string-split "Can't split empty string with given grammar" "grammar" grammar)
         '())]
    [(string-null? delim)
     (for/list ([ch (in-string s start end)]) (string ch))]
    [else
     (define splitted
       (let loop ([i start]
                  [splits (if limit limit (add1 (string-length s)))]
                  [result '()])
         (cond
           [(= splits 0)
            (cons (substring s i end) result)]
           [(string-contains s delim i end)
            => (lambda (pos)
                 (loop (+ pos (string-length delim))
                       (sub1 splits)
                       (cons (substring s i pos) result)))]
           [else
            (cons (substring s i end) result)])))
     (case grammar
       [(infix strict-infix) (reverse splitted)]
       [(prefix)
        (define reversed (reverse splitted))
        (if (string-null? (car reversed))
            (cdr reversed)
            reversed)]
       [(suffix)
        (if (string-null? (car splitted))
            (reverse (cdr splitted))
            (reverse splitted))])]))

;;; mutable string functions

(define (make-string [len 0] [char #\nul])
  (rkt:make-string len char))

(define (string-copy s [start 0] [end (string-length s)])
  (rkt:substring s start end))

(define (string-fill! s c [start 0] [end (string-length s)])
  (for ([n (in-range start end)])
    (string-set! s n c)))

(define (read-line [in (current-input-port)] [mode 'linefeed])
  (let ([raw (rkt:read-line in mode)])
    (if (string? raw)
        (unsafe-string->immutable-string! raw)
        raw)))

(define (read-string len [in (current-input-port)])
  (let ([raw (rkt:read-string len in)])
    (if (string? raw)
        (unsafe-string->immutable-string! raw)
        raw)))

(define (number->string n [radix 10])
  (unsafe-string->immutable-string! (rkt:number->string n radix)))

(module+ test


  ;;; Modified for Racket by Shawn Wagner
  ;;; Copyright (C) Per Bothner (2017).
  ;;; Copyright (C) William D Clinger (2016).
  ;;;
  ;;; Permission is hereby granted, free of charge, to any person
  ;;; obtaining a copy of this software and associated documentation
  ;;; files (the "Software"), to deal in the Software without
  ;;; restriction, including without limitation the rights to use,
  ;;; copy, modify, merge, publish, distribute, sublicense, and/or
  ;;; sell copies of the Software, and to permit persons to whom the
  ;;; Software is furnished to do so, subject to the following
  ;;; conditions:
  ;;;
  ;;; The above copyright notice and this permission notice shall be
  ;;; included in all copies or substantial portions of the Software.
  ;;;
  ;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  ;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  ;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  ;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  ;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  ;;; OTHER DEALINGS IN THE SOFTWARE.


  #|
;;; Help functions for testing.

(define (as-string . args)
  (string-concatenate (map (lambda (x)
                              (cond ((string? x) x)
                                   ((char? x) (string x))
                                    (else
                                     (error "as-string: illegal argument" x))))
                            args)))
|#


  (define-syntax-rule (test-equal expected test)
    (check-equal? test expected))
  (define-syntax-rule (test-assert test)
    (check-true test))

  ;;; Unicode is a strong motivation for immutable strings, so we ought
  ;;; to use at least some non-ASCII strings for testing.
  ;;; Some systems would blow up if this file were to contain non-ASCII
  ;;; characters, however, so we have to be careful here.
  ;;;
  ;;; FIXME: need more tests with really high code points

  (define ABC
    (list->string (map integer->char
                       '(#x3b1 #x3b2 #x3b3))))
  (define ABCDEF
    (list->string (map integer->char
                       '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066))))
  (define DEFABC
    (list->string (map integer->char
                       '(#x064 #x0c9 #x066 #x0c0 #x062 #x0c7))))
  (define eszett (integer->char #xDF))
  (define fuss (string #\F #\u eszett))
  (define chaos0
    (list->string (map integer->char
                       '(#x39E #x391 #x39F #x3A3))))
  (define chaos1
    (list->string (map integer->char
                       '(#x3BE #x3B1 #x3BF #x3C2))))
  (define chaos2
    (list->string (map integer->char
                       '(#x3BE #x3B1 #x3BF #x3C3))))
  (define beyondBMP
    (list->string (map integer->char
                       '(#x61 #xc0 #x3bf
                              #x1d441 #x1d113 #x1d110 #x7a))))


  ;;; Predicates

  (test-assert (string? (string)))

  (test-assert (not (string? #\a)))

  (test-assert (string-null? (string)))

  (test-assert (not (string-null? ABC)))

  (define (check-istring str)
    (list (immutable-string? str) (string-length str)))

  (test-equal '(#t 0) (check-istring ""))
  (test-equal '(#t 4) (check-istring "abcd"))
  (test-equal '(#t 4) (check-istring (string #\A #\b #\c #\d)))
  (test-equal '(#t 3) (check-istring (substring (make-string 4 #\X) 1 4)))
  (test-equal '(#f 4) (check-istring (make-string 4 #\X)))
  (test-equal '(#f 4) (check-istring (string-copy (make-string 4 #\X))))
  (test-equal '(#f 3) (check-istring (string-copy (make-string 4 #\X) 1 4)))
  (test-equal '(#t 3) (check-istring (vector->string #(#\x #\y #\z))))
  (test-equal '(#t 3) (check-istring (vector->string #(#\x #\y #\z))))
  (test-equal '(#t 3) (check-istring (list->string '(#\x #\y #\z))))
  (test-equal '(#t 3) (check-istring (reverse-list->string '(#\x #\y #\z))))
  (test-equal '(#t 3) (check-istring (utf8->string (string->utf8 "abc"))))
  (test-equal '(#t 3) (check-istring (utf16->string (string->utf16 "abc"))))
  (test-equal '(#t 3) (check-istring (utf16be->string (string->utf16be "abc"))))
  (test-equal '(#t 3) (check-istring (utf16le->string (string->utf16le "abc"))))
  (test-equal '(#t 2) (check-istring (string-take "abcd" 2)))
  (test-equal '(#t 2) (check-istring (string-drop "abcd" 2)))
  (test-equal '(#t 2) (check-istring (string-take-right "abcd" 2)))
  (test-equal '(#t 2) (check-istring (string-drop-right "abcd" 2)))
  (test-equal '(#t 5) (check-istring (string-pad "abcd" 5)))
  (test-equal '(#t 3) (check-istring (string-pad-right "abcd" 3)))
  (test-equal '(#t 2) (check-istring (string-trim "  A ")))
  (test-equal '(#t 3) (check-istring (string-trim-right "  A ")))
  (test-equal '(#t 1) (check-istring (string-trim-both "  A ")))
  (test-equal '(#t 3) (check-istring (string-replace "AB" "X" 1 1)))
  (test-equal '(#t 3) (check-istring (string-upcase (make-string 3 #\X))))
  (test-equal '(#t 3) (check-istring (string-downcase (make-string 3 #\x))))
  (test-equal '(#t 3) (check-istring (string-foldcase (make-string 3 #\x))))
  (test-equal '(#t 3) (check-istring (string-titlecase (make-string 3 #\X))))
  (test-equal '(#t 6) (check-istring (string-append-immutable "abcd" "XY")))
  (test-equal '(#t 6) (check-istring (string-concatenate (list "abcd" "XY"))))
  (test-equal '(#t 6) (check-istring
                       (string-concatenate-reverse  (list "abcd" "XY"))))
  (test-equal '(#t 7) (check-istring (string-join (list "abc" "xyz"))))
  (test-equal '(#t 3) (check-istring (string-map char-upcase "abc")))
  (test-equal '(#t 6) (check-istring (string-repeat "ab" 3)))
  (test-equal '(#t 14) (check-istring (xsubstring "abcdef" -4 10)))
  (test-equal '(#t 3) (check-istring (cadr (string-split "ab cef" " "))))
  ;(test-expect-fail 1)
  (test-equal '(#t 5) (check-istring (symbol->string 'Hello)))

  (test-equal #t (string-every (lambda (c) (if (char? c) c #f))
                               (string)))

  (test-equal #\c (string-every (lambda (c) (if (char? c) c #f))
                                "abc"))

  (test-equal #f (string-every (lambda (c) (if (char>? c #\b) c #f))
                               "abc"))

  (test-equal #\c (string-every (lambda (c) (if (char>? c #\b) c #f))
                                "abc" 2))

  (test-equal #t (string-every (lambda (c) (if (char>? c #\b) c #f))
                               "abc" 1 1))

  (test-equal #f (string-any (lambda (c) (if (char? c) c #f))
                             (string)))

  (test-equal #\a (string-any (lambda (c) (if (char? c) c #f))
                              "abc"))

  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f))
                              "abc"))

  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f))
                              "abc" 2))

  (test-equal #f (string-any (lambda (c) (if (char>? c #\b) c #f))
                             "abc" 0 2))


  (test-equal #t (string-every (lambda (c) (if (char? c) c #f)) ""))

  (test-equal #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))

  (test-equal #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))

  (test-equal #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

  (test-equal #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))

  (test-equal #f (string-any (lambda (c) (if (char? c) c #f)) ""))

  (test-equal #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))

  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))

  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

  (test-equal #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))

  ;;; Constructors

  (test-equal ""
              (string-tabulate (lambda (i)
                                 (integer->char (+ i (char->integer #\a))))
                               0))

  (let ((r (string-tabulate (lambda (i)
                              (integer->char (+ i (char->integer #\a))))
                            3)))
    (test-equal '(#t 3) (check-istring r))
    (test-equal "abc" r))

  (let* ((p (open-input-string "abc"))
         (r (string-unfold eof-object?
                           values
                           (lambda (x) (read-char p))
                           (read-char p))))
    (test-equal '(#t 3) (check-istring r))
    (test-equal "abc" r))

  (test-equal "" (string-unfold null? car cdr '()))

  (test-equal "abc"
              (string-unfold null? car cdr (string->list "abc")))

  (test-equal "def"
              (string-unfold null? car cdr '() "def"))

  (test-equal "defabcG"
              (string-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             "def"
                             (lambda (x) (if (null? x) (string #\G) ""))))

  (test-equal "" (string-unfold-right null? car cdr '()))

  (test-equal "cba"
              (string-unfold-right null? car cdr (string->list "abc")))

  (test-equal "def"
              (string-unfold-right null? car cdr '() "def"))
  (test-equal '(#t 3)
              (check-istring (string-unfold-right null? car cdr '() "def")))

  (test-equal "Gcbadef"
              (string-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   "def"
                                   (lambda (x) (if (null? x) (string #\G) ""))))

  (test-equal "def"
              (string-unfold null? car cdr '() "def"))

  (test-equal "defabcG"
              (string-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             "def"
                             (lambda (x) (if (null? x) "G" ""))))

  (test-equal "dabcG"
              (string-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             #\d
                             (lambda (x) (if (null? x) "G" ""))))

  (test-equal (string-append "%="
                             (make-string 200 #\*)
                             "A B C D E F G H I J K L M "
                             "N O P Q R S T U V W X Y Z "
                             (make-string (* 200 (- (char->integer #\a)
                                                    (char->integer #\Z)
                                                    1))
                                          #\*)
                             "abcdefghijklmnopqrstuvwxyz"
                             " ")
              (string-unfold (lambda (n) (char>? (integer->char n) #\z))
                             (lambda (n)
                               (let ((c (integer->char n)))
                                 (cond ((char<=? #\a c #\z) c)
                                       ((char<=? #\A c #\Z) (string c #\space))
                                       (else (make-string 200 #\*)))))
                             (lambda (n) (+ n 1))
                             (char->integer #\@)
                             "%="
                             (lambda (n) #\space)))

  (test-equal "def"
              (string-unfold-right null? car cdr '() "def"))

  (test-equal "Gcbadef"
              (string-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   "def"
                                   (lambda (x) (if (null? x) "G" ""))))

  (test-equal "Gcbad"
              (string-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   #\d
                                   (lambda (x) (if (null? x) "G" ""))))

  (test-equal (string-append " "
                             (list->string
                              (reverse
                               (string->list "abcdefghijklmnopqrstuvwxyz")))
                             (make-string (* 200 (- (char->integer #\a)
                                                    (char->integer #\Z)
                                                    1))
                                          #\*)
                             "Z Y X W V U T S R Q P O N "
                             "M L K J I H G F E D C B A "
                             (make-string 200 #\*)
                             "%=")
              (string-unfold-right
               (lambda (n) (char>? (integer->char n) #\z))
               (lambda (n)
                 (let ((c (integer->char n)))
                   (cond ((char<=? #\a c #\z) c)
                         ((char<=? #\A c #\Z) (string c #\space))
                         (else (make-string 200 #\*)))))
               (lambda (n) (+ n 1))
               (char->integer #\@)
               "%="
               (lambda (n) #\space)))

  (test-equal " The English alphabet: abcdefghijklmnopqrstuvwxyz "
              (string-unfold-right (lambda (n) (< n (char->integer #\A)))
                                   (lambda (n)
                                     (char-downcase (integer->char n)))
                                   (lambda (n) (- n 1))
                                   (char->integer #\Z)
                                   #\space
                                   (lambda (n) " The English alphabet: ")))

  ;;; Conversion

  (let ((txt (string #\s #\t #\r)))
    (test-assert (and (string? txt) (string=? txt "str"))))

  (test-equal "" (string))

  (test-equal "" (substring (string) 0 0))

  (test-equal "abc" (string #\a #\b #\c))

  (test-equal "" (substring (string #\a #\b #\c) 3 3))

  (test-equal "bc" (substring (string #\a #\b #\c) 1 3))


  ;(test-equal "" (substring "" 0))

  (test-equal "" (substring "" 0 0))

  (test-equal "" (substring "abc" 3 3))

  (test-equal "bc" (substring "abc" 1 3))


  (test-equal '#() (string->vector (string)))

  ;(test-equal '#() (string->vector (string) 0))

  (test-equal '#() (string->vector (string) 0 0))

  (test-equal '#(#\a #\b #\c) (string->vector (string #\a #\b #\c)))

  (test-equal '#() (string->vector (string #\a #\b #\c) 3))

  (test-equal '#(#\b #\c) (string->vector (string #\a #\b #\c) 1 3))


  (test-equal '#() (string->vector ""))

  (test-equal '#() (string->vector "" 0))

  (test-equal '#() (string->vector "" 0 0))

  (test-equal '#(#\a #\b #\c) (string->vector "abc"))

  (test-equal '#() (string->vector "abc" 3))

  (test-equal '#(#\b #\c) (string->vector "abc" 1 3))


  (test-equal '() (string->list (string)))

  (test-equal '() (string->list (string) 0))

  (test-equal '() (string->list (string) 0 0))

  (test-equal '(#\a #\b #\c) (string->list (string #\a #\b #\c)))

  (test-equal '() (string->list (string #\a #\b #\c) 3))

  (test-equal '(#\b #\c) (string->list (string #\a #\b #\c) 1 3))


  (test-equal '() (string->list ""))

  (test-equal '() (string->list "" 0))

  (test-equal '() (string->list "" 0 0))

  (test-equal '(#\a #\b #\c) (string->list "abc"))

  (test-equal '() (string->list "abc" 3))

  (test-equal '(#\b #\c) (string->list "abc" 1 3))


  (test-equal "" "")

  (test-equal "" (substring "" 0 0))
  (test-equal "bc" (substring "abc" 1 3))
  (test-equal "" (substring "abc" 3 3))
  (test-equal "b" (substring "abc" 1 2))
  (test-equal "bc" (substring "abc" 1 3))


  (test-equal "" (vector->string '#()))
  (test-equal "" (vector->string '#() 0))
  (test-equal "" (vector->string '#() 0 0))
  (test-equal "abc" (vector->string '#(#\a #\b #\c)))
  (test-equal "bc" (vector->string '#(#\a #\b #\c) 1))
  (test-equal "" (vector->string '#(#\a #\b #\c) 3))
  (test-equal "b" (vector->string '#(#\a #\b #\c) 1 2))
  (test-equal "bc" (vector->string '#(#\a #\b #\c) 1 3))


  (test-equal "" (list->string '()))

  #| FIXME TODO
(test-equal "" (list->string '() 0))

(test-equal "" (list->string '() 0 0))

(test-equal "abc" (list->string '(#\a #\b #\c)))

(test-equal "bc" (list->string '(#\a #\b #\c) 1))

(test-equal "" (list->string '(#\a #\b #\c) 3))

(test-equal "b" (list->string '(#\a #\b #\c) 1 2))

(test-equal "bc" (list->string '(#\a #\b #\c) 1 3))
|#

  (test-equal "" (reverse-list->string '()))

  (test-equal "cba" (reverse-list->string '(#\a #\b #\c)))


  (test-equal (bytes 97 98 99)
              (string->utf8 "abc"))

  (test-equal (bytes 97 98 99 121 121 121 122 122 122)
              (string->utf8 "xxxabcyyyzzz" 3))

  (test-equal (bytes 97 98 99)
              (string->utf8 "xxxabcyyyzzz" 3 6))


  (test-equal (if (system-big-endian?)
                  (bytes 254 255 0 97 0 98 0 99)
                  (bytes 255 254 97 0 98 0 99 0))
              (string->utf16 "abc"))

  (test-equal (if (system-big-endian?)
                  (bytes 254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
                  (bytes 255  254 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0))
              (string->utf16 "xxxabcyyyzzz" 3))

  (test-equal (if (system-big-endian?)
                  (bytes 254 255 0 97 0 98 0 99)
                  (bytes 255 254 97 0 98 0 99 0))
              (string->utf16 "xxxabcyyyzzz" 3 6))


  (test-equal (bytes 0 97 0 98 0 99)
              (string->utf16be "abc"))

  (test-equal (bytes 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
              (string->utf16be "xxxabcyyyzzz" 3))

  (test-equal (bytes 0 97 0 98 0 99)
              (string->utf16be "xxxabcyyyzzz" 3 6))


  (test-equal (bytes 97 0 98 0 99 0)
              (string->utf16le "abc"))

  (test-equal (bytes 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
              (string->utf16le "xxxabcyyyzzz" 3))

  (test-equal (bytes 97 0 98 0 99 0)
              (string->utf16le "xxxabcyyyzzz" 3 6))


  (test-equal "abc"
              (utf8->string (bytes 97 98 99)))

  (test-equal "abcyyyzzz"
              (utf8->string (bytes 0 1 2 97 98 99 121 121 121 122 122 122) 3))

  (test-equal "abc"
              (utf8->string (bytes 41 42 43 97 98 99 100 101 102) 3 6))


  (test-equal "abc"
              (utf16->string (bytes 254 255 0 97 0 98 0 99)))

  (test-equal "abc"
              (utf16->string (bytes 255 254 97 0 98 0 99 0)))

  (test-equal "abc"
              (utf16->string (string->utf16 "abc") 2))

  (test-equal "bcdef"
              (utf16->string (string->utf16 "abcdef") 4))

  (test-equal "bcd"
              (utf16->string (string->utf16 "abcdef") 4 10))


  (test-equal "abc"
              (utf16be->string (bytes 0 97 0 98 0 99)))

  (test-equal "bc"
              (utf16be->string (string->utf16be "abc") 2))

  (test-equal "bcd"
              (utf16be->string (string->utf16be "abcdef") 2 8))


  (test-equal "abc"
              (utf16le->string (bytes 97 0 98 0 99 0)))

  (test-equal "bc"
              (utf16le->string (string->utf16le "abc") 2))

  (test-equal "bcd"
              (utf16le->string (string->utf16le "abcdef") 2 8))


  (test-equal (bytes 97 195 128 206 191
                     240 157 145 129 240 157 132 147 240 157 132 144 122)
              (string->utf8 beyondBMP))

  (let ((bv (string->utf16 beyondBMP)))
    (test-assert
     (or (equal? bv
                 (bytes 254 255 0 97 0 192 3 191
                        216 53 220 65 216 52 221 19 216 52 221 16 0 122))
         (equal? bv
                 (bytes 255 254 97 0 192 0 191 3
                        53 216 65 220 52 216 19 221 52 216 16 221 122 0)))))

  (test-equal
   (bytes 0 97 0 192 3 191 216 53 220 65 216 52 221 19 216 52 221 16 0 122)
   (string->utf16be beyondBMP))

  (test-equal
   (bytes 97 0 192 0 191 3 53 216 65 220 52 216 19 221 52 216 16 221 122 0)
   (string->utf16le beyondBMP))

  (test-equal
   beyondBMP
   (utf8->string
    (bytes 97 195 128 206 191
           240 157 145 129 240 157 132 147 240 157 132 144 122)))

  (test-equal beyondBMP (utf16->string (string->utf16 beyondBMP)))

  (test-equal beyondBMP
              (utf16->string (string->utf16 beyondBMP) 2))

  (test-equal beyondBMP (utf16be->string (string->utf16be beyondBMP)))

  (test-equal beyondBMP (utf16le->string (string->utf16le beyondBMP)))

  (test-equal (string-append (string (integer->char #xfeff)) "abc")
              (utf16be->string (bytes 254 255 0 97 0 98 0 99)))

  (test-equal (string-append (string (integer->char #xfeff)) "abc")
              (utf16le->string (bytes 255 254 97 0 98 0 99 0)))

  ;;; Selection

  (test-equal 0 (string-length (string)))

  (test-equal 6 (string-length ABCDEF))

  (test-equal 1234 (string-length (make-string 1234 (string-ref ABC 0))))


  (test-equal #\a (string-ref (string #\a #\b #\c) 0))

  (test-equal #\c (string-ref (string #\a #\b #\c) 2))

  (test-equal 0 (string-length (string)))

  (test-equal 6 (string-length ABCDEF))

  (test-equal 1234 (string-length (make-string 1234 (string-ref ABC 0))))



  (test-equal #\a (string-ref (string #\a #\b #\c) 0))

  (test-equal #\c (string-ref (string #\a #\b #\c) 2))

  (test-equal ""
              (substring (string) 0 0))

  (test-equal ""
              (substring "abcdef" 0 0))

  (test-equal "" (substring "abcdef" 4 4))

  (test-equal "" (substring "abcdef" 6 6))

  (test-equal "abcd" (substring "abcdef" 0 4))

  (test-equal "cde" (substring "abcdef" 2 5))

  (test-equal "cdef" (substring "abcdef" 2 6))

  (test-equal "abcdef" (substring "abcdef" 0 6))


  (test-equal "" (substring (string) 0 0))
  (test-equal "" (substring "abcdef" 0 0))

  (test-equal "" (substring "abcdef" 4 4))
  (test-equal "" (substring "abcdef" 6 6))
  (test-equal "abcd" (substring "abcdef" 0 4))
  (test-equal "cde" (substring "abcdef" 2 5))
  (test-equal "cdef" (substring "abcdef" 2 6))
  (test-equal "abcdef" (substring "abcdef" 0 6))


  (test-equal "" (substring "" 0 0))
  (test-equal "" (substring "abcdef" 0 0))
  (test-equal "" (substring "abcdef" 4 4))
  (test-equal "" (substring "abcdef" 6 6))
  (test-equal "abcd" (substring "abcdef" 0 4))
  (test-equal "cde" (substring "abcdef" 2 5))
  (test-equal "cdef" (substring "abcdef" 2 6))
  (test-equal "abcdef" (substring "abcdef" 0 6))


  (test-equal "" (string-copy (string)))

  (let* ((txt "abcdef")
         (copy (string-copy txt)))
    (test-equal "abcdef" copy)
    (test-assert (not (eqv? txt copy))))


  (test-equal "" (string-copy ""))

  (test-equal "abcdef" (string-copy "abcdef"))


  (test-equal "" (string-copy (string) 0))
  (test-equal "abcdef" (string-copy "abcdef" 0))
  (test-equal "ef" (string-copy "abcdef" 4))
  (test-equal "" (string-copy "abcdef" 6))


  (test-equal "" (string-copy "" 0))
  (test-equal "abcdef" (string-copy "abcdef" 0))
  (test-equal "ef" (string-copy "abcdef" 4))
  (test-equal "" (string-copy "abcdef" 6))


  (test-equal "" (string-copy (string) 0 0))
  (test-equal "" (string-copy "abcdef" 0 0))
  (test-equal "" (string-copy "abcdef" 4 4))

  (test-equal "" (string-copy "abcdef" 6 6))
  (test-equal "abcd" (string-copy "abcdef" 0 4))
  (test-equal "cde" (string-copy "abcdef" 2 5))
  (test-equal "cdef" (string-copy "abcdef" 2 6))
  (test-equal "abcdef" (string-copy "abcdef" 0 6))


  (test-equal ""
              (string-copy "" 0 0))

  (test-equal ""
              (string-copy "abcdef" 0 0))

  (test-equal ""
              (string-copy "abcdef" 4 4))

  (test-equal ""
              (string-copy "abcdef" 6 6))

  (test-equal "abcd"
              (string-copy "abcdef" 0 4))

  (test-equal "cde"
              (string-copy "abcdef" 2 5))

  (test-equal "cdef"
              (string-copy "abcdef" 2 6))

  (test-equal "abcdef"
              (string-copy "abcdef" 0 6))


  (test-equal "" (string-take (string) 0))

  (test-equal "" (string-take "abcdef" 0))

  (test-equal "ab" (string-take "abcdef" 2))

  (test-equal "" (string-drop "" 0))

  (test-equal "abcdef" (string-drop "abcdef" 0))

  (test-equal "cdef" (string-drop "abcdef" 2))

  (test-equal "" (string-take-right (string) 0))

  (test-equal "" (string-take-right "abcdef" 0))

  (test-equal "ef" (string-take-right "abcdef" 2))

  (test-equal "" (string-drop-right (string) 0))

  (test-equal "abcdef"
              (string-drop-right "abcdef" 0))

  (test-equal "abcd"
              (string-drop-right "abcdef" 2))


  (test-equal "" (string-take "" 0))

  (test-equal "" (string-take "abcdef" 0))

  (test-equal "ab" (string-take "abcdef" 2))

  (test-equal "" (string-drop "" 0))

  (test-equal "abcdef" (string-drop "abcdef" 0))

  (test-equal "cdef" (string-drop "abcdef" 2))

  (test-equal "" (string-take-right "" 0))

  (test-equal "" (string-take-right "abcdef" 0))

  (test-equal "ef" (string-take-right "abcdef" 2))

  (test-equal "" (string-drop-right "" 0))

  (test-equal "abcdef" (string-drop-right "abcdef" 0))

  (test-equal "abcd" (string-drop-right "abcdef" 2))


  (test-equal ""
              (string-pad "" 0))

  (test-equal "     "
              (string-pad "" 5))

  (test-equal "  325"
              (string-pad "325" 5))

  (test-equal "71325"
              (string-pad "71325" 5))

  (test-equal "71325"
              (string-pad "8871325" 5))

  (test-equal ""
              (string-pad "" 0 #\*))

  (test-equal "*****"
              (string-pad "" 5 #\*))

  (test-equal "**325"
              (string-pad "325" 5 #\*))

  (test-equal "71325"
              (string-pad "71325" 5 #\*))

  (test-equal "71325"
              (string-pad "8871325" 5 #\*))

  (test-equal ""
              (string-pad "" 0 #\* 0))

  (test-equal "*****"
              (string-pad "" 5 #\* 0))

  (test-equal "**325"
              (string-pad "325" 5 #\* 0))

  (test-equal "71325"
              (string-pad "71325" 5 #\* 0))

  (test-equal "71325"
              (string-pad "8871325" 5 #\* 0))

  (test-equal "***25"
              (string-pad "325" 5 #\* 1))

  (test-equal "*1325"
              (string-pad "71325" 5 #\* 1))

  (test-equal "71325"
              (string-pad "8871325" 5 #\* 1))

  (test-equal ""
              (string-pad "" 0 #\* 0 0))

  (test-equal "*****"
              (string-pad "" 5 #\* 0 0))

  (test-equal "**325"
              (string-pad "325" 5 #\* 0 3))

  (test-equal "**713"
              (string-pad "71325" 5 #\* 0 3))

  (test-equal "**887"
              (string-pad "8871325" 5 #\* 0 3))

  (test-equal "***25"
              (string-pad "325" 5 #\* 1 3))

  (test-equal "**132"
              (string-pad "71325" 5 #\* 1 4))

  (test-equal "*8713"
              (string-pad "8871325" 5 #\* 1 5))

  (test-equal ""
              (string-pad-right "" 0))

  (test-equal "     "
              (string-pad-right "" 5))

  (test-equal "325  "
              (string-pad-right "325" 5))

  (test-equal "71325"
              (string-pad-right "71325" 5))

  (test-equal "88713"
              (string-pad-right "8871325" 5))

  (test-equal ""
              (string-pad-right "" 0 #\*))

  (test-equal "*****"
              (string-pad-right "" 5 #\*))

  (test-equal "325**"
              (string-pad-right "325" 5 #\*))

  (test-equal "71325"
              (string-pad-right "71325" 5 #\*))

  (test-equal "88713"
              (string-pad-right "8871325" 5 #\*))

  (test-equal ""
              (string-pad-right "" 0 #\* 0))

  (test-equal "*****"
              (string-pad-right "" 5 #\* 0))

  (test-equal "325**"
              (string-pad-right "325" 5 #\* 0))

  (test-equal "71325"
              (string-pad-right "71325" 5 #\* 0))

  (test-equal "88713"
              (string-pad-right "8871325" 5 #\* 0))

  (test-equal "25***"
              (string-pad-right "325" 5 #\* 1))

  (test-equal "1325*"
              (string-pad-right "71325" 5 #\* 1))

  (test-equal "87132"
              (string-pad-right "8871325" 5 #\* 1))

  (test-equal ""
              (string-pad-right "" 0 #\* 0 0))

  (test-equal "*****"
              (string-pad-right "" 5 #\* 0 0))

  (test-equal "325**"
              (string-pad-right "325" 5 #\* 0 3))

  (test-equal "713**"
              (string-pad-right "71325" 5 #\* 0 3))

  (test-equal "887**"

              (string-pad-right "8871325" 5 #\* 0 3))

  (test-equal "25***"
              (string-pad-right "325" 5 #\* 1 3))

  (test-equal "132**"
              (string-pad-right "71325" 5 #\* 1 4))

  (test-equal "8713*"

              (string-pad-right "8871325" 5 #\* 1 5))


  (test-equal "" (string-pad "" 0))

  (test-equal "     " (string-pad "" 5))

  (test-equal "  325" (string-pad "325" 5))

  (test-equal "71325" (string-pad "71325" 5))

  (test-equal "71325" (string-pad "8871325" 5))

  (test-equal "" (string-pad "" 0 #\*))

  (test-equal "*****" (string-pad "" 5 #\*))

  (test-equal "**325" (string-pad "325" 5 #\*))

  (test-equal "71325" (string-pad "71325" 5 #\*))

  (test-equal "71325" (string-pad "8871325" 5 #\*))

  (test-equal "" (string-pad "" 0 #\* 0))

  (test-equal "*****" (string-pad "" 5 #\* 0))

  (test-equal "**325" (string-pad "325" 5 #\* 0))

  (test-equal "71325" (string-pad "71325" 5 #\* 0))

  (test-equal "71325" (string-pad "8871325" 5 #\* 0))

  (test-equal "***25" (string-pad "325" 5 #\* 1))

  (test-equal "*1325" (string-pad "71325" 5 #\* 1))

  (test-equal "71325" (string-pad "8871325" 5 #\* 1))

  (test-equal "" (string-pad "" 0 #\* 0 0))

  (test-equal "*****" (string-pad "" 5 #\* 0 0))

  (test-equal "**325" (string-pad "325" 5 #\* 0 3))

  (test-equal "**713" (string-pad "71325" 5 #\* 0 3))

  (test-equal "**887" (string-pad "8871325" 5 #\* 0 3))

  (test-equal "***25" (string-pad "325" 5 #\* 1 3))

  (test-equal "**132" (string-pad "71325" 5 #\* 1 4))

  (test-equal "*8713" (string-pad "8871325" 5 #\* 1 5))

  (test-equal "" (string-pad-right "" 0))

  (test-equal "     " (string-pad-right "" 5))

  (test-equal "325  " (string-pad-right "325" 5))

  (test-equal "71325" (string-pad-right "71325" 5))

  (test-equal "88713" (string-pad-right "8871325" 5))

  (test-equal "" (string-pad-right "" 0 #\*))

  (test-equal "*****" (string-pad-right "" 5 #\*))

  (test-equal "325**" (string-pad-right "325" 5 #\*))

  (test-equal "71325" (string-pad-right "71325" 5 #\*))

  (test-equal "88713" (string-pad-right "8871325" 5 #\*))

  (test-equal "" (string-pad-right "" 0 #\* 0))

  (test-equal "*****" (string-pad-right "" 5 #\* 0))

  (test-equal "325**" (string-pad-right "325" 5 #\* 0))

  (test-equal "71325" (string-pad-right "71325" 5 #\* 0))

  (test-equal "88713" (string-pad-right "8871325" 5 #\* 0))

  (test-equal "25***" (string-pad-right "325" 5 #\* 1))

  (test-equal "1325*" (string-pad-right "71325" 5 #\* 1))

  (test-equal "87132" (string-pad-right "8871325" 5 #\* 1))

  (test-equal "" (string-pad-right "" 0 #\* 0 0))

  (test-equal "*****" (string-pad-right "" 5 #\* 0 0))

  (test-equal "325**" (string-pad-right "325" 5 #\* 0 3))

  (test-equal "713**" (string-pad-right "71325" 5 #\* 0 3))

  (test-equal "887**" (string-pad-right "8871325" 5 #\* 0 3))

  (test-equal "25***" (string-pad-right "325" 5 #\* 1 3))

  (test-equal "132**" (string-pad-right "71325" 5 #\* 1 4))

  (test-equal "8713*" (string-pad-right "8871325" 5 #\* 1 5))


  (test-equal ""
              (string-trim ""))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  "))

  (test-equal ""
              (string-trim "" char-whitespace?))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace?))

  (test-equal ""
              (string-trim "  a  b  c  " char?))

  (test-equal ""
              (string-trim "" char-whitespace? 0))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace? 0))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0))

  (test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3))

  (test-equal ""
              (string-trim "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0 11))

  (test-equal "b  c  "
              (string-trim "  a  b  c  "
                           char-whitespace? 3 11))

  (test-equal "" (string-trim "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0 8))

  (test-equal "b  "
              (string-trim "  a  b  c  "
                           char-whitespace? 3 8))

  (test-equal ""
              (string-trim "  a  b  c  " char? 3 8))

  (test-equal ""
              (string-trim-right ""))

  (test-equal "  a  b  c" (string-trim-right "  a  b  c  "))

  (test-equal "" (string-trim-right "" char-whitespace?))

  (test-equal "  a  b  c"
              (string-trim-right "  a  b  c  " char-whitespace?))

  (test-equal ""
              (string-trim-right "  a  b  c  " char?))

  (test-equal ""
              (string-trim-right "" char-whitespace? 0))

  (test-equal "  a  b  c"
              (string-trim-right "  a  b  c  "
                                 char-whitespace? 0))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0))

  (test-equal "  b  c"
              (string-trim-right "  a  b  c  "
                                 char-whitespace? 3))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0 11))

  (test-equal "  b  c"
              (string-trim-right "  a  b  c  "
                                 char-whitespace? 3 11))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0 8))

  (test-equal "  b"
              (string-trim-right "  a  b  c  "
                                 char-whitespace? 3 8))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3 8))

  (test-equal ""
              (string-trim-both ""))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  "))

  (test-equal ""
              (string-trim-both "" char-whitespace?))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  "
                                char-whitespace?))

  (test-equal ""
              (string-trim-both "  a  b  c  " char?))

  (test-equal ""
              (string-trim-both "" char-whitespace? 0))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  "
                                char-whitespace? 0))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0))

  (test-equal "b  c"
              (string-trim-both "  a  b  c  "
                                char-whitespace? 3))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0 11))

  (test-equal "b  c"
              (string-trim-both "  a  b  c  "
                                char-whitespace? 3 11))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0 8))

  (test-equal "b"
              (string-trim-both "  a  b  c  "
                                char-whitespace? 3 8))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3 8))

  (test-equal ""
              (string-trim ""))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  "))

  (test-equal ""
              (string-trim "" char-whitespace?))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace?))

  (test-equal ""
              (string-trim "  a  b  c  " char?))

  (test-equal ""
              (string-trim "" char-whitespace? 0))

  (test-equal "a  b  c  "
              (string-trim "  a  b  c  " char-whitespace? 0))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0))

  (test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3))

  (test-equal ""
              (string-trim "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0 11))

  (test-equal "b  c  "
              (string-trim "  a  b  c  " char-whitespace? 3 11))

  (test-equal ""
              (string-trim "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim "  a  b  c  " char? 0 8))

  (test-equal "b  "
              (string-trim "  a  b  c  " char-whitespace? 3 8))

  (test-equal ""
              (string-trim "  a  b  c  " char? 3 8))


  (test-equal ""
              (string-trim-right ""))

  (test-equal "  a  b  c"
              (string-trim-right "  a  b  c  "))

  (test-equal ""
              (string-trim-right "" char-whitespace?))

  (test-equal "  a  b  c"
              (string-trim-right "  a  b  c  " char-whitespace?))

  (test-equal ""
              (string-trim-right "  a  b  c  " char?))

  (test-equal ""
              (string-trim-right "" char-whitespace? 0))

  (test-equal "  a  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 0))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0))

  (test-equal "  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 3))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0 11))

  (test-equal "  b  c"
              (string-trim-right "  a  b  c  " char-whitespace? 3 11))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 0 8))

  (test-equal "  b"
              (string-trim-right "  a  b  c  " char-whitespace? 3 8))

  (test-equal ""
              (string-trim-right "  a  b  c  " char? 3 8))


  (test-equal ""
              (string-trim-both ""))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  "))

  (test-equal ""
              (string-trim-both "" char-whitespace?))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  " char-whitespace?))

  (test-equal ""
              (string-trim-both "  a  b  c  " char?))

  (test-equal ""
              (string-trim-both "" char-whitespace? 0))

  (test-equal "a  b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 0))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0))

  (test-equal "b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 3))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0 11))

  (test-equal "b  c"
              (string-trim-both "  a  b  c  " char-whitespace? 3 11))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3 11))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 0 8))

  (test-equal "b"
              (string-trim-both "  a  b  c  " char-whitespace? 3 8))

  (test-equal ""
              (string-trim-both "  a  b  c  " char? 3 8))

  ;;; Replacement

  (test-equal "It's lots of fun to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "lots of fun"
                              5 9))

  (test-equal "The miserable perl programmer endured daily ridicule."
              (string-replace "The TCL programmer endured daily ridicule."
                              "another miserable perl drone"
                              4 7 8 22))

  (test-equal "It's really easy to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "really "
                              5 5))

  (test-equal "Runs in O(1) time." ; for strings (using sample implementations)
              (string-replace "Runs in O(n) time." (string #\1) 10 11))

  ;;; Comparison
  ;;;
  ;;; The comparison tests aren't perfectly black-box because the
  ;;; specification of these comparison procedures allows them to
  ;;; use an ordering other than the usual lexicographic ordering.
  ;;; The sample implementations use lexicographic ordering, however,
  ;;; and a test program that discourages implementations from using
  ;;; orderings that differ from the usual on such simple cases is
  ;;; probably doing a public service.

  (test-assert (string=? "Strasse" "Strasse"))

  (test-assert (string=? "Strasse" "Strasse" "Strasse"))

  (test-equal #f (string<? "z" "z"))
  (test-assert (string<? "z" "zz"))
  (test-equal #f (string<? "z" "Z"))
  (test-assert (string<=? "z" "zz"))
  (test-equal #f (string<=? "z" "Z"))
  (test-assert (string<=? "z" "z"))

  (test-equal #f (string<? "z" "z"))
  (test-equal #f (string>? "z" "zz"))
  (test-equal #t (string>? "z" "Z"))
  (test-equal #f (string>=? "z" "zz"))
  (test-equal #t (string>=? "z" "Z"))
  (test-assert (string>=? "z" "z"))


  (let* ((w "a")
         (x "abc")
         (y "def")
         (z (string #\a #\b #\c)))

    (test-equal (string=? x y z)                           #f)
    (test-equal (string=? x x z)                           #t)
    (test-equal (string=? w x y)                           #f)
    (test-equal (string=? y x w)                           #f)

    (test-equal (string<? x y z)                           #f)
    (test-equal (string<? x x z)                           #f)
    (test-equal (string<? w x y)                           #t)
    (test-equal (string<? y x w)                           #f)

    (test-equal (string>? x y z)                           #f)
    (test-equal (string>? x x z)                           #f)
    (test-equal (string>? w x y)                           #f)
    (test-equal (string>? y x w)                           #t)

    (test-equal (string<=? x y z)                          #f)
    (test-equal (string<=? x x z)                          #t)
    (test-equal (string<=? w x y)                          #t)
    (test-equal (string<=? y x w)                          #f)

    (test-equal (string>=? x y z)                          #f)
    (test-equal (string>=? x x z)                          #t)
    (test-equal (string>=? w x y)                          #f)
    (test-equal (string>=? y x w)                          #t)


    (test-equal (string=? x x)                             #t)
    (test-equal (string=? w x)                             #f)
    (test-equal (string=? y x)                             #f)

    (test-equal (string<? x x)                             #f)
    (test-equal (string<? w x)                             #t)
    (test-equal (string<? y x)                             #f)

    (test-equal (string>? x x)                             #f)
    (test-equal (string>? w x)                             #f)
    (test-equal (string>? y x)                             #t)

    (test-equal (string<=? x x)                            #t)
    (test-equal (string<=? w x)                            #t)
    (test-equal (string<=? y x)                            #f)

    (test-equal (string>=? x x)                            #t)
    (test-equal (string>=? w x)                            #f)
    (test-equal (string>=? y x)                            #t)
    )

  (test-equal #t (string-ci<? "a" "Z"))
  (test-equal #t (string-ci<? "A" "z"))
  (test-equal #f (string-ci<? "Z" "a"))
  (test-equal #f (string-ci<? "z" "A"))
  (test-equal #f (string-ci<? "z" "Z"))
  (test-equal #f (string-ci<? "Z" "z"))
  (test-equal #f (string-ci>? "a" "Z"))
  (test-equal #f (string-ci>? "A" "z"))
  (test-equal #t (string-ci>? "Z" "a"))
  (test-equal #t (string-ci>? "z" "A"))
  (test-equal #f (string-ci>? "z" "Z"))
  (test-equal #f (string-ci>? "Z" "z"))
  (test-equal #t (string-ci=? "z" "Z"))
  (test-equal #f (string-ci=? "z" "a"))
  (test-equal #t (string-ci<=? "a" "Z"))
  (test-equal #t (string-ci<=? "A" "z"))
  (test-equal #f (string-ci<=? "Z" "a"))
  (test-equal #f (string-ci<=? "z" "A"))
  (test-equal #t (string-ci<=? "z" "Z"))
  (test-equal #t (string-ci<=? "Z" "z"))
  (test-equal #f (string-ci>=? "a" "Z"))
  (test-equal #f (string-ci>=? "A" "z"))
  (test-equal #t (string-ci>=? "Z" "a"))
  (test-equal #t (string-ci>=? "z" "A"))
  (test-equal #t (string-ci>=? "z" "Z"))
  (test-equal #t (string-ci>=? "Z" "z"))

  ;;; The full-unicode feature doesn't imply full Unicode in strings,
  ;;; so these tests might fail even in a conforming implementation.
  ;;; Implementations that support full Unicode strings often have
  ;;; this feature, however, even though it isn't listed in the R7RS.

  (test-equal #f (string=? ABCDEF DEFABC))
  (test-equal #f (string=? DEFABC ABCDEF))
  (test-equal #t (string=? DEFABC DEFABC))

  (test-equal #f (string<? ABCDEF DEFABC))
  (test-equal #t (string<? DEFABC ABCDEF))
  (test-equal #f (string<? DEFABC DEFABC))

  (test-equal #t (string>? ABCDEF DEFABC))
  (test-equal #f (string>? DEFABC ABCDEF))
  (test-equal #f (string>? DEFABC DEFABC))

  (test-equal #f (string<=? ABCDEF DEFABC))
  (test-equal #t (string<=? DEFABC ABCDEF))
  (test-equal #t (string<=? DEFABC DEFABC))

  (test-equal #t (string>=? ABCDEF DEFABC))
  (test-equal #f (string>=? DEFABC ABCDEF))
  (test-equal #t (string>=? DEFABC DEFABC))

  (test-equal #f (string=? "Fuss" fuss))
  (test-equal #f (string=? "Fuss" "Fuss" fuss))
  (test-equal #f (string=? "Fuss" fuss "Fuss"))
  (test-equal #f (string=? fuss "Fuss" "Fuss"))
  (test-equal #t (string<? "z" (string eszett)))
  (test-equal #f (string<? (string eszett) "z"))
  (test-equal #t (string<=? "z" (string eszett)))
  (test-equal #f (string<=? (string eszett) "z"))
  (test-equal #f (string>? "z" (string eszett)))
  (test-equal #t (string>? (string eszett) "z"))
  (test-equal #f (string>=? "z" (string eszett)))
  (test-equal #t (string>=? (string eszett) "z"))
  (test-assert (string-ci=? fuss "Fuss"))
  (test-assert (string-ci=? fuss "FUSS"))
  (test-assert (string-ci=? chaos0 chaos1 chaos2))


  ;;; Prefixes and suffixes

  (test-equal 0 (string-prefix-length ABC ABCDEF))

  (test-equal 0 (string-prefix-length ABCDEF ABC))

  (test-equal 0 (string-prefix-length ABCDEF DEFABC))

  (test-equal 6 (string-prefix-length DEFABC DEFABC))

  (test-equal 0 (string-prefix-length "" ""))

  (test-equal 0 (string-prefix-length "" "aabbccddee"))

  (test-equal 0 (string-prefix-length "aisle" ""))

  (test-equal 0 (string-prefix-length "" "aabbccddee"))

  (test-equal 1 (string-prefix-length "aisle" "aabbccddee"))

  (test-equal 0 (string-prefix-length "bail" "aabbccddee"))

  (test-equal 4 (string-prefix-length "prefix" "preface"))

  (test-equal 0 (string-prefix-length "" "" 0))

  (test-equal 0 (string-prefix-length "" "aabbccddee" 0))

  (test-equal 0 (string-prefix-length "aisle" "" 0))

  (test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0))

  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 0))

  (test-equal 4 (string-prefix-length "prefix" "preface" 0))

  (test-equal 0 (string-prefix-length "aisle" "" 1))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1))

  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 1))

  (test-equal 0 (string-prefix-length "prefix" "preface" 1))

  (test-equal 0 (string-prefix-length "" "" 0 0))

  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0))

  (test-equal 0 (string-prefix-length "aisle" "" 0 4))

  (test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0 4))

  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 0 1))

  (test-equal 0 (string-prefix-length "aisle" "" 1 4))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4))

  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 1 4))

  (test-equal 0 (string-prefix-length "prefix" "preface" 1 5))

  (test-equal 0 (string-prefix-length "" "" 0 0 0))

  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0))

  (test-equal 0 (string-prefix-length "aisle" "" 0 4 0))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))

  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))

  (test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1))

  (test-equal 0 (string-prefix-length "aisle" "" 1 4 0))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))

  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))

  (test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1))

  (test-equal 0 (string-prefix-length "" "" 0 0 0 0))

  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))

  (test-equal 0 (string-prefix-length "aisle" "" 0 4 0 0))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))

  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))

  (test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1 6))

  (test-equal 0 (string-prefix-length "aisle" "" 1 4 0 0))

  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))

  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))

  (test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1 7))


  (test-equal 0 (string-suffix-length ABC ABCDEF))

  (test-equal 0 (string-suffix-length ABCDEF ABC))

  (test-equal 0 (string-suffix-length ABCDEF DEFABC))

  (test-equal 6 (string-suffix-length DEFABC DEFABC))

  (test-equal 0 (string-suffix-length "" ""))

  (test-equal 0 (string-suffix-length "" "aabbccddee"))

  (test-equal 0 (string-suffix-length "aisle" ""))

  (test-equal 0 (string-suffix-length "" "aabbccddee"))

  (test-equal 1 (string-suffix-length "aisle" "aabbccddee"))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee"))

  (test-equal 3 (string-suffix-length "place" "preface"))

  (test-equal 0 (string-suffix-length "" "" 0))

  (test-equal 0 (string-suffix-length "" "aabbccddee" 0))

  (test-equal 0 (string-suffix-length "aisle" "" 0))

  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0))

  (test-equal 3 (string-suffix-length "place" "preface" 0))

  (test-equal 0 (string-suffix-length "aisle" "" 1))

  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1))

  (test-equal 3 (string-suffix-length "place" "preface" 1))

  (test-equal 0 (string-suffix-length "" "" 0 0))

  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0))

  (test-equal 0 (string-suffix-length "aisle" "" 0 4))

  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1))

  (test-equal 0 (string-suffix-length "aisle" "" 1 4))

  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4))

  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1 5))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4))

  (test-equal 3 (string-suffix-length "place" "preface" 1 5))

  (test-equal 0 (string-suffix-length "" "" 0 0 0))

  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0))

  (test-equal 0 (string-suffix-length "aisle" "" 0 4 0))

  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))

  (test-equal 3 (string-suffix-length "place" "preface" 0 5 1))

  (test-equal 0 (string-suffix-length "aisle" "" 1 4 0))

  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))

  (test-equal 3 (string-suffix-length "place" "preface" 1 5 1))

  (test-equal 0 (string-suffix-length "" "" 0 0 0 0))

  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))

  (test-equal 0 (string-suffix-length "aisle" "" 0 4 0 0))

  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))

  (test-equal 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))

  (test-equal 0 (string-suffix-length "place" "preface" 0 5 1 6))

  (test-equal 2 (string-suffix-length "place" "preface" 0 4 1 6))

  (test-equal 0 (string-suffix-length "aisle" "" 1 4 0 0))

  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))

  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))

  (test-equal 3 (string-suffix-length "place" "preface" 1 5 1 7))


  (test-equal #f (string-prefix? ABC ABCDEF))

  (test-equal #f (string-prefix? ABCDEF ABC))

  (test-equal #f (string-prefix? ABCDEF DEFABC))

  (test-equal #t (string-prefix? DEFABC DEFABC))

  (test-equal #t (string-prefix? "" ""))

  (test-equal #t (string-prefix? "" "abc"))

  (test-equal #t (string-prefix? "a" "abc"))

  (test-equal #f (string-prefix? "c" "abc"))

  (test-equal #t (string-prefix? "ab" "abc"))

  (test-equal #f (string-prefix? "ac" "abc"))

  (test-equal #t (string-prefix? "abc" "abc"))

  (test-equal #f (string-suffix? ABC ABCDEF))

  (test-equal #f (string-suffix? ABCDEF ABC))

  (test-equal #f (string-suffix? ABCDEF DEFABC))

  (test-equal #t (string-suffix? DEFABC DEFABC))

  (test-equal #t (string-suffix? "" ""))

  (test-equal #t (string-suffix? "" "abc"))

  (test-equal #f (string-suffix? "a" "abc"))

  (test-equal #t (string-suffix? "c" "abc"))

  (test-equal #f (string-suffix? "ac" "abc"))

  (test-equal #t (string-suffix? "bc" "abc"))

  (test-equal #t (string-suffix? "abc" "abc"))

  (test-equal #t (string-prefix? "" "" 0))

  (test-equal #t (string-prefix? "" "abc" 0))

  (test-equal #t (string-prefix? "a" "abc" 0))

  (test-equal #f (string-prefix? "c" "abc" 0))

  (test-equal #t (string-prefix? "ab" "abc" 0))

  (test-equal #f (string-prefix? "ac" "abc" 0))

  (test-equal #t (string-prefix? "abc" "abc" 0))

  (test-equal #t (string-suffix? "" "" 0))

  (test-equal #t (string-suffix? "" "abc" 0))

  (test-equal #f (string-suffix? "a" "abc" 0))

  (test-equal #t (string-suffix? "c" "abc" 0))

  (test-equal #f (string-suffix? "ac" "abc" 0))

  (test-equal #t (string-suffix? "bc" "abc" 0))

  (test-equal #t (string-suffix? "abc" "abc" 0))

  (test-equal #t (string-prefix? "ab" "abc" 2))

  (test-equal #t (string-prefix? "ac" "abc" 2))

  (test-equal #f (string-prefix? "abc" "abc" 2))

  (test-equal #t (string-suffix? "ac" "abc" 2))

  (test-equal #t (string-suffix? "bc" "abc" 2))

  (test-equal #t (string-suffix? "abc" "abc" 2))


  (test-equal #t (string-prefix? "" "" 0 0))

  (test-equal #t (string-prefix? "" "abc" 0 0))

  (test-equal #t (string-prefix? "a" "abc" 0 0))

  (test-equal #f (string-prefix? "c" "abc" 0 1))

  (test-equal #t (string-prefix? "ab" "abc" 0 1))

  (test-equal #t (string-prefix? "ab" "abc" 0 2))

  (test-equal #f (string-prefix? "ac" "abc" 0 2))

  (test-equal #t (string-prefix? "abc" "abc" 0 3))

  (test-equal #t (string-suffix? "" "" 0 0))

  (test-equal #t (string-suffix? "" "abc" 0 0))

  (test-equal #f (string-suffix? "a" "abc" 0 1))

  (test-equal #t (string-suffix? "c" "abc" 0 1))

  (test-equal #t (string-suffix? "ac" "abc" 1 2))

  (test-equal #f (string-suffix? "ac" "abc" 0 2))

  (test-equal #t (string-suffix? "bc" "abc" 0 2))

  (test-equal #t (string-suffix? "abc" "abc" 0 3))

  (test-equal #t (string-prefix? "ab" "abc" 2 2))

  (test-equal #t (string-prefix? "ac" "abc" 2 2))

  (test-equal #f (string-prefix? "abc" "abc" 2 3))

  (test-equal #t (string-suffix? "ac" "abc" 2 2))

  (test-equal #t (string-suffix? "bc" "abc" 2 2))

  (test-equal #t (string-suffix? "abc" "abc" 2 3))


  (test-equal #t (string-prefix? "" "" 0 0 0))

  (test-equal #t (string-prefix? "" "abc" 0 0 0))

  (test-equal #t (string-prefix? "a" "abc" 0 0 0))

  (test-equal #f (string-prefix? "c" "abc" 0 1 0))

  (test-equal #t (string-prefix? "ab" "abc" 0 1 0))

  (test-equal #t (string-prefix? "ab" "abc" 0 2 0))

  (test-equal #f (string-prefix? "ac" "abc" 0 2 0))

  (test-equal #t (string-prefix? "abc" "abc" 0 3 0))

  (test-equal #t (string-suffix? "" "" 0 0 0))

  (test-equal #t (string-suffix? "" "abc" 0 0 0))

  (test-equal #f (string-suffix? "a" "abc" 0 1 0))

  (test-equal #t (string-suffix? "c" "abc" 0 1 0))

  (test-equal #t (string-suffix? "ac" "abc" 1 2 0))

  (test-equal #f (string-suffix? "ac" "abc" 0 2 0))

  (test-equal #t (string-suffix? "bc" "abc" 0 2 0))

  (test-equal #t (string-suffix? "abc" "abc" 0 3 0))

  (test-equal #t (string-prefix? "ab" "abc" 2 2 0))

  (test-equal #t (string-prefix? "ac" "abc" 2 2 0))

  (test-equal #f (string-prefix? "abc" "abc" 2 3 0))

  (test-equal #t (string-suffix? "ac" "abc" 2 2 0))

  (test-equal #t (string-suffix? "bc" "abc" 2 2 0))

  (test-equal #t (string-suffix? "abc" "abc" 2 3 0))

  (test-equal #t (string-prefix? "" "abc" 0 0 1))

  (test-equal #t (string-prefix? "a" "abc" 0 0 1))

  (test-equal #t (string-prefix? "c" "abc" 0 1 2))

  (test-equal #f (string-prefix? "ab" "abc" 0 1 2))

  (test-equal #f (string-prefix? "ab" "abc" 0 2 1))

  (test-equal #f (string-prefix? "ac" "abc" 0 2 1))

  (test-equal #f (string-prefix? "abc" "abc" 0 3 1))

  (test-equal #f (string-suffix? "a" "abc" 0 1 2))

  (test-equal #t (string-suffix? "c" "abc" 0 1 1))

  (test-equal #t (string-suffix? "ac" "abc" 1 2 2))

  (test-equal #t (string-suffix? "bc" "abc" 0 2 1))

  (test-equal #f (string-suffix? "bc" "abc" 0 2 2))


  (test-equal #t (string-prefix? "" "" 0 0 0 0))

  (test-equal #t (string-prefix? "" "abc" 0 0 0 3))

  (test-equal #t (string-prefix? "a" "abc" 0 0 0 3))

  (test-equal #f (string-prefix? "c" "abc" 0 1 0 3))

  (test-equal #t (string-prefix? "ab" "abc" 0 1 0 3))

  (test-equal #t (string-prefix? "ab" "abc" 0 2 0 3))

  (test-equal #f (string-prefix? "ac" "abc" 0 2 0 3))

  (test-equal #t (string-prefix? "abc" "abc" 0 3 0 3))

  (test-equal #t (string-suffix? "" "abc" 0 0 0 3))

  (test-equal #f (string-suffix? "a" "abc" 0 1 0 3))

  (test-equal #t (string-suffix? "c" "abc" 0 1 0 3))

  (test-equal #t (string-suffix? "ac" "abc" 1 2 0 3))

  (test-equal #f (string-suffix? "ac" "abc" 0 2 0 3))

  (test-equal #t (string-suffix? "bc" "abc" 0 2 0 3))

  (test-equal #t (string-suffix? "abc" "abc" 0 3 0 3))

  (test-equal #t (string-prefix? "ab" "abc" 2 2 0 3))

  (test-equal #t (string-prefix? "ac" "abc" 2 2 0 3))

  (test-equal #f (string-prefix? "abc" "abc" 2 3 0 3))

  (test-equal #t (string-suffix? "ac" "abc" 2 2 0 3))

  (test-equal #t (string-suffix? "bc" "abc" 2 2 0 3))

  (test-equal #t (string-suffix? "abc" "abc" 2 3 0 3))

  (test-equal #t (string-prefix? "" "abc" 0 0 1 3))

  (test-equal #t (string-prefix? "a" "abc" 0 0 1 3))

  (test-equal #t (string-prefix? "c" "abc" 0 1 2 3))

  (test-equal #f (string-prefix? "ab" "abc" 0 1 2 3))

  (test-equal #f (string-prefix? "ab" "abc" 0 2 1 3))

  (test-equal #f (string-prefix? "ac" "abc" 0 2 1 3))

  (test-equal #f (string-prefix? "abc" "abc" 0 3 1 3))

  (test-equal #f (string-suffix? "a" "abc" 0 1 2 3))

  (test-equal #t (string-suffix? "c" "abc" 0 1 1 3))

  (test-equal #t (string-suffix? "ac" "abc" 1 2 2 3))

  (test-equal #t (string-suffix? "bc" "abc" 0 2 1 3))

  (test-equal #f (string-suffix? "bc" "abc" 0 2 2 3))


  (test-equal #t (string-prefix? "" "abc" 0 0 0 2))

  (test-equal #t (string-prefix? "a" "abc" 0 0 0 2))

  (test-equal #f (string-prefix? "c" "abc" 0 1 0 2))

  (test-equal #t (string-prefix? "ab" "abc" 0 1 0 2))

  (test-equal #f (string-prefix? "abc" "abc" 0 3 0 2))

  (test-equal #t (string-suffix? "" "abc" 0 0 0 2))

  (test-equal #f (string-suffix? "c" "abc" 0 1 0 2))

  (test-equal #f (string-suffix? "ac" "abc" 1 2 0 2))


  ;;; Searching

  (test-equal #f (string-index "" char?))

  (test-equal 0 (string-index "abcdef" char?))

  (test-equal 4 (string-index "abcdef" (lambda (c) (char>? c #\d))))

  (test-equal #f (string-index "abcdef" char-whitespace?))

  (test-equal #f (string-index-right "" char?))

  (test-equal 5 (string-index-right "abcdef" char?))

  (test-equal 5 (string-index-right "abcdef"
                                    (lambda (c) (char>? c #\d))))


  (test-equal #f (string-index-right "abcdef" char-whitespace?))

  (test-equal #f (string-skip "" string?))

  (test-equal 0 (string-skip "abcdef" string?))

  (test-equal 4 (string-skip "abcdef" (lambda (c) (char<=? c #\d))))

  (test-equal #f (string-skip "abcdef" char?))

  (test-equal #f (string-skip-right "" string?))

  (test-equal 5 (string-skip-right "abcdef" string?))

  (test-equal 5 (string-skip-right "abcdef"
                                   (lambda (c) (char<=? c #\d))))

  (test-equal #f (string-skip-right "abcdef" char?))


  (test-equal 2 (string-index "abcdef" char? 2))

  (test-equal 4 (string-index "abcdef" (lambda (c) (char>? c #\d)) 2))

  (test-equal #f (string-index "abcdef" char-whitespace? 2))

  (test-equal 5 (string-index-right "abcdef" char? 2))

  (test-equal 5 (string-index-right "abcdef"
                                    (lambda (c)
                                      (char>? c #\d)) 2))

  (test-equal #f (string-index-right "abcdef" char-whitespace? 2))

  (test-equal 2 (string-skip "abcdef" string? 2))

  (test-equal 4 (string-skip "abcdef"
                             (lambda (c)
                               (char<=? c #\d)) 2))

  (test-equal #f (string-skip "abcdef" char? 2))

  (test-equal 5 (string-skip-right "abcdef" string? 2))

  (test-equal 5 (string-skip-right "abcdef"
                                   (lambda (c)
                                     (char<=? c #\d)) 2))

  (test-equal #f (string-skip-right "abcdef" char? 2))


  (test-equal 2 (string-index "abcdef" char? 2 5))

  (test-equal 4 (string-index "abcdef"
                              (lambda (c) (char>? c #\d)) 2 5))

  (test-equal #f (string-index "abcdef" char-whitespace? 2 5))

  (test-equal 4 (string-index-right "abcdef" char? 2 5))

  (test-equal 4 (string-index-right "abcdef"
                                    (lambda (c)
                                      (char>? c #\d)) 2 5))

  (test-equal #f (string-index-right "abcdef"
                                     char-whitespace? 2 5))


  (test-equal 2 (string-skip "abcdef" string? 2 5))

  (test-equal 4 (string-skip "abcdef"
                             (lambda (c) (char<=? c #\d)) 2 5))

  (test-equal #f (string-skip "abcdef" char? 2 5))

  (test-equal 4 (string-skip-right "abcdef" string? 2 5))

  (test-equal 4 (string-skip-right "abcdef"
                                   (lambda (c)
                                     (char<=? c #\d)) 2 5))

  (test-equal #f (string-skip-right "abcdef" char? 2 5))


  (test-equal 0 (string-contains "" ""))

  (test-equal 0 (string-contains "abcdeffffoo" ""))

  (test-equal 0 (string-contains "abcdeffffoo" "a"))

  (test-equal 5 (string-contains "abcdeffffoo" "ff"))

  (test-equal 4 (string-contains "abcdeffffoo" "eff"))

  (test-equal 8 (string-contains "abcdeffffoo" "foo"))

  (test-equal #f (string-contains "abcdeffffoo" "efffoo"))

  (test-equal 0 (string-contains-right "" ""))

  (test-equal 11 (string-contains-right "abcdeffffoo" ""))

  (test-equal 0 (string-contains-right "abcdeffffoo" "a"))

  (test-equal 7 (string-contains-right "abcdeffffoo" "ff"))

  (test-equal 4 (string-contains-right "abcdeffffoo" "eff"))

  (test-equal 8 (string-contains-right "abcdeffffoo" "foo"))

  (test-equal #f (string-contains-right "abcdeffffoo"
                                        "efffoo"))


  (test-equal 0 (string-contains "" "" 0))

  (test-equal 2 (string-contains "abcdeffffoo" "" 2))

  (test-equal #f (string-contains "abcdeffffoo" "a" 2))

  (test-equal 5 (string-contains "abcdeffffoo" "ff" 2))

  (test-equal 4 (string-contains "abcdeffffoo" "eff" 2))

  (test-equal 8 (string-contains "abcdeffffoo" "foo" 2))

  (test-equal #f (string-contains "abcdeffffoo" "efffoo" 2))

  (test-equal 0 (string-contains-right "" "" 0))

  (test-equal 11 (string-contains-right "abcdeffffoo" "" 2))

  (test-equal #f (string-contains-right "abcdeffffoo" "a" 2))

  (test-equal 7 (string-contains-right "abcdeffffoo" "ff" 2))

  (test-equal 4 (string-contains-right "abcdeffffoo" "eff" 2))

  (test-equal 8 (string-contains-right "abcdeffffoo" "foo" 2))

  (test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2))


  (test-equal 0 (string-contains "" "" 0 0))

  (test-equal 2 (string-contains "abcdeffffoo" "" 2 10))

  (test-equal #f (string-contains "abcdeffffoo" "a" 2 10))

  (test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10))

  (test-equal 4 (string-contains "abcdeffffoo" "eff" 2 10))

  (test-equal #f (string-contains "abcdeffffoo" "foo" 2 10))

  (test-equal #f (string-contains "abcdeffffoo" "efffoo" 2 10))

  (test-equal 0 (string-contains-right "" "" 0 0))

  (test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10))

  (test-equal #f (string-contains-right "abcdeffffoo" "a" 2 10))

  (test-equal 7 (string-contains-right "abcdeffffoo" "ff" 2 10))

  (test-equal 4 (string-contains-right "abcdeffffoo" "eff" 2 10))

  (test-equal #f (string-contains-right "abcdeffffoo" "foo" 2 10))

  (test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2 10))


  (test-equal 0 (string-contains "" "" 0 0 0))

  (test-equal 2 (string-contains "abcdeffffoo" "" 2 10 0))

  (test-equal 2 (string-contains "abcdeffffoo" "a" 2 10 1))

  (test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10 1))

  (test-equal 5 (string-contains "abcdeffffoo" "eff" 2 10 1))

  (test-equal #f (string-contains "abcdeffffoo" "foo" 2 10 1))

  (test-equal #f (string-contains "abcdeffffoo" "efffoo" 2 10 1))

  (test-equal 0 (string-contains-right "" "" 0 0 0))

  (test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10 0))

  (test-equal 10 (string-contains-right "abcdeffffoo" "a" 2 10 1))

  (test-equal 8 (string-contains-right "abcdeffffoo" "ff" 2 10 1))

  (test-equal 7 (string-contains-right "abcdeffffoo" "eff" 2 10 1))

  (test-equal #f (string-contains-right "abcdeffffoo" "foo" 2 10 1))

  (test-equal #f (string-contains-right "abcdeffffoo" "efffoo" 2 10 1))


  (test-equal 0 (string-contains "" "" 0 0 0 0))

  (test-equal 2 (string-contains "abcdeffffoo" "" 2 10 0 0))

  (test-equal 2 (string-contains "abcdeffffoo" "a" 2 10 1 1))

  (test-equal 5 (string-contains "abcdeffffoo" "ff" 2 10 1 2))

  (test-equal 5 (string-contains "abcdeffffoo" "eff" 2 10 1 2))

  (test-equal 9 (string-contains "abcdeffffoo" "foo" 2 10 1 2))

  (test-equal 4 (string-contains "abcdeffffoo" "efffoo" 2 10 0 2))

  (test-equal 0 (string-contains-right "" "" 0 0 0 0))

  (test-equal 10 (string-contains-right "abcdeffffoo" "" 2 10 0 0))

  (test-equal 10 (string-contains-right "abcdeffffoo" "a" 2 10 1 1))

  (test-equal 8  (string-contains-right "abcdeffffoo" "ff" 2 10 1 2))

  (test-equal 8 (string-contains-right "abcdeffffoo" "eff" 2 10 1 2))

  (test-equal 9 (string-contains-right "abcdeffffoo" "foo" 2 10 1 2))

  (test-equal 7 (string-contains-right "abcdeffffoo" "efffoo" 2 10 1 3))


  ;;; Case conversion

  ;;; FIXME: should test some non-ASCII cases here.

  (test-equal "1234STRIKES" (string-upcase "1234Strikes"))

  (test-equal "1234STRIKES" (string-upcase "1234strikes"))

  (test-equal "1234STRIKES" (string-upcase "1234STRIKES"))

  (test-equal "1234strikes" (string-downcase "1234Strikes"))

  (test-equal "1234strikes" (string-downcase "1234strikes"))

  (test-equal "1234strikes" (string-downcase "1234STRIKES"))

  (test-equal "1234strikes" (string-foldcase "1234Strikes"))

  (test-equal "1234strikes" (string-foldcase "1234strikes"))

  (test-equal "1234strikes" (string-foldcase "1234STRIKES"))

  (test-equal "And With Three Strikes You Are Out"
              (string-titlecase
               "and with THREE STRIKES you are oUT"))

  ;;; Concatenation

  (test-equal "" (string-append))

  (test-equal "abcdef"

              (string-append ""
                             "a"
                             "bcd"
                             "" "ef" "" ""))

  (test-equal "" (string-concatenate '()))

  (test-equal "abcdef"
              (string-concatenate '("" "a" "bcd" "" "ef" "" "")))

  ;;; string-concatenate is likely to have special cases for longer strings.

  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (str1 alphabet)
         (str10 (apply string-append (vector->list (make-vector 10 str1))))
         (str100 (apply string-append (vector->list (make-vector 10 str10))))
         (str100-500 (substring str100 100 500))
         (str600-999 (substring str100 600 999))
         (alph1 (string-copy alphabet))
         (alph10 (string-concatenate (vector->list (make-vector 10 alph1))))
         (alph100 (string-concatenate (vector->list (make-vector 10 alph10))))
         (t100-500 (substring alph100 100 500))
         (t600-999 (substring alph100 600 999)))

    (test-equal str10 alph10)

    (test-equal str100 alph100)

    (test-equal str100-500 t100-500)

    (test-equal str600-999 t600-999)

    ;; concatenating a short string with a long string

    (test-equal (string-append str1 str600-999)
                (string-concatenate (list alph1 t600-999)))

    (test-equal (string-append str1 str600-999)
                (string-concatenate (list alph1 (string-copy t600-999))))

    (test-equal (string-append str600-999 str1)
                (string-concatenate (list t600-999 alph1)))

    (test-equal (string-append str600-999 str1)
                (string-concatenate (list (string-copy t600-999) alph1))))


  (test-equal "" (string-concatenate-reverse '()))

  (test-equal "efbcda"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))

  (test-equal "huh?"
              (string-concatenate-reverse '() "huh?"))

  (test-equal "efbcdaxy"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))

  (test-equal "huh"
              (string-concatenate-reverse '() "huh?" 3))

  (test-equal "efbcdax"
              (string-concatenate-reverse
               '("" "a" "bcd" "" "ef" "" "") "x" 1))


  (test-equal "" (string-join '()))

  (test-equal " ab cd  e f "
              (string-join '("" "ab" "cd" "" "e" "f" "")))

  (test-equal ""
              (string-join '() ""))

  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") ""))

  (test-equal ""
              (string-join '() "xyz"))

  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))

  (test-equal ""
              (string-join '() "" 'infix))

  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))

  (test-equal ""
              (string-join '() "xyz" 'infix))

  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))

  (test-equal "foo bar baz" (string-join '("foo" "bar" "baz")))
  (test-equal "foobarbaz" (string-join '("foo" "bar" "baz") ""))
  (test-equal "foo:bar:baz" (string-join '("foo" "bar" "baz") ":"))
  (test-equal "foo:bar:baz:" (string-join '("foo" "bar" "baz") ":" 'suffix))
  (test-equal "" (string-join '() ":"))
  (test-equal "" (string-join '("") ":"))
  (test-equal "" (string-join '()  ":" 'infix))
  (check-exn exn? (lambda () (string-join '()  ":" 'strict-infix)))
  (test-equal "A" (string-join '("A")  ":" 'strict-infix))
  (test-equal "A:B" (string-join '("A" "B")  ":" 'strict-infix))
  (test-equal "" (string-join '()  ":" 'suffix))
  (test-equal ":" (string-join '("") ":" 'suffix))

  (check-exn exn? (lambda () (string-join '() "" 'strict-infix)))

  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))

  (check-exn exn? (lambda () (string-join '() "xyz" 'strict-infix)))

  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))

  (test-equal ""
              (string-join '() "" 'suffix))

  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))

  (test-equal ""
              (string-join '() "xyz" 'suffix))

  (test-equal "xyzabxyzcdxyzxyzexyzfxyzxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))

  (test-equal ""
              (string-join '() "" 'prefix))

  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))

  (test-equal ""
              (string-join '() "xyz" 'prefix))

  (test-equal "xyzxyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))


  ;;; Fold & map & friends

  (test-equal 8
              (string-fold (lambda (c count)
                             (if (char-whitespace? c)
                                 (+ count 1)
                                 count))
                           0
                           " ...a couple of spaces in this one... "))

  (test-equal 7 (string-fold (lambda (c count)
                               (if (char-whitespace? c)
                                   (+ count 1)
                                   count))
                             0
                             " ...a couple of spaces in this one... "
                             1))

  (test-equal 6 (string-fold (lambda (c count)
                               (if (char-whitespace? c)
                                   (+ count 1)
                                   count))
                             0
                             " ...a couple of spaces in this one... "
                             1
                             32))

  (test-equal (string->list "abcdef")
              (string-fold-right cons '() "abcdef"))

  (test-equal (string->list "def")
              (string-fold-right cons '() "abcdef" 3))

  (test-equal (string->list "cde")
              (string-fold-right cons '() "abcdef" 2 5))

  (test-equal "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (string-fold (lambda (c sum)
                                             (+ sum (if (char=? c #\a) 2 1)))
                                           0 s))
                     (ans (make-string ans-len)))
                (string-fold (lambda (c i)
                               (let ((i (if (char=? c #\a)
                                            (begin (string-set! ans i #\a)
                                                   (+ i 1))
                                            i)))
                                 (string-set! ans i c)
                                 (+ i 1)))
                             0 s)
                ans))


  (test-equal "abc" (string-map string "abc"))

  (test-equal "ABC" (string-map char-upcase "abc"))

  (test-equal "Hear-here!"
              (string-map (lambda (c0 c1 c2)
                            (case c0
                              ((#\1) c1)
                              ((#\2) (string c2))
                              ((#\-) (string #\- c1))))
                          "1222-1111-2222"
                          "Hi There!"
                          "Dear John"))

  (test-equal "abc"
              (let ((q (open-output-string)))
                (string-for-each (lambda (c) (write-char c q))
                                 "abc")
                (get-output-string q)))

  (test-equal '("cfi" "beh" "adg")
              (let ((x '()))
                (string-for-each (lambda (c1 c2 c3)
                                   (set! x (cons (string c1 c2 c3) x)))
                                 "abc"
                                 "defxyz"
                                 "ghijklmnopqrstuvwxyz")
                x))

  (test-equal "abc"
              (string-map-index (lambda (i)
                                  (integer->char (+ i (char->integer #\a))))
                                "xyz"))

  (let ((r (string-map-index (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             "xyz***" 3)))
    (test-equal '(#t 3) (check-istring r))
    (test-equal "def" r))

  (test-equal "cde"
              (string-map-index (lambda (i)
                                  (integer->char (+ i (char->integer #\a))))
                                "......" 2 5))

  (test-equal '(101 100 99 98 97)
              (let ((s "abcde")
                    (v '()))
                (string-for-each-index
                 (lambda (i)
                   (set! v (cons (char->integer (string-ref s i)) v)))
                 s)
                v))

  (test-equal '(101 100 99)
              (let ((s "abcde")
                    (v '()))
                (string-for-each-index
                 (lambda (i)
                   (set! v (cons (char->integer (string-ref s i)) v)))
                 s 2)
                v))

  (test-equal '(99 98)
              (let ((s "abcde")
                    (v '()))
                (string-for-each-index
                 (lambda (i)
                   (set! v (cons (char->integer (string-ref s i)) v)))
                 s 1 3)
                v))

  (test-equal 6 (string-count "abcdef" char?))

  (test-equal 4 (string-count "counting  whitespace, again " char-whitespace? 5))

  (test-equal 3 (string-count "abcdefwxyz"
                              (lambda (c) (odd? (char->integer c)))
                              2 8))


  (let ((r (string-filter (lambda (c) (memv c (string->list "aeiou")))
                          "What is number, that man may know it?")))
    (test-equal "aiueaaaoi" r)
    (test-equal '(#t 9) (check-istring r)))

  (let ((r (string-remove (lambda (c) (memv c (string->list "aeiou")))
                          "And woman, that she may know number?")))
    (test-equal "And wmn, tht sh my knw nmbr?" r)
    (test-equal '(#t 28) (check-istring r)))

  (test-equal "iueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             4))

  (test-equal "mn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"
                             6))

  (test-equal "aaao"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             16 32))

  (test-equal "And woman, that sh may know"
              (string-remove (lambda (c) (memv c (string->list "eiu")))
                             "And woman, that she may know number?"
                             0 28))


  #|
(test-equal "" (string-reverse ""))

(test-equal  "fedcba" (string-reverse "abcdef"))

(test-equal "" (string-reverse "" 0))

(test-equal "fedcba" (string-reverse "abcdef" 0))

(test-equal "fedc" (string-reverse "abcdef" 2))

(test-equal "" (string-reverse "" 0 0))

(test-equal "fedcba" (string-reverse "abcdef" 0 6))

(test-equal "edc" (string-reverse "abcdef" 2 5))
|#


  ;;; Replication and splitting

  (test-equal "" (string-repeat #\X 0))
  (test-equal "XXX" (string-repeat #\X 3))
  (test-equal "" (string-repeat "abc" 0))
  (test-equal "abcabcabc" (string-repeat "abc" 3))

  (test-equal "cdefabcdefabcd"
              (xsubstring "abcdef" -4 10))

  (test-equal "bcdefbcdefbcd"
              (xsubstring "abcdef" 90 103 1))

  (test-equal "ecdecdecde"
              (xsubstring "abcdef" -13 -3 2 5))

  (test-equal "cdefab" (xsubstring "abcdef" 2 8))
  (test-equal "efabcd" (xsubstring "abcdef" -2 4))
  (test-equal "abcabca" (xsubstring "abc" 0 7))

  (test-equal '() (string-split "" ""))

  (test-equal '("a" "b" "c") (string-split "abc" ""))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " "))

  (test-equal '("" "there" "ya" "go" "")
              (string-split "***there***ya***go***" "***"))

  (test-equal '() (string-split "" "" 'infix))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'infix))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'infix))

  (test-equal '("" "there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'infix))

  (check-exn exn? (lambda () (string-split "" "" 'strict-infix)))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'strict-infix))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'strict-infix))

  (test-equal '("" "there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'strict-infix))

  (test-equal '()
              (string-split "" "" 'prefix))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'prefix))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'prefix))

  (test-equal '("there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'prefix))

  (test-equal '()
              (string-split "" "" 'suffix))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'suffix))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'suffix))

  (test-equal '("" "there" "ya" "go")
              (string-split "***there***ya***go***" "***" 'suffix))


  (test-equal '() (string-split "" "" 'infix #f))

  (test-equal '("a" "b" "c") (string-split "abc" "" 'infix #f))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'infix #f))

  (test-equal '("" "there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'infix #f))

  (check-exn exn? (lambda () (string-split "" "" 'strict-infix #f)))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'strict-infix #f))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'strict-infix #f))

  (test-equal '("" "there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'strict-infix #f))

  (test-equal '() (string-split "" "" 'prefix #f))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'prefix #f))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'prefix #f))

  (test-equal '("there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'prefix #f))

  (test-equal '()
              (string-split "" "" 'suffix #f))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'suffix #f))

  (test-equal '("too" "" "much" "" "data")
              (string-split "too  much  data" " " 'suffix #f))

  (test-equal '("" "there" "ya" "go")
              (string-split "***there***ya***go***" "***" 'suffix #f))


  (check-exn exn? (lambda () (string-split "" "" 'strict-infix 3)))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'strict-infix 3))

  (test-equal '("too" "" "much" " data")
              (string-split "too  much  data" " " 'strict-infix 3))

  (test-equal '("" "there" "ya" "go***")
              (string-split "***there***ya***go***" "***" 'strict-infix 3))

  (test-equal '()
              (string-split "" "" 'prefix 3))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'prefix 3))

  (test-equal '("too" "" "much" " data")
              (string-split "too  much  data" " " 'prefix 3))

  (test-equal '("there" "ya" "go***")
              (string-split "***there***ya***go***" "***" 'prefix 3))

  (test-equal '()
              (string-split "" "" 'suffix 3))

  (test-equal '("a" "b" "c")
              (string-split "abc" "" 'suffix 3))

  (test-equal '("too" "" "much" " data")
              (string-split "too  much  data" " " 'suffix 3))

  (test-equal '("" "there" "ya" "go***")
              (string-split "***there***ya***go***" "***" 'suffix 3))

  (check-exn exn? (lambda () (string-split "" "" 'strict-infix 3 0)))

  (test-equal '("b" "c")
              (string-split "abc" "" 'strict-infix 3 1))

  (test-equal '("oo" "" "much" " data")
              (string-split "too  much  data" " " 'strict-infix 3 1))

  (test-equal '("**there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'strict-infix 3 1))

  (test-equal '()
              (string-split "" "" 'prefix 3 0))

  (test-equal '("b" "c")
              (string-split "abc" "" 'prefix 3 1))

  (test-equal '("oo" "" "much" " data")
              (string-split "too  much  data" " " 'prefix 3 1))

  (test-equal '("**there" "ya" "go" "")
              (string-split "***there***ya***go***" "***" 'prefix 3 1))

  (test-equal '()
              (string-split "" "" 'suffix 3 0))

  (test-equal '("b" "c")
              (string-split "abc" "" 'suffix 3 1))

  (test-equal '("oo" "" "much" " data")
              (string-split "too  much  data" " " 'suffix 3 1))

  (test-equal '("**there" "ya" "go")
              (string-split "***there***ya***go***" "***" 'suffix 3 1))


  (check-exn exn? (lambda () (string-split "" "" 'strict-infix 3 0 0)))

  (test-equal '("b")
              (string-split "abc" "" 'strict-infix 3 1 2))

  (test-equal '("oo" "" "much" " ")
              (string-split "too  much  data" " " 'strict-infix 3 1 11))

  (test-equal '()
              (string-split "" "" 'prefix 3 0 0))

  (test-equal '("b")
              (string-split "abc" "" 'prefix 3 1 2))

  (test-equal '("oo" "" "much" " ")
              (string-split "too  much  data" " " 'prefix 3 1 11))

  (test-equal '()
              (string-split "" "" 'suffix 3 0 0))

  (test-equal '("b")
              (string-split "abc" "" 'suffix 3 1 2))

  (test-equal '("oo" "" "much" " ")
              (string-split "too  much  data" " " 'suffix 3 1 11))
#|
  (define (translate-space-to-newline str)
    (let ((result (make-string 0)))
      (string-for-each
       (lambda (ch)
         (string-append! result
                         (if (char=? ch #\space) #\newline ch)))
       str)
      result))
  (test-equal "ab\ncd\nx"
              (translate-space-to-newline "ab cd x"))

  ;; begin section with UTF-8 literals
  (let ((str (make-string 3 #\😂)))
    (test-equal 3 (string-length str))
    ;; (test-equal 6 (str:length))
    (string-replace! str 1 2 "abc")
    (test-equal "😂abc😂" str)
    (string-replace! str 5 5 str 3)
    (test-equal "😂abc😂c😂" str)
    (string-replace! str 0 2 "ABC" 1 2)
    (test-equal "Bbc😂c😂" str)
    (test-equal 6 (string-length str))
    (test-equal #\c (string-ref str 2))
    (test-equal #\U1f602 (string-ref str 3))
    (test-equal #\c (string-ref str 4)))
|#
  (test-equal "c😼b😂a" (reverse-list->string '(#\a #\😂 #\b #\😼 #\c)))

  (test-equal "y😂a😼xy" (xsubstring "a😼xy😂" 3 9))
  (test-equal "y😂a😼" (xsubstring "a😼xy😂" -2 2))
  ;; end section with UTF-8 literals

  )