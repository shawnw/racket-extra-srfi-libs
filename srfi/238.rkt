#lang racket/base

;;; SRFI-238 Codesets

(require racket/contract racket/list racket/match (rename-in ffi/unsafe [-> -->]))

(provide
 (contract-out
  [codeset? predicate/c]
  [codeset-obj? predicate/c]
  [codeset-symbols (-> codeset? (listof symbol?))]
  [codeset-symbol (-> codeset? (or/c symbol? exact-integer?) (or/c symbol? #f))]
  [codeset-number (-> codeset? (or/c symbol? exact-integer?) (or/c exact-integer? #f))]
  [codeset-message (-> codeset? (or/c symbol? exact-integer?) (or/c string? #f))]
  [make-codeset (-> (listof (list/c (or/c symbol? #f) (or/c exact-integer? #f) (or/c string? #f))) codeset-obj?)]
  [register-codeset! (-> symbol? (or/c codeset-obj? (listof (list/c (or/c symbol? #f) (or/c exact-integer? #f) (or/c string? #f)))) void?)]
  ))

(define *known-codesets* (make-hasheq))

(define (codeset? obj)
  (or (and (symbol? obj) (hash-ref *known-codesets* obj #f) #t)
      (codeset-obj? obj)))

(struct codeset-obj (by-name by-number) #:sealed)

(define (make-codeset data)
  (let-values ([(name-table number-table)
                (for/fold ([name-table (hasheq)]
                           [number-table (hasheqv)])
                          ([elem (in-list data)])
                  (match elem
                    [(list (? symbol? name) #f message)
                     (values
                      (hash-set name-table name (cons #f (if message (string->immutable-string message) #f)))
                      number-table)]
                    [(list #f (? exact-integer? number) message)
                     (values
                      name-table
                      (hash-set number-table number (cons #f (if message (string->immutable-string message) #f))))]
                    [(list (? symbol? name) (? exact-integer? number) message)
                     (let ([message (if message (string->immutable-string message) #f)])
                       (values
                        (hash-set name-table name (cons number message))
                        (hash-set number-table number (cons name message))))]
                    [_
                     (values name-table number-table)]))])
    (codeset-obj name-table number-table)))

(define (register-codeset! name codeset)
  (if (codeset-obj? codeset)
      (hash-set! *known-codesets* name codeset)
      (hash-set! *known-codesets* name (make-codeset codeset))))

(define (lookup-codeset fun codeset)
  (if (codeset-obj? codeset)
      codeset
      (hash-ref *known-codesets* codeset (lambda () (raise-argument-error fun "Unknown codeset name" codeset)))))

(define (codeset-symbols codeset)
  (hash-keys (codeset-obj-by-name (lookup-codeset 'codeset-symbols codeset))))

(define *no-such-entry* (cons #f #f))

(define (codeset-symbol codeset code)
  (let ([codeset (lookup-codeset 'codeset-symbol codeset)])
    (cond
      ((integer? code)
       (car (hash-ref (codeset-obj-by-number codeset) code *no-such-entry*)))
      ((symbol? code)
       code)
      (else
       (raise-argument-error 'codeset-symbol "(or/c symbol? exact-integer?)" code)))))

(define (codeset-number codeset code)
  (let ([codeset (lookup-codeset 'codeset-number codeset)])
    (cond
      ((symbol? code)
       (car (hash-ref (codeset-obj-by-name codeset) code *no-such-entry*)))
      ((integer? code)
       code)
      (else
       (raise-argument-error 'codeset-number "(or/c symbol? exact-integer?)" code)))))

(define (codeset-message codeset code)
  (let ([codeset (lookup-codeset 'codeset-message codeset)])
    (cond
      ((symbol? code)
       (cdr (hash-ref (codeset-obj-by-name codeset) code *no-such-entry*)))
      ((integer? code)
       (cdr (hash-ref (codeset-obj-by-number codeset) code *no-such-entry*)))
      (else
       (raise-argument-error 'codeset-message "(or/c symbol? exact-integer?)" code)))))

(define libc-ffi (ffi-lib #f))
(define %strerror (get-ffi-obj "strerror" libc-ffi (_fun _int --> _string/locale)))

(register-codeset!
 'errno
 (filter-map
  (lambda (name)
    (cond
      ((lookup-errno name) => (lambda (code) (list name code (%strerror code))))
      (else #f)))
  '(
    ; Names defined by POSIX
    E2BIG EACCES EADDRINUSE EADDRNOTAVAIL EAFNOSUPPORT EAGAIN EALREADY
          EBADF EBADMSG EBUSY
          ECANCELED ECHILD ECONNABORTED ECONNREFUSED ECONNRESET
          EDEADLK EDESTADDRREQ EDOM EDQUOT
          EEXIST
          EFAULT EFBIG
          EHOSTUNREACH
          EIDRM EILSEQ EINPROGRESS EINTR EINVAL EIO EISCONN EISDIR
          ELOOP
          EMFILE EMLINK EMULTIHOP
          ENAMETOOLONG ENETDOWN ENETRESET ENETUNREACH ENFILE ENOBUFS ENODATA ENODEV ENOENT ENOEXEC ENOLCK
          ENOLINK ENOMEM ENOMSG ENOPROTOOPT ENOSPC ENOSR ENOSTR ENOSYS ENOTCONN ENOTDIR ENOTEMPTY ENOTRECOVERABLE
          ENOTSOCK ENOTSUP ENOTTY ENXIO
          EOPNOTSUPP EOVERFLOW EOWNERDEAD
          EPERM EPIPE EPROTO EPROTONOSUPPORT EPROTOTYPE
          ERANGE EROFS
          ESPIPE ESRCH ESTALE
          ETIME ETIMEDOUT ETXTBSY
          EWOULDBLOCK
          EXDEV
          )))
