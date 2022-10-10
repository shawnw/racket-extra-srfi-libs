#lang racket/base
;;; SRFI-193 Command line

(require racket/contract racket/path)
(provide
 (contract-out
  [command-line (-> (listof string?))]
  [command-name (-> (or/c string? #f))]
  [command-args (-> (listof string?))]
  [script-file (-> (or/c string? #f))]
  [script-directory (-> (or/c string? #f))]
  ))

(define raw-script-path (find-system-path 'run-file))
(define in-script?
  (or
   (not (equal? raw-script-path (find-system-path 'exec-file)))
   (not (string=? (path->string (file-name-from-path (find-system-path 'exec-file))) "racket"))))

(define absolute-script-path (if in-script? (normalize-path raw-script-path) #f))

(define (command-line)
  (if in-script?
      (cons (path->string raw-script-path)
            (vector->list (current-command-line-arguments)))
      (cons "" (vector->list (current-command-line-arguments)))))

(define (command-name)
  (if in-script?
      (let* ([name (file-name-from-path raw-script-path)]
             [friendly-name (path->string (if (or (path-has-extension? name #".rkt")
                                                  (path-has-extension? name #".exe"))
                                              (path-replace-extension name #"")
                                              name))])
        friendly-name)
      #f))

(define (command-args)
  (current-command-line-arguments))

(define script-file
  (make-parameter (if in-script? (path->string absolute-script-path) #f)))

(define script-directory
  (make-parameter (if in-script? (path->string (path-only absolute-script-path)) #f)))
