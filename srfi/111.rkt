#lang racket/base

;;; SRFI-111, single-valued boxes implemented via SRFI-195

(require (only-in "195.rkt" box? box unbox set-box!))
(provide (all-from-out "195.rkt"))
