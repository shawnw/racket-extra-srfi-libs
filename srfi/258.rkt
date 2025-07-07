#lang racket/base

;;;; SRFI-258 Uinterned Symbols

(provide string->uninterned-symbol symbol-interned? (rename-out [gensym generate-uninterned-symbol]))
