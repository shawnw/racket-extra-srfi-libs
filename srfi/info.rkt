#lang info
(define collection "srfi")
(define deps '("base" "typed-racket-lib" ("racket" #:version "8.6")))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "rackunit-lib" "rackunit-typed"))
(define scribblings '(("scribblings/extra-srfi-libs.scrbl" ())))
(define pkg-authors '(shawnw))
