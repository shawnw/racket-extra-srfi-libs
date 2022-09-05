#lang info
(define collection "srfi")
(define deps '("base" "typed-racket-lib" ("racket" #:version "8.6")))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-typed" "srfi-lib"))
(define scribblings '(("scribblings/extra-srfi-libs.scrbl" ())))
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
