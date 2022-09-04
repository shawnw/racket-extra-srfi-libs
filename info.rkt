#lang info
(define collection 'multi)
(define deps '("base" "typed-racket-lib" ("racket" #:version "8.6")))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-typed"))
(define pkg-desc "Ports of more SRFIs to Racket")
(define version "0.0")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
