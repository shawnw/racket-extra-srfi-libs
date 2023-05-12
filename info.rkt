#lang info
(define collection 'multi)
(define deps '("base" "typed-racket-lib" "typed-racket-more" "srfi-lib"
                      "math-lib" "data-lib"
                      ("racket" #:version "8.9")))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"
                                    "rackunit-typed" "data-doc"))
(define compile-omit-paths '(#rx"srfi-160-builder")) ; This doesn't seem to actually work as advertised
(define test-omit-paths '(#rx"srfi-160-builder" #rx"160/[^bs]" #rx"private"))
(define pkg-desc "Ports of more SRFIs to Racket")
(define version "0.7")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
