#lang scribble/manual
@require[@for-label[racket/base srfi/151 racket/generator]]

@title{Extra SRFI Libraries}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

While Racket comes with a number of SRFI libraries, it's missing quite a lot of useful ones. This collection adds some.

A note on licensings: Most of the included SRFIs are the reference implementations adapted to Racket, and retain the original licenses.

@section{SRFI-112 Environment Inquiry}

@defmodule[srfi/112]

@hypderlink["https://srfi.schemers.org/srfi-112/srfi-112.html"]{Reference documentation}.

@bold{Notes:} @code{(os-version)} always returns @code{#f}, but the other functions are all implemented.

@section{SRFI-141 Integer division}

@defmodule[srfi/141]

@defmodule[typed/srfi/141]

@hyperlink["https://srfi.schemers.org/srfi-141/srfi-141.html"]{Reference documentation}.

@bold{Notes:}

The functions in the typed version are constrained to only take and
return exact integers. The regular package's will accept inexact
integers.

@section{SRFI-151 Bitwise Operations}

@defmodule[srfi/151]

@hyperlink["https://srfi.schemers.org/srfi-151/srfi-151.html"]{Reference documentation}.

@bold{Notes:}

Written in Typed Racket. Hopefully the type signatures should be
obvious and intuitive. If performance matters, code that uses these
routines should also be written in Typed Racket.

@section{SRFI-158 Generators and Accumulators}

@defmodule[srfi/158]

@hyperlink["https://srfi.schemers.org/srfi-158/srfi-158.html"]{Reference
documentation}. Also includes
@hyperlink["https://srfi.schemers.org/srfi-221/srfi-221.html"]{SRFI-221
Generator/accumulator sub-library} routines.

These generators are @bold{not} compatible with the ones in
@racket{racket/generator}. There is an adaptor function provided to
wrap Racket generators in SRFI-158 ones, but beware of conflicting
@code{generator} identifiers in the two modules.

@defproc[(rkt-generator->srfi-generator [g generator?]) (-> any/c)]{

Adapt a Racket generator to a SRFI-158 generator. Generators that are
called with arguments are not supported.

}

@section{SRFI-175 ASCII character library}

@defmodule[srfi/175]

@hyperlink["https://srfi.schemers.org/srfi-175/srfi-175.html"]{Reference documentation}.

@bold{Notes:}

What the SRFI calls a bytevector is what Racket calls a byte string.

@section{SRFI-208 NaN Procedures}

@defmodule[srfi/208]

@hyperlink["https://srfi.schemers.org/srfi-208/srfi-208.html"]{Reference documentation}.

@bold{Notes:}

Currently only works with double-precision flonums, though the SRFI
allows for other floating point types. While Racket BC supports single
precision flonums, Racket CS doesn't, and I don't have a version of CS
installed that supports extflonums for testing. If either changes I
might go back and add support for those types.

@section{SRFI-223 Generalized binary search procedures}

@defmodule[srfi/223]

@hyperlink["https://srfi.schemers.org/srfi-223/srfi-223.html"]{Reference documentation}.

@defmodule[srfi/223/bytes]

SRFI-223 procedures specialized for byte strings.

@defproc[(bytes-bisect-left [bs bytes?] [val byte?] [less? (-> byte? byte? any/c)] [lo integer? 0] [hi integer? (bytes-length bs)])
         integer?]{

Do a binary search in @code{bs} for @code{val} per SRFI-223 @code{bisect-left}.

}

@defproc[(bytes-bisect-right [bs bytes?] [val byte?] [less? (-> byte? byte? any/c)] [lo integer? 0] [hi integer? (bytes-length bs)])
         integer?]{

Do a binary search in @code{bs} for @code{val} per SRFI-223 @code{bisect-right}.

}
