#lang scribble/manual
@require[@for-label[racket/base srfi/151]]

@title{Extra SRFI Libraries}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

While Racket comes with a number of SRFI libraries, it's missing quite a lot of useful ones. This collection adds some.

A note on licensings: Most of the included SRFIs are the reference implementations adapted to Racket, and retain the original licenses.

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

@itemlist[

@item{Written in Typed Racket. Hopefully the type signatures should be
obvious and intuitive. If performance matters, code that uses these
routines should also be written in Typed Racket.}

@item{The @code{make-bitwise-generator} routine is currently not supported.}

]

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
