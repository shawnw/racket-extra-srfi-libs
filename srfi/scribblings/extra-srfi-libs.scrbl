#lang scribble/manual
@require[@for-label[racket/base srfi/151]]

@title{Extra SRFI Libraries}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

While Racket comes with a number of SRFI libraries, it's missing quite a lot of useful ones. This collection adds some.

A note on licensings: Most of the included SRFIs are the reference implementations adapted to Racket, and retain the original licenses.

@section{SRFI-151 Bitwise Operations}

@defmodule[srfi/151]

@hyperlink["https://srfi.schemers.org/srfi-151/srfi-151.html"]{Reference Documentation}

@bold{Notes:}

@itemlist[

@item{Written in Typed Racket. Hopefully the type signatures should be
obvious and intuitive. If performance matters, code that uses these
routines should also be written in Typed Racket.}

@item{The @code{make-bitwise-generator} routine is currently not supported.}

]
