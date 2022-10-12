#lang scribble/manual
@require[@for-label[racket/base racket/generator racket/dict racket/set racket/unsafe/ops
                    data/gvector]]

@title{Extra SRFI Libraries}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

While Racket comes with a number of SRFI libraries, it's missing quite a lot of useful ones. This collection adds some.

A note on licensings: Most of the included SRFIs are the reference implementations adapted to Racket, and retain the original licenses.

Typical changes to the reference versions include adding contracts and removing now-redundant type checking, avoiding things Scheme allows
that Racket doesn't like @code{if} missing an else clause, reorganization of source files, etc.

@section{SRFI-112 Environment Inquiry}

@defmodule[srfi/112]

@hyperlink["https://srfi.schemers.org/srfi-112/srfi-112.html"]{Reference documentation}.

@bold{Notes:} @code{(os-version)} always returns @code{#f}, but the other functions are all implemented.

@section{SRFI-128 Comparators (reduced)}

@defmodule[srif/128]

@hyperlink["https://srfi.schemers.org/srfi-128/srfi-128.html"]{Reference
documentation}. Also includes
@hyperlink["https://srfi.schemers.org/srfi-162/srfi-162.html"]{SRFI-162
Comparators sublibrary} and
@hyperlink["https://srfi.schemers.org/srfi-228/srfi-228.html"]{SRFI-228
A further comparator library (draft)} routines and variables.

@bold{Notes}:

The @code{make-eq-comparator}, @code{make-eqv-comparator} and
@code{make-equal-comparator} functions return comparators using the
standard Racket @code{eq-hash-code} etc. hash functions instead of
@code{default-hash}. Not terribly useful at the moment, but it allows
for Racket structs with custom hash functions to work in case a hash
table SRFI that uses comparators is added to this collection later.

@subsection{Additional definitions}

@defproc[(make-equal-always-comparator) comparator?]{

Return a comparator that uses @code{equal-always?} for equality and
@code{equal-always-hash-code} for hashing.

}

@defthing[equal-always-comparator comparator?]{

A comparator returned by @code{make-equal-always-comparator}.

}

@section{SRFI-132 Sort Libraries}

@defmodule[srif/132]

@hyperlink["https://srfi.schemers.org/srfi-132/srfi-132.html"]{Reference
documentation}.

@bold{Notes:}

@code{vector-sort} and @code{vector-sort!} conflict with
the ones in @code{racket/vector} - the order of the vector and
ordering predicate is reversed.

The side-effect-enabled list functions @code{list-merge!} and
@code{list-delete-neighbor-dups!} currently use
@code{unsafe-immutable-set-cdr!} to modify the lists in place. The
test cases pass, but if this becomes an issue in practice (The
function has lots of warnings attached), I'll switch them to just
being aliases for the non-side-effect versions.

@section{SRFI-141 Integer division}

@defmodule[srfi/141]

@defmodule[typed/srfi/141]

@hyperlink["https://srfi.schemers.org/srfi-141/srfi-141.html"]{Reference documentation}.

@bold{Notes:}

The functions in the typed version are constrained to only take and
return exact integers. The regular package's will accept inexact
integers.

@section{SRFI-145 Assumptions}

@defmodule[srfi/145]

@hyperlink["https://srfi.schemers.org/srfi-145/srfi-145.html"]{Reference documentation}.

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

@section{SRFI-173 Hooks}

@defmodule[srfi/173]

@hyperlink["https://srfi.schemers.org/srfi-173/srfi-173.html"]{Reference documentation}.

@bold{Notes}: A hook object is callable as a procedure;
@code{(hook-obj args ...)} is the same as @code{(hook-run hook-obj args ...)}.

@section{SRFI-175 ASCII character library}

@defmodule[srfi/175]

@hyperlink["https://srfi.schemers.org/srfi-175/srfi-175.html"]{Reference documentation}.

@bold{Notes}: What the SRFI calls a bytevector is what Racket calls a byte string.

@section{SRFI-190 Coroutine Generators}

@defmodule[srfi/190]

@hyperlink["https://srfi.schemers.org/srfi-190/srfi-190.html"]{Reference documentation}.

@bold{Notes:} The @code{yield} syntax conflicts with @racket{racket/generator}.

@section{SRFI-193 Command line}

@defmodule[srfi/193]

@hyperlink["https://srfi.schemers.org/srfi-193/srfi-193.html"]{Reference documentation}.

@bold{Notes}:

The heuristics for telling if a command or script or
neither is being executed could probably stand to be improved.

The @code{command-line} procedure conflicts with the one in
@racket{racket/cmdline}.

@section{SRFI-194 Random data generators}

@defmodule[srif/194]

@hyperlink["https://srfi.schemers.org/srfi-194/srfi-194.html"]{Reference documentation}.

@code{make-ellipsoid-generator} and @code{make-ball-generator} can
take a @code{flvector} or a vector of reals as an argument and returns
the same.

@section{SRFI-207 String-notated bytevectors}

@defmodule[srfi/207]

@hyperlink["https://srfi.schemers.org/srfi-207/srfi-207.html"]{Reference documentation}.

@bold{Notes}: The @racket{u8"..."} reader syntax and I/O functions are not supported.

@section{SRFI-208 NaN Procedures}

@defmodule[srfi/208]

@defmodule[typed/srfi/208]

@hyperlink["https://srfi.schemers.org/srfi-208/srfi-208.html"]{Reference documentation}.

@bold{Notes}:

Currently only works with double-precision flonums, though the SRFI allows for
other floating point types. While Racket BC supports single precision flonums,
Racket CS doesn't, and I don't have a version of CS installed that supports
extflonums (maybe it doesn't support them at all either?). If either condition
changes I might go back and add support for those types.

The main difference between the untyped and typed versions are that the former
includes NaN checking of arguments in the contracts; the typed one has an
explicit check to raise an error if passed a non-NaN number. I might remove
that check in the future and just say it's undefined what happens when they're
passed a non-NaN.

@section{SRFI-214 Flexvectors}

@defmodule[srfi/214]

@hyperlink["https://srfi.schemers.org/srfi-214/srfi-214.html"]{Reference documentation}.

@bold{Notes}: The impelmentation is built on @racket{data/gvector} and
flexvectors are also @code{gvector}s.

@subsection{Additional functions}

@defproc[(flexvectorof [c contract?] [#:flat-contract boolean? #f]) contract?]{

Returns a contract that validates flexvectors whose every element satisfies @code{c}.

}

@defproc[(build-flexvector [len exact-nonnegative-integer?] [proc (-> exact-nonnegative-integer? any/c)]) flexvector?]{

Return a new flexvector @code{len} elements long, populated with the results of calling @code{proc} for each index.

}

@defproc[(flexvector->bytes [fv (flexvectorof byte?)] [start exact-integer? 0] [end exact-integer? (flexvector-length fv)]) bytes?] {

Convert a flexvector of bytes to a byte string.

}

@defproc[(bytes->flexvector [bs bytes?] [start exact-integer? 0] [end exact-integer? (bytes-length bs)]) (flexvectorof byte?)]{

Convert a byte string to a flexvector.

}

@defproc[(flexvector-bisect-left [fv flexvector?] [val any/c] [less? (-> any/c? any/c? any/c)] [lo integer? 0] [hi integer? (flexvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-left}.

}

@defproc[(flexvector-bisect-right [fv flexvector?] [val any/c] [less? (-> any/c? any/c? any/c)] [lo integer? 0] [hi integer? (flexvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-right}.

}


@section{SRFI-217 Integer Sets}

@defmodule[srfi/217]

@hyperlink["https://srfi.schemers.org/srfi-217/srfi-217.html"]{Reference documentation}.

@bold{Notes}:

@racket{iset} objects also support the Racket @code{gen:set} interface
and can be used with @racket{racket/set} functions. The set-theory
functions like @code{set-union} only work when all sets are isets (The
same restriction applies to other types of sets).

They also support @code{equal?} and are hashable so they can be used
as keys in hash tables and sets. The usual warnings about mutating
such sets apply.

The reference implementation, using Patricia trees, is currently being
used. I'm considering replacing it with one that can more compactly
store ranges of numbers.

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

@defmodule[srfi/223/flvector]

SRFI-223 procedures specialized for flvectors.

@defproc[(flvector-bisect-left [fv flvector?] [val flonum?] [less? (-> flonum? flonum? any/c)] [lo integer? 0] [hi integer? (flvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-left}.

}

@defproc[(flvector-bisect-right [fv flvector?] [val flonum?] [less? (-> flonum? flonum? any/c)] [lo integer? 0] [hi integer? (flvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-right}.

}

@defmodule[srfi/223/fxvector]

SRFI-223 procedures specialized for fxvectors.

@defproc[(fxvector-bisect-left [fv fxvector?] [val fixnum?] [less? (-> fixnum? fixnum? any/c)] [lo integer? 0] [hi integer? (fxvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-left}.

}

@defproc[(fxvector-bisect-right [fv fxvector?] [val fixnum?] [less? (-> fixnum? fixnum? any/c)] [lo integer? 0] [hi integer? (fxvector-length fv)])
         integer?]{

Do a binary search in @code{fv} for @code{val} per SRFI-223 @code{bisect-right}.

}

@section{SRFI-224 Integer Mappings}

@defmodule[srfi/224]

@hyperlink["https://srfi.schemers.org/srfi-224/srfi-224.html"]{Reference documentation}.

@bold{Notes}: @racket{fxmapping} objects support the @code{gen:dict}
interface and can thus be used with @racket{racket/dict} functions.
They also support @code{equal?} and are hashable so they can be used
as keys in hash tables and sets. The usual warnings about mutating
values stored in such mappings apply.

@section{SRFI-232 Flexible curried procedures}

@defmodule[srfi/232]

@hyperlink["https://srfi.schemers.org/srfi-232/srfi-232.html"]{Reference documentation}.
