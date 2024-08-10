#lang scribble/manual
@require[@for-label[racket/base racket/contract racket/generator racket/dict racket/set racket/unsafe/ops
                    racket/fixnum racket/flonum racket/fixnum racket/extflonum racket/match
                    data/order data/gvector ffi/vector]]

@title{Extra SRFI Libraries}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

While Racket comes with a number of SRFI libraries, it's missing quite a lot of useful ones. This collection adds some.

A note on licensings: Most of the included SRFIs are the reference implementations adapted to Racket, and retain the original licenses.

Typical changes to the reference versions include adding contracts and removing now-redundant type checking, avoiding things Scheme allows
that Racket doesn't like @code{if} missing an else clause, reorganization of source files, etc.

Modules that are noted as providing an @tt{unsafe} option have a submodule named @tt{unsafe} that exports functions without contracts and potentially other run time checks.
Passing those functions invalid or out of range arguments can have unpredictable results. Use with caution.

@section{SRFI-1 List Library}

@subsection{Typed Racket}

@defmodule[typed/srfi/1]
@defmodule[typed/srfi/lists]

@hyperlink["https://srfi.schemers.org/srfi-1/srfi-1.html"]{Reference documentation}.

Imports SRFI-1 functions into a Shallow Typed Racket module.
Circular and dotted lists aren't always accepted where the documentation says they should be; I still need to work out if it's even possible to represent a circular list in TR.

The linear update procedures ending with a ! are just aliases for the normal ones, thanks to Racket's immutable cons cells. This behavior is allowed by the SRFI.

@defform[#:kind "type" (Circular-Listof a)]{

The type of a homogeneous circular list as returned by @code{circular-list}.

}

@subsection{Mutable lists}

@defmodule[srfi/1m]
@defmodule[srfi/mlists]

SRFI-1 for mutable lists. Procedures have the same name with an @tt{m} prepended, except ones with @tt{list} or @tt{pair} in the name, which turns into @tt{mlist} or @tt{mpair}.
So, for example, @code{make-mlist}, @code{not-mpair?} and @code{mxcons}.

Also available in an unsafe module with @code{(require (submod srfi/1m unsafe))}.

@subsubsection{Extra functions}

@defproc[(list->mlist [lst list?]) proper-mlist?]{

 Convert a normal immutable list into one made of mutable pairs.

}


@defproc[(mlist->list [mlst proper-mlist?]) list?]{

 Convert a mutable list into a normal one made of immutable pairs.

}

@section{SRFI-13 String Libraries}

@defmodule[typed/srfi/13]
@defmodule[typed/srfi/strings]

@hyperlink["https://srfi.schemers.org/srfi-13/srfi-13.html"]{Reference documentation}.

@bold{Notes}: While Racket comes with a SRFI-13 implementation, it's
only for normal Racket, not Typed Racket. This module can be used
instead of having to @racketid{require/typed} specific functions from it
that you might need in Typed Racket.

@section{SRFI-27 Sources of Random Bits}

@defmodule[typed/srfi/27]
@defmodule[typed/srfi/random-bits]

@hyperlink["https://srfi.schemers.org/srfi-27/srfi-27.html"]{Reference
documentation}.

@bold{Notes}: While Racket comes with a SRFI-27 implementation, it's only for
normal Racket, not Typed Racket. This module can be used instead of having to
@racketid{require/typed} specific functions from it that you might need in
Typed Racket.

@section{SRFI-74 Octet-Addressed Binary Blocks}

@defmodule[typed/srfi/74]
@defmodule[typed/srfi/blobs]

@hyperlink["https://srfi.schemers.org/srfi-74/srfi-74.html"]{Reference documentation}.

@bold{Notes}: The untyped version of this module in @code{srfi-lib} is just the reference implementation and has, as of Racket 8.8,
a serious bug in that the "native" endianness is always big-endian. This version, besides being written in Typed Racket, fixes that,
and uses a more Racket-specific implementation. You should probably use always use this one, at least until the bug is fixed.

@section{SRFI-87 => in case clauses}

@defmodule[srfi/87x]

@hyperlink["https://srfi.schemers.org/srfi-87/srfi-87.html"]{Reference documentation}.

While Racket comes with a SRFI-87 implementation in @code{srfi-lib}, trying to use it causes syntax errors.
This version actually works, and it follows the Racket convention of using @code{equal?} instead of @code{eqv?} to test values in @code{case}.

@section{SRFI-111 Boxes}

@defmodule[srfi/111]

@hyperlink["https://srfi.schemers.org/srfi-111/srfi-111.html"]{Reference documentation}.

Racket natively supports SRFI-111 single-valued boxes, but this module re-exports SRFI-195 versions of functions to comply with that document.

@section{SRFI-112 Environment Inquiry}

@defmodule[srfi/112]

@hyperlink["https://srfi.schemers.org/srfi-112/srfi-112.html"]{Reference documentation}.

@bold{Notes}: @code{(os-version)} always returns @code{#f}, but the other functions are all implemented.

@section{SRFI-117 Queues based on (mutable) lists}

@defmodule[srfi/117]
@defmodule[srfi/list-queues]

@hyperlink["https://srfi.schemers.org/srfi-117/srfi-117.html"]{Reference documentation}.

The SRFI uses normal lists, which are immutable in Racket. Therefore, this implemention uses mutable lists of @code{mcons} pairs.
The single argument form of @code{make-list-queue} and two argument form of @code{list-queue-set-list!} can be passed normal lists, though.
@code{list-queue-list} returns a mutable list.

Also available in an unsafe module with @code{(require (submod srfi/117 unsafe))}.


@defproc[(in-list-queue [lq list-queue?]) sequence?]{

Return a sequence that iterates over the elements of the queue. List queues can also be used directly as sequences.

}

@section{SRFI-127 Lazy Sequences}

@defmodule[srfi/127]
@defmodule[srfi/lazy-sequences]

@hyperlink["https://srfi.schemers.org/srfi-127/srfi-127.html"]{Reference documentation}.

@bold{Notes}: The lazy sequences described in the SRFI are built on
normal, mutable Scheme @code{cons} cells. Racket @code{cons} cells are
in theory immutable, so this implementation instead uses @code{mcons}
cells; lists of which aren't compatible with list functions. There's a
few extra functions to help make that easier to work with, as well as the @code{srfi/1m} module.

@defproc[(list->lseq [lst list?]) lseq?]{

Convert a list into an lseq.

}

@defproc[(make-lseq [val any/c] ...) lseq?]{

Converts its arguments into an lseq.

}

@defproc[(in-lseq [lseq lseq?]) sequence?]{

Create a Racket sequence that iterates over the lseq.

}

@section{SRFI-128 Comparators (reduced)}

@defmodule[srfi/128]
@defmodule[srfi/comparators]

@hyperlink["https://srfi.schemers.org/srfi-128/srfi-128.html"]{Reference
documentation}. Also includes
@hyperlink["https://srfi.schemers.org/srfi-162/srfi-162.html"]{SRFI-162
Comparators sublibrary} and
@hyperlink["https://srfi.schemers.org/srfi-228/srfi-228.html"]{SRFI-228
Composing Comparators} routines and variables.

The @code{make-eq-comparator}, @code{make-eqv-comparator},
@code{make-equal-comparator} and @code{make-equal-always} functions
return comparators using the standard Racket @code{eq-hash-code}
etc. hash functions instead of @code{default-hash}.

@code{make-comparator} takes an optional keyword argument,
@racketid{#:secondary-hash}, whose value has the same signature as the
@racketid{hash} one - either @code{(-> any/c exact-integer?)} or
@code{#f}. This is used for better compability with Racket's custom
hash tables, which take two hash functions. Hash functions can also return
negative numbers, contrary to the SRFI spec.

Comparators can be used as flat contracts (And thus predicates) that test their argument
against their type test predicate.

@subsection{Additional definitions}

@defproc[(comparator-secondary-hash-function [cmp comparator?]) (-> any/c exact-integer?)]{

Returns the secondary hash function associated with the comparator.

}

@defproc[(comparator-secondary-hash [cmp comparator?] [obj any/c]) exact-integer?]{

Returns the secondary hash code of the given value.

}

@defproc[(make-equal-always-comparator) comparator?]{

Return a comparator that uses @code{equal-always?} for equality and
@code{equal-always-hash-code}/@code{equal-always-secondary-hash-code} for hashing.

}

@defthing[equal-always-comparator comparator?]{

A comparator returned by @code{make-equal-always-comparator}.

}

@section{SRFI-132 Sort Libraries}

@defmodule[srfi/132]
@defmodule[srfi/sorting]

@hyperlink["https://srfi.schemers.org/srfi-132/srfi-132.html"]{Reference
documentation}.

@code{vector-sort} and @code{vector-sort!} conflict with
the ones in @code{racket/vector} - the order of the vector and
ordering predicate is reversed.

The side-effect-enabled list functions @code{list-merge!} and
@code{list-delete-neighbor-dups!} currently use
@code{unsafe-immutable-set-cdr!} to modify the lists in place. The
test cases pass, but if this becomes an issue in practice (The
function has lots of warnings attached), I'll switch them to just
being aliases for the non-side-effect versions.

Available as an unsafe module via @code{(require (submod srfi/132 unsafe))}.

@section{SRFI-133 Vector Library (R7RS-compatible)}

@defmodule[srfi/133]
@defmodule[srfi/vectors]
@defmodule[typed/srfi/133]
@defmodule[typed/srfi/vectors]

@hyperlink["https://srfi.schemers.org/srfi-133/srfi-133.html"]{Reference documentation}.

See also @hyperlink["https://srfi.schemers.org/srfi-43/srfi-43.html"]{SRFI-43} included with Racket.
Notable differences are functions that take callbacks not passing the current index like they do in 43.

Also available in a Typed Racket version and an unsafe version as @code{(require (submod srfi/133 unsafe))}.

@section{SRFI-134 Immutable Deques}

@defmodule[srfi/134]

@hyperlink["https://srfi.schemers.org/srfi-134/srfi-134.html"]{Reference documentation}.

@bold{Notes}:

Deques are @code{equal?} if they are the same length and all corresponding elements are @code{equal?}.

Deques are streams and sequences.

As of Racket 8.13, implemented with treelists instead of the banker's deque reference implementation.

@bold{Extra functions}:

@defproc[(in-ideque [dq ideque?]) sequence?]{

 Returns a sequence that iterates through the deque from front to back. A @code{ideque?} can also be used directly as a sequence.

}

@defproc[(in-ideque-backwards [dq ideque?]) sequence?]{

 Returns a sequence that iterates through the deque in reverse order, back to front.

}

@defform[(for/ideque (sequence-binding ...) (body ...))]{

 A @code{for} comprehension that returns a queue built from the value returned by each iteration of the body.

}

@defform[(for*/ideque (sequence-binding ...) (body ...))]{

 A @code{for*} comprehension that returns a queue built from the value returned by each iteration of the body.

}

@section{SRFI-141 Integer division}

@defmodule[srfi/141]
@defmodule[srfi/integer-division]
@defmodule[typed/srfi/141]
@defmodule[typed/srfi/integer-division]

@hyperlink["https://srfi.schemers.org/srfi-141/srfi-141.html"]{Reference documentation}.

@bold{Notes}:

The functions in the typed version are constrained to only take and
return exact integers. The regular package's will accept inexact
integers. The typed module also has @code{Fixnum} versions with a
@racketid{fx} prefix - @code{fxfloor/}, for example.

@section{SRFI-143 Fixnums}

@defmodule[srfi/143]
@defmodule[srfi/fixnums]
@defmodule[typed/srfi/143]
@defmodule[typed/srfi/fixnums]

@hyperlink["https://srfi.schemers.org/srfi-143/srfi-143.html"]{Reference documentation}.

@section{SRFI-145 Assumptions}

@defmodule[srfi/145]
@defmodule[srfi/assume]

@hyperlink["https://srfi.schemers.org/srfi-145/srfi-145.html"]{Reference documentation}.

@defstruct*[(exn:fail:contract:assume exn:fail:contract) () #:transparent #:extra-constructor-name make-exn:fail:contract:assume]{

The type of exception raised by @code{assume}.

}


@section{SRFI-146 Mappings}

@hyperlink["https://srfi.schemers.org/srfi-146/srfi-146.html"]{Reference documentation}.

@defmodule[srfi/146]

@code{mapping?} objects are also @code{ordered-dict?}s and many
functions in this module can be used with other ordered dicts or even unordered-ones for operations where order doens't matter.
The mapping implementation uses
@hyperlink["https://en.wikipedia.org/wiki/Scapegoat_tree"]{scapegoat trees}.

The current @code{make-mapping-comparator} and @code{mapping-comparator} do not provide ordering.

The linear update versions of functions with names ending in !, if passed an external dict object that supports side effect operations, will update it in place and return it.
If passed an immutable dict, they will just return a new object like the non-linear versions. Basically, as long as you follow the guideline of never using a particular dict
object again after passing it to a linear update function, everything will Just Work no matter what.

@bold{Additional functions}:

@defproc[(in-ordered-dict [od ordered-dict?] [starting-pos any/c (dict-iterate-least od)])
         sequence?]{

Return a sequence of key/value values that starts with the least key
(Or the given iteration position). The @code{dict-iterate-next}
implementation of the ordered dict is assumed to return elements in
ascending order of keys.

}

@defproc[(in-ordered-dict-keys [od ordered-dict?] [starting-pos any/c (dict-iterate-least od)])
         sequence?]{

Like @code{in-ordered-dict} but only returns the keys.

}

@defproc[(in-ordered-dict-values [od ordered-dict?] [starting-pos any/c (dict-iterate-least od)])
         sequence?]{

Like @code{in-ordered-dict} but only returns the values.

}

@defmodule[srfi/146/hash]

@code{hashmap?} objects are also @code{dict?}s and many functions in
this module can be used with other types of dicts. The hashmap implementation
uses Racket's immutable hash tables, but also support mutable side-effect functionality.

@section{SRFI-151 Bitwise Operations}

@defmodule[srfi/151]
@defmodule[srfi/bitwise-operations]

@hyperlink["https://srfi.schemers.org/srfi-151/srfi-151.html"]{Reference documentation}.

@bold{Notes}:

Written in Typed Racket. Hopefully the type signatures should be
obvious and intuitive. If performance matters, code that uses these
routines should also be written in Typed Racket.

@section{SRFI-158 Generators and Accumulators}

@defmodule[srfi/158]
@defmodule[srfi/generators-and-accumulators]

@hyperlink["https://srfi.schemers.org/srfi-158/srfi-158.html"]{Reference
documentation}. Also includes
@hyperlink["https://srfi.schemers.org/srfi-221/srfi-221.html"]{SRFI-221
Generator/accumulator sub-library} routines.

These generators are @bold{not} compatible with the ones in
@racket{racket/generator}. There is an adaptor function provided to
wrap Racket generators in SRFI-158 ones, but beware of conflicting
@code{generator} identifiers in the two modules.

Available as an unsafe module via @code{(require (submod srfi/158 unsafe))}.

@defproc[(generator? [obj any/c]) boolean?]{

Returns @code{#t} if the object appears to be a generator - a
procedure that can be called with 0 arguments, and, if the number of
values it returns is known, only returns 1.

}

@defproc[(rkt-generator->srfi-generator [g generator?]) (-> any/c)]{

Adapt a Racket generator to a SRFI-158 generator. Generators that are
called with arguments are not supported.

}

@defproc[(sequence->generator [s sequence?]) (-> any/c)]{

Adapt a Racket sequence to a SRFI-158 generator.

}

@section{SRFI-160 Homogenous numeric vector libraries}

@hyperlink["https://srfi.schemers.org/srfi-160/srfi-160.html"]{Reference
documentation}. In addition to all the numeric types in the SRFI,
functions for @code{flvector} and @code{fxvector} vectors are also
provided, with a @racketid{fl} and @racketid{fx} prefix
respectively. If Racket CS ever gains support for 80-bit
@code{extflonum?}  numbers on x86, I'll add support for
@code{extflvector?} and @code{f80vector?} vectors too (And might
adjust the @code{f32vector?} contracts to explicitly work with
@code{single-flonum?} values if CS ever gets them).

All these modules have an unsafe submodule.

@defmodule[srfi/160/base]

Additional functions for converting between flvectors and SRFI-4 vectors:

@defproc[(flvector->f32vector [fl flvector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (flvector-length fv)]) f32vector?]{}

@defproc[(flvector->f64vector [fl flvector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (flvector-length fv)]) f64vector?]{}

@defproc[(f32vector->flvector [f32 f32vector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (f32vector-length f32)]) flvector?]{}

@defproc[(f64vector->flvector [f64 f64vector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (f64vector-length f64)]) flvector?]{}

@defmodule[srfi/160/u8]
@defmodule[srfi/160/s8]
@defmodule[srfi/160/u16]
@defmodule[srfi/160/s16]
@defmodule[srfi/160/u32]
@defmodule[srfi/160/s32]
@defmodule[srfi/160/u64]
@defmodule[srfi/160/s64]
@defmodule[srfi/160/f32]
@defmodule[srfi/160/f64]
@defmodule[srfi/160/c64]
@defmodule[srfi/160/c128]
@defmodule[srfi/160/fl]
@defmodule[srfi/160/fx]

Additional functions:

@defproc[(in-\@vector [vec \@vector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (\@vector-length vec)]) sequence?]{

Sequence constructor that iterates over the given range of the numeric vector.

}

@section{SRFI-173 Hooks}

@defmodule[srfi/173]

@hyperlink["https://srfi.schemers.org/srfi-173/srfi-173.html"]{Reference documentation}.

@bold{Notes}: A hook object is callable as a procedure;
@code{(hook-obj args ...)} is the same as @code{(hook-run hook-obj args ...)}.

@section{SRFI-174 POSIX Timespecs}

@defmodule[srfi/174]

@hyperlink["https://srfi.schemers.org/srfi-174/srfi-174.html"]{Reference documentation}.

@bold{Notes}:

Implemented as a distinct type (A transparent structure), with the range of the seconds values the same as the range of Racket integers.

@code{timespec-hash} uses @code{equal-hash-code} and might return negative values contrary to the SRFI description of the function.

@section{SRFI-175 ASCII character library}

@defmodule[srfi/175]
@defmodule[srfi/ascii]

@hyperlink["https://srfi.schemers.org/srfi-175/srfi-175.html"]{Reference documentation}.

What the SRFI documentation calls a bytevector is what Racket calls a byte string.

Available as an unsafe module via @code{(require (submod srfi/175 unsafe))}.

@section{SRFI-180 JSON}

@defmodule[srfi/180]

@hyperlink["https://srfi.schemers.org/srfi-180/srfi-180.html"]{Reference documentation}.

You can never have too many JSON parsers available for a language.

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

@defmodule[srfi/194]

@hyperlink["https://srfi.schemers.org/srfi-194/srfi-194.html"]{Reference documentation}.

@bold{Notes}:

Written in Typed Racket.

@code{make-ellipsoid-generator}, @code{make-ball-generator} and
@code{make-sphere-generator} have variants @code{flmake-ellipsoid-generator},
@code{flmake-ball-generator} and @code{flmake-sphere-generator} that return
flvectors. The ellipsoid generator functions can take a flvector or a vector
of reals. The ball generator functions can take a flvector, vector of reals,
or an integer.

@section{SRFI-195 Multiple-value Boxes}

@defmodule[srfi/195]

@hyperlink["https://srfi.schemers.org/srfi-195/srfi-195.html"]{Reference documentation}.

@bold{Notes}:

Native Racket single-valued boxes are accepted by these functions, and multiple-valued boxes of arity 1 created by this SRFI's @code{box} use them.

Multiple-valued boxes can be compared and hashed with @code{equal?}. The usual caveats about modifying a box used as a key in a hash table apply.

@subsection{Additional procedures and forms}

@defform[(mvbox match-pat ...)]

A match expander to use multiple-valued boxes in @code{match} clauses.

@codeblock{
           (match (box 1 2 3)
                  [(mvbox a b c) (list a b c)]) ; '(1 2 3)
}

@defproc[(box-immutable [arg any/c] ...) box?]{

Creates an immutable box; using @code{set-box!} or @code{set-box-value!} with it is an error.
 Only single-valued boxes make @code{immutable?} return true.

}

@section{SRFI-196 Range Objects}

@defmodule[srfi/196]

@hyperlink["https://srfi.schemers.org/srfi-196/srfi-196.html"]{Reference documentation}.

@bold{Notes}: The @racket{range} function from this module conflicts with the one from @racket{racket/list}.

@subsection{Additional functions}

@defproc[(range-empty? [r range?]) boolean?]{

 Returns true if the range's length is 0, otherwise false.

}

@defproc[(in-range-object [r range?]) sequence?]{

 Returns a sequence that iterates over the range. Range objects can also be used directly as sequences.

}

@section{SRFI-202 Pattern-matching Variant of the @code{and-let*} Form that Supports Multiple Values}

@defmodule[srfi/202]

@hyperlink["https://srfi.schemers.org/srfi-202/srfi-202.html"]{Reference documentation}.

@bold{Notes}:

The reference implementations for this SRFI include one for Racket, but this one is original, using @code{syntax-parse} macros. It's also a lot simpler,
which makes me wonder, but it passes all the test cases...

Uses @code{match} style pattern matching.

@section{SRFI-207 String-notated bytevectors}

@defmodule[srfi/207]
@defmodule[srfi/bytestrings]

@hyperlink["https://srfi.schemers.org/srfi-207/srfi-207.html"]{Reference documentation}.

The @code{u8"..."} reader syntax and I/O functions are not supported.

Available in an unsafe version as @code{(require (submod srfi/207 unsafe))}.

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

@section{SRFI-210 Procedures and Syntax for Multiple Values}

@defmodule[srfi/210]

@hyperlink["https://srfi.schemers.org/srfi-210/srfi-210.html"]{Reference documentation}.

@bold{Notes}:

@code{apply/mv}, @code{call/mv}, and @code{with-values} have been extended to work with Racket's keyword arguments by taking optional keyword+value arguments
after the last documented ones that are passed to the consumer procedure. For example,

@codeblock{
(apply/mv ~a #\a (values #\b #\c) #:separator ", ")
}

The forms and functions that take/return boxes use SRFI-195 multiple-value ones.

@subsection{Additional functions}

@defproc[(bind/vector [vec vector?] [transducer procedure?] ...) any]{

 Like @code{bind/list} but takes a vector instead of a list.

}

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

@defproc[(flexvector->bytes [fv (flexvectorof byte?)] [start exact-integer? 0] [end exact-integer? (flexvector-length fv)]) bytes?]{

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

@defproc[(in-flexvector [fv flexvector?]) sequence?]{

 Returns a sequence that iterates through the elements of the flexvector.

}

@section{SRFI-217 Integer Sets}

@defmodule[srfi/217]

@hyperlink["https://srfi.schemers.org/srfi-217/srfi-217.html"]{Reference documentation}.

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

Available as an unsafe module via @code{(require (submod srfi/217 unsafe))}.

@section{SRFI-223 Generalized binary search procedures}

@defmodule[srfi/223]
@defmodule[srfi/bisect]

@hyperlink["https://srfi.schemers.org/srfi-223/srfi-223.html"]{Reference documentation}.
This and the following modules are written in Typed Racket.

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

@racket{fxmapping} objects support the @code{gen:dict}
interface and can thus be used with @racket{racket/dict} functions.
They also support @code{equal?} and are hashable so they can be used
as keys in hash tables and sets. The usual warnings about mutating
values stored in such mappings apply.

Available as an unsafe module via @code{(require (submod srfi/224 unsafe))}.

@section{SRFI-232 Flexible curried procedures}

@defmodule[srfi/232]

@hyperlink["https://srfi.schemers.org/srfi-232/srfi-232.html"]{Reference documentation}.

@section{SRFI-235 Combinators}

@defmodule[srfi/235]

@hyperlink["https://srfi.schemers.org/srfi-235/srfi-235.html"]{Reference documentation}.

@bold{Notes}:

The @code{conjoin} and @code{disjoin} functions conflict with the ones in @racket{racket/function}
and @code{group-by} with the one in @racket{racket/list}.

@code{case-procedure} uses @code{equal?} to compare values instead of @code{eqv?} to match Racket's @code{case}.

Procedures that take or return procedures that take arbitary arguments will work with keyword arguments for the most part.

@section{SRFI-238 Codesets}

@defmodule[srfi/238]
@defmodule[srfi/codesets]

@hyperlink["https://srfi.schemers.org/srfi-238/srfi-238.html"]{Reference documentation}.

The only predefined codeset is @code{'errno} with POSIX errno values.

@subsection{Additional functions}

@defproc[(register-codeset! [name symbol?] [data (listof (list/c (or/c symbol? #f) (or/c exact-integer? #f) (or/c string? #f)))]) void?]{

Define and register a new codeset with the given names, numbers and messages. The message is optional, and either the name or number is, but not both.

}

@section{SRFI-239 Destructuring Lists}

@defmodule[srfi/239]
@defmodule[srfi/list-case]

@hyperlink["https://srfi.schemers.org/srfi-239/srfi-239.html"]{Reference documentation}.

Normally you'd use @code{match} in Racket, but this is a lightweight alternative when just dealing with lists.

@subsection{Additional functions}

@defform[(mlist-case mlist-expr list-case-clause ...)]{

 Like @code{list-case} but for mutable lists.

}

@defproc[(exn:fail:list-case? [obj any/c]) boolean?]{

 Predicate for the exceptions raised by @code{list-case} when no matching clause is provided.

}
