#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     dbc)
          scribble/example)

@(define dbc-eval (make-base-eval #:lang 'racket/base '(require dbc)))

@title[#:tag "dbc"]{dbc: Vector DBC (CAN database) files}
@author{turinglambdaai}

@defmodule[dbc]

The @filepath{dbc} package reads and writes @deftech{DBC} files — the
de-facto-standard CAN database format invented by Vector Informatik and
consumed by virtually every CAN tool in the automotive industry.  A DBC file
describes the symbolic layer of a CAN bus: message and signal names, bit
layouts, scaling, value tables (choices), comments and attributes.

Everything is implemented in pure Racket: no FFI, no system dependency.
The implementation is validated bit-for-bit against the Python
@hyperlink["https://github.com/cantools/cantools"]{@tt{cantools}} library on
its own test corpus (Intel and Motorola byte order, signed/unsigned and IEEE
float signals, multiplexing, extended multiplexing).

@section{Quick start}

@examples[
#:eval dbc-eval
(define db (parse-dbc #<<DBC
VERSION "1.0"

BO_ 37 ExampleMessage: 8 Vector__XXX
 SG_ VehicleSpeed : 39|16@1+ (0.01,0) [-100|100] "kph" Vector__XXX
 SG_ Temperature : 0|12@0- (0.01,250) [-40|215] "degC" Vector__XXX

VAL_ 37 VehicleSpeed 0 "standstill" 1 "moving" ;
DBC
))
(define m (for/first ([e (in-list (dbc-entries db))]
                      #:when (entry-bo? e))
            (entry-bo-message e)))
(message-name m)
(message-decode m (bytes #x01 #xb0 0 0 0 0 0 0))
(message-decode m (bytes #x01 #xb0 0 0 0 0 0 0) #:decode-choices? #t)
(message-encode m (hash 'Temperature 250.47 'VehicleSpeed 0.5))
]

A file is parsed with @racket[parse-dbc], which returns a @racket[dbc?]
value.  Two views live side by side:

@itemlist[
 @item{the @emph{entry stream} — @racket[(dbc-entries db)] keeps every
  grammar element of the file (@racket[BO_], @racket[CM_], @racket[BA_DEF_],
  @racket[VAL_], …) in source order, which is what makes lossless
  serialization possible; and}
 @item{the @emph{codec view} — each @racket[entry-bo] wraps a
  @racket[message?] whose @racket[signal?]s carry the decoded
  choices/comments/float kinds, ready for @racket[message-decode] and
  @racket[message-encode].}
]

@section{Bit numbering conventions}

A payload bit @tt{n} lives in byte @tt{(quotient n 8)} at bit position
@tt{(remainder n 8)} counting from the LSB.

@itemlist[
 @item{@bold{Intel} @(tt "@1") (little-endian):: the value's LSB sits at
  @racket[signal-start-bit]; the value occupies bits @tt{start …
  start+len-1}, accumulated LSB-first.}
 @item{@bold{Motorola} @(tt "@0") (big-endian): the value's MSB sits at
  @racket[signal-start-bit]; the remaining bits walk the sawtooth — from a
  byte's LSB the walk jumps to the next byte's MSB (@tt{+15}), otherwise it
  descends one position (@tt{-1}) — accumulated MSB-first.  This is the
  same placement @tt{cantools} produces (verified bit by bit).}
]

@section{Parser and serializer}

@defproc[(parse-dbc [text string?]) dbc?]{
 Parses a full DBC file.  Unknown entry kinds are preserved as
 @racket[entry-unparsed] values rather than rejected, so files with
 vendor-specific sections still round-trip.
}

@defproc[(serialize-dbc [db dbc?]) string?]{
 Renders a @racket[dbc?] back to text.  The round-trip guarantee:
 @racket[parse-dbc] of the result is @racket[equal?] to the original value
 (whitespace is normalized; string escapes are re-applied).
}

@defstruct*[dbc ([version (or/c string? #f)] [entries list?])]{
 The parsed file: the @racket[VERSION] string (or @racket[#f]) and the
 ordered entry stream.  Entries are instances of the @racket[entry-*]
 structs: @racket[entry-version], @racket[entry-ns], @racket[entry-bs],
 @racket[entry-bu], @racket[entry-val-table], @racket[entry-bo],
 @racket[entry-cm], @racket[entry-ba-def], @racket[entry-ba-def-def],
 @racket[entry-ba], @racket[entry-val], @racket[entry-sig-valtype],
 @racket[entry-sig-mul-val], @racket[entry-ev] and @racket[entry-unparsed].
}

@defstruct*[message ([id exact-integer?] [extended? boolean?]
                     [name symbol?] [dlc exact-nonnegative-integer?]
                     [transmitter (or/c symbol? #f)]
                     [signals (listof signal?)])]{
 One @racket[BO_]-declared message.  @racket[id] has the
 @tt{0x80000000}-extended flag stripped (see @racket[message-extended?]).
}

@defstruct*[signal ([name symbol?]
                    [mux (or/c #f (integer-in 0 65535) 'multiplexor)]
                    [mux-ranges list?]
                    [start-bit (integer-in 0 511)]
                    [bit-count (integer-in 1 64)]
                    [little-endian? boolean?]
                    [signed? boolean?]
                    [scale rational?] [offset rational?]
                    [minimum (or/c rational? #f)]
                    [maximum (or/c rational? #f)]
                    [unit (or/c string? #f)]
                    [receivers (listof symbol?)]
                    [float-kind (or/c #f 'ieee)]
                    [choices (or/c hash? #f)]
                    [comment (or/c string? #f)])]{
 One @racket[SG_]-declared signal.  @racket[mux] is @racket[#f] for a plain
 signal, @racket['multiplexor] for the @tt{M} switch and an integer for an
 @tt{mN} variant; @racket[mux-ranges] holds the @racket[SG_MUL_VAL_]
 extended-multiplexing ranges as @racket[(cons lo hi)] pairs.  @racket[scale]
 and @racket[offset] are exact rationals.  @racket[choices] maps raw values
 to strings (from @racket[VAL_] / @racket[VAL_TABLE_]).
}

@section{Message codec}

@defproc[(message-decode [msg message?] [payload bytes?]
                         [#:decode-choices? decode-choices? boolean? #f])
         (hash/c symbol? (or/c exact-integer? rational? real? string?))]{
 Decodes @racket[payload] as @racket[msg].  Returns a hash keyed by signal
 name.  Values are exact integers when the signal's scale is 1 and its
 offset is 0, inexact reals otherwise, IEEE floats for float signals, and —
 with @racket[#:decode-choices?] — the choice strings for values present in
 the signal's value table.  Only signals active under the current
 multiplexor value are included.  Signals are scaled with the exact
 rational arithmetic of @racket[scale]/@racket[offset] before the final
 conversion.
}

@defproc[(message-encode [msg message?]
                         [values (hash/c symbol? real?)]
                         [#:base base (or/c bytes? #f) #f]
                         [#:dlc dlc (or/c #f exact-nonnegative-integer?) #f])
         bytes?]{
 Encodes @racket[values] (physical, scaled values) into a DLC-sized byte
 string.  Signals absent from @racket[values] stay at the buffer's existing
 bits (@racket[#:base]) or zero.  The multiplexor switch, when present in
 @racket[values], is written first so muxed signals see the correct switch
 value regardless of hash iteration order.
}

@defproc[(signal-decode [sig signal?] [payload bytes?]) (or/c exact-integer? rational? real?)]{
 Decodes a single signal, ignoring multiplexing.
}

@defproc[(signal-encode [sig signal?] [payload bytes?] [value real?]) bytes?]{
 Encodes a single signal into @racket[payload] (mutated and returned).
 Rounding is half-to-even, matching Python's @racket[round] and therefore
 @tt{cantools}.
}

@defproc[(message-signal [msg message?] [name symbol?]) (or/c signal? #f)]{
 The first signal named @racket[name], or @racket[#f].
}

@defproc[(signal-active? [sig signal?] [switch (or/c exact-integer? #f)]) boolean?]{
 Would @racket[sig] be present in a frame whose multiplexor value is
 @racket[switch]?  Plain muxed signals match on their @tt{mN} value;
 extended-mux signals (with @racket[SG_MUL_VAL_] ranges) match on @tt{mN}
 or when the switch falls inside a range.
}

@section{Tokenizer}

@defproc[(tokenize [text string?]) (listof token?)]{
 Splits DBC text into @racket[token]s.  DBC has no comments; a @tt{;}
 terminates an entry.  Quoted strings may span lines and support @tt{\"}
 and @tt{\\} escapes.
}

@defstruct*[token ([kind (or/c 'word 'string 'punct)] [text string?])]{
 A @racket['word] is a bare identifier or number (signs fold into
 negative-number words only when not glued to a preceding word, so
 @tt{100-200} of @racket[SG_MUL_VAL_] splits correctly); a
 @racket['string] is a resolved quoted string; a @racket['punct] is one of
 @tt{( ) ; : | , [ ] + -} plus the at-sign.
}

@section{Limitations}

@itemlist[
 @item{Attribute definitions and values (@racket[BA_DEF_], @racket[BA_]) are
  parsed and round-tripped, but attribute semantics are not interpreted.}
 @item{The @racket[EV_] (environment variable), @racket[SIG_GROUP_] and
  @racket[SGTYPE_] families are kept as opaque entries (parsed structurally
  where the grammar is regular, @racket[entry-unparsed] otherwise).}
 @item{J1939-specific conventions (PGN packing inside the 29-bit ID) are out
  of scope; the extended-ID bit is exposed via @racket[message-extended?].}
]

@section{License}

MIT.  The test corpus includes sample files from the @tt{cantools} project
(MIT).
