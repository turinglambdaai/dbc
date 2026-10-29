# dbc — Vector DBC (CAN database) in pure Racket

**English** · [中文](README.zh-CN.md)

[![CI](https://github.com/turlamai/dbc/actions/workflows/ci.yml/badge.svg)](https://github.com/turlamai/dbc/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

`dbc` reads and writes **DBC files** — the de-facto-standard CAN database
format invented by Vector Informatik and consumed by virtually every CAN tool
in the automotive industry.  A DBC file describes the symbolic layer of a CAN
bus: message and signal names, bit layouts, scaling, value tables, comments
and attributes.

Everything is implemented in pure Racket: no FFI, no system dependency.  The
signal codec is validated bit-for-bit against Python's
[cantools](https://github.com/cantools/cantools) on its own test corpus
(its sample files ship as test data).

## Installation

```
raco pkg install dbc
```

## Usage

A parsed file is a `dbc?` value holding two views:

- the **entry stream** — `dbc-entries` keeps every grammar element
  (`BO_`, `CM_`, `BA_DEF_`, `VAL_`, …) in source order, which is what makes
  lossless serialization possible (`serialize-dbc` round-trips
  `equal?`-exactly, including unknown vendor sections kept as
  `entry-unparsed`);
- the **codec view** — each `entry-bo` wraps a `message?` whose `signal?`s
  carry attached choices/comments/float kinds, ready for
  `message-decode` / `message-encode`.

```racket
#lang racket/base
(require dbc)

;; A self-contained synthetic Vector DBC; no external .dbc file needed.
(define db (parse-dbc #<<DBC
VERSION "1.0"

BO_ 37 ExampleMessage: 8 Vector__XXX
 SG_ VehicleSpeed : 39|16@1+ (0.01,0) [-100|100] "kph" Vector__XXX
 SG_ Temperature : 0|12@0- (0.01,250) [-40|215] "degC" Vector__XXX
 SG_ DriveMode : 56|8@1+ (1,0) [0|1] "" Vector__XXX

VAL_ 37 DriveMode 0 "parked" 1 "active" ;
DBC
))
(define msg
  (for/first ([e (in-list (dbc-entries db))] #:when (entry-bo? e))
    (entry-bo-message e)))

(message-name msg) ; => 'ExampleMessage
(message-decode msg (make-bytes 8 0) #:decode-choices? #t)
;; DriveMode is "parked" according to VAL_; scaled VehicleSpeed stays numeric.
(define payload
  (message-encode msg (hash 'Temperature 250.47 'VehicleSpeed 0.5)))
(message-decode msg payload)
(equal? (parse-dbc (serialize-dbc db)) db) ; => #t
```

### Bit numbering

A payload bit `n` lives in byte `(quotient n 8)` at position
`(remainder n 8)` from the LSB.

- **Intel** (`@1`, little-endian): the value's LSB sits at the start bit;
  bits `start … start+len-1`, accumulated LSB-first.
- **Motorola** (`@0`, big-endian): the value's MSB sits at the start bit;
  the remaining bits walk the sawtooth — from a byte's LSB the walk jumps
  to the next byte's MSB (+15), otherwise it descends one position (-1) —
  accumulated MSB-first.  This is the placement cantools produces
  (verified bit by bit against its probes).

### Multiplexing

Plain multiplexing (`M` switch, `mN` variants) and extended multiplexing
(`SG_MUL_VAL_` ranges) are both decoded and encoded.  Same-name muxed
variants are kept as separate signals; only the variants active under the
current switch value appear in a decode result.

## Reference, checked examples and tests

The [Scribble API manual](dbc/scribblings/dbc.scrbl) describes the entry
model, lossless serialization, Intel/Motorola bit layouts, scaled signal
encoding and choice tables (unscaled integer DriveMode); its synthetic CAN DBC example is evaluated during
documentation generation. A separate [executable quickstart](examples/quickstart.rkt)
checks message lookup, frame encoding/decoding, VAL_ choices and DBC
serialization without hardware, file downloads or proprietary databases.

From a local checkout (Racket installed):

```sh
raco pkg install --auto --link "$(pwd)"
raco test -p dbc
racket examples/quickstart.rkt
raco setup --check-pkg-deps -l dbc
```

The Linux/macOS/Windows CI matrix verifies all these checks. The pure
Racket `dbc` collection is inside the multi-collection package; parsing a
DBC is not itself a live CAN adapter.

## Limitations

- Attributes (`BA_DEF_` / `BA_`) are parsed and round-tripped, but their
  semantics are not interpreted.
- Environment variables (`EV_`), `SIG_GROUP_` and `SGTYPE_` families are
  kept structurally (opaque entries where the grammar is irregular).
- J1939 PGN conventions are out of scope; the extended-ID bit is exposed
  via `message-extended?`.

## License

[MIT](LICENSE).  The test corpus includes sample files from the
[cantools](https://github.com/cantools/cantools) project (MIT).
