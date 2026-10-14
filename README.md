# dbc — Vector DBC (CAN database) in pure Racket

[![CI](https://github.com/turinglambdaai/dbc/actions/workflows/ci.yml/badge.svg)](https://github.com/turinglambdaai/dbc/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

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
(require dbc)

(define db (parse-dbc text))
(define msg (for/first ([e (in-list (dbc-entries db))] #:when (entry-bo? e))
              (entry-bo-message e)))

(message-decode msg (bytes #x01 #xb0 0 0 0 0 0 0))
;; '#hash((Temperature . 243.6) (AverageRadius . 0.0) (Enable . 0))

(message-decode msg (bytes #x01 #xb0 0 0 0 0 0 0) #:decode-choices? #t)
;; Enable becomes "Disabled" via its VAL_ value table

(message-encode msg (hash 'Temperature 250.47 'AverageRadius 1.5 'Enable 1))
;; #"\236\5\340\0\0\0\0\0" — byte-identical to cantools
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
