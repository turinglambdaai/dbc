#lang info

(define version "0.1")
;; Multi-collection package shape: the collection is the `dbc/` subdirectory.
;; `collection 'multi` is the standard declaration for this layout.
(define collection 'multi)
;; rackunit-lib is a run-time dependency (not merely a build-time one)
;; because the package ships its test suite, and the catalog's dependency
;; check scans shipped modules in run mode.
(define deps '("base" "rackunit-lib"))
(define build-deps '("base" "scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Pure-Racket Vector DBC (CAN database) reader/writer: full grammar, signal codec for Intel and Motorola byte order, multiplexing, round-trip serialization")
(define pkg-authors '("turlamai"))
(define license 'MIT)
;; Documentation is declared in dbc/info.rkt (collection level) — `raco
;; setup` only discovers scribblings from a collection's own info.rkt for
;; `collection 'multi` packages.
(define test-omit-paths '("dbc/scribblings"))
