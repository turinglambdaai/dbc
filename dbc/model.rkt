#lang racket/base

;; Data model of a parsed DBC file.  A `dbc` keeps the ordered stream of
;; entries so serialization can round-trip; signal/message structs carry the
;; codec-relevant facts (and are enriched with choices, comments and float
;; kinds after parsing).

(require racket/contract
         racket/list)

(provide (contract-out
          (struct dbc ((version (or/c string? #f))
                       (entries list?)))
          (struct entry-version ((text string?)))
          (struct entry-ns ((symbols (listof symbol?))))
          (struct entry-bs ())
          (struct entry-bu ((nodes (listof symbol?))))
          (struct entry-val-table ((name symbol?)
                                   (pairs list?)))
          (struct entry-bo ((message message?)))
          (struct entry-cm ((target list?)
                            (comment string?)))
          (struct entry-ba-def ((object (or/c symbol? #f))
                                (name string?)
                                (kind (or/c 'int 'hex 'float 'string 'enum))
                                (minimum (or/c rational? #f))
                                (maximum (or/c rational? #f))
                                (enums list?)))
          (struct entry-ba-def-def ((name string?) (value (or/c rational? string? #f))))
          (struct entry-ba ((name string?)
                            (target (or/c #f list?))
                            (value (or/c rational? string? #f))))
          (struct entry-val ((id (or/c exact-integer? symbol?))
                             (signal-name (or/c symbol? #f))
                             (pairs list?)))
          (struct entry-sig-valtype ((id exact-integer?)
                                     (signal-name symbol?)
                                     (kind (or/c 'float 'double))))
          (struct entry-sig-mul-val ((id exact-integer?)
                                     (signal-name symbol?)
                                     (switch symbol?)
                                     (ranges list?)))
          (struct entry-ev ((name symbol?)
                            (type exact-integer?)
                            (minimum rational?)
                            (maximum rational?)
                            (unit string?)
                            (persistence exact-integer?)
                            (callback-type exact-integer?)
                            (access-type symbol?)
                            (access-nodes (listof symbol?))))
          (struct entry-unparsed ((text string?)))
          (struct message ((id exact-integer?)
                           (extended? boolean?)
                           (name symbol?)
                           (dlc exact-nonnegative-integer?)
                           (transmitter (or/c symbol? #f))
                           (signals (listof signal?))))
          (struct signal ((name symbol?)
                          (mux (or/c #f (integer-in 0 65535) 'multiplexor))
                          (mux-ranges list?)   ; extended multiplexing ranges
                          (start-bit (integer-in 0 511))
                          (bit-count (integer-in 1 64))
                          (little-endian? boolean?)
                          (signed? boolean?)
                          (scale rational?)
                          (offset rational?)
                          (minimum (or/c rational? #f))
                          (maximum (or/c rational? #f))
                          (unit (or/c string? #f))
                          (receivers (listof symbol?))
                          (float-kind (or/c #f 'ieee))
                          (choices (or/c hash? #f))
                          (comment (or/c string? #f))))
          [signal-with (-> signal? #:choices (or/c hash? #f) signal?)]
          [signal-with-float (-> signal? (or/c #f 'ieee) signal?)]
          [signal-with-comment (-> signal? string? signal?)]
          [message-with-signal (-> message? symbol? signal? message?)]))

(struct dbc (version entries) #:transparent)

(struct entry-version (text) #:transparent)
(struct entry-ns (symbols) #:transparent)
(struct entry-bs () #:transparent)
(struct entry-bu (nodes) #:transparent)
(struct entry-val-table (name pairs) #:transparent)         ; pairs: (list num string)
(struct entry-bo (message) #:transparent)
(struct entry-cm (target comment) #:transparent)            ; target: '(bo id) '(sg id name) '(bu name) '(ev name)
(struct entry-ba-def (object name kind minimum maximum enums) #:transparent)
(struct entry-ba-def-def (name value) #:transparent)
(struct entry-ba (name target value) #:transparent)         ; target: '(bo id) '(sg id name) '(bu name) '(ev name) or #f
(struct entry-val (id signal-name pairs) #:transparent)
(struct entry-sig-valtype (id signal-name kind) #:transparent)
(struct entry-sig-mul-val (id signal-name switch ranges) #:transparent) ; ranges: (list (cons lo hi))
(struct entry-ev (name type minimum maximum unit persistence callback-type access-type access-nodes) #:transparent)
(struct entry-unparsed (text) #:transparent)

(struct message (id extended? name dlc transmitter signals) #:transparent)

(struct signal (name
                mux            ; #f plain, 'multiplexor for M, or a number for mN
                mux-ranges     ; extended multiplexing: (list (cons lo hi))
                start-bit
                bit-count
                little-endian? ; @1 Intel, @0 Motorola
                signed?
                scale          ; exact rational
                offset         ; exact rational
                minimum
                maximum
                unit
                receivers
                float-kind     ; #f, 'float (32-bit), 'double (64-bit)
                choices        ; hash raw-value -> string, or #f
                comment)
  #:transparent)

;; Functional update helpers used by the parser when it attaches later
;; sections (VAL_, CM_, SIG_VALTYPE_) to signals.
(define (signal-with s #:choices choices)
  (struct-copy signal s [choices choices]))

(define (signal-with-float s kind)
  (struct-copy signal s [float-kind kind]))

(define (signal-with-comment s comment)
  (struct-copy signal s [comment comment]))

(define (message-with-signal m name new-signal)
  ;; DBC allows same-name muxed variants (m1/m2); only an exact match on
  ;; name AND multiplexor identity replaces, otherwise the variant appends.
  (define sigs (message-signals m))
  (define idx
    (index-where sigs
                 (lambda (s)
                   (and (eq? (signal-name s) name)
                        (equal? (signal-mux s) (signal-mux new-signal))))))
  (struct-copy message m
               [signals (if idx
                            (list-set sigs idx new-signal)
                            (append sigs (list new-signal)))]))

;; Comments on messages live in entry-cm entries; nothing to change on the
;; message struct itself, so this is intentionally absent.
