#lang racket/base

;; Test suite for the dbc package.  Structural goldens come from the public
;; cantools test files (MIT, embedded under test/data/); decode/encode
;; vectors were generated with cantools 40.7.1 and are pinned here as the
;; second implementation for cross-checking.

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         racket/match
         dbc/model
         dbc/tokenizer
         dbc/parser
         dbc/serializer
         dbc/signal)

(define-runtime-path data-dir "data")

(define (load-sample name)
  (parse-dbc (file->string (build-path data-dir name))))

(define (find-message db name)
  (for/first ([e (in-list (dbc-entries db))]
              #:when (and (entry-bo? e)
                          (eq? (message-name (entry-bo-message e)) name)))
    (entry-bo-message e)))

;; ---------------------------------------------------------------------------
;; tokenizer

(test-case "tokenizer basics"
  (define toks (tokenize "BO_ 496 Name: 8 PCM1\n SG_ Sig : 0|12@0+ (0.01,250) \"a\\\"b\" X,Y"))
  (check-equal? (token-text (list-ref toks 0)) "BO_")
  (check-equal? (token-text (list-ref toks 1)) "496")
  ;; the quoted string keeps the escaped quote, resolved
  (check-true (token-string? (list-ref toks 20)))
  (check-equal? (token-text (list-ref toks 20)) "a\"b")
  (check-true (token-punct? (list-ref toks 10) #\|))
  ;; signs: negative numbers fold, separators stay punct
  (check-equal? (map token-text (tokenize "-5 -128 1- 100-200"))
                '("-5" "-128" "1" "-" "100" "-" "200")))

(test-case "tokenizer multi-line string"
  (define toks (tokenize "CM_ BO_ 1 \"line one\nline two\";"))
  (check-true (token-string? (list-ref toks 3)))
  (check-equal? (token-text (list-ref toks 3)) "line one\nline two"))

;; ---------------------------------------------------------------------------
;; exact decimals

(test-case "decimal conversion stays exact"
  (check-equal? (decimal->exact "0.01") 1/100)
  (check-equal? (decimal->exact "-0.125") -1/8)
  (check-equal? (decimal->exact "5") 5)
  (check-equal? (exact-rational->string 1/100) "0.01")
  (check-equal? (exact-rational->string -1/8) "-0.125")
  (check-equal? (exact-rational->string 5) "5")
  (check-equal? (exact-rational->string 0) "0"))

;; ---------------------------------------------------------------------------
;; parsing: motohawk.dbc (Motorola, signed, attributes, comments)

(define motohawk (load-sample "motohawk.dbc"))

(test-case "parse motohawk.dbc"
  (define m (find-message motohawk 'ExampleMessage))
  (check-pred message? m)
  (check-equal? (message-id m) 496)
  (check-false (message-extended? m))
  (check-equal? (message-dlc m) 8)
  (check-equal? (message-transmitter m) 'PCM1)
  (check-equal? (length (message-signals m)) 3)
  (define temp (message-signal m 'Temperature))
  (check-equal? (signal-start-bit temp) 0)
  (check-equal? (signal-bit-count temp) 12)
  (check-false (signal-little-endian? temp))    ; @0 Motorola
  (check-true (signal-signed? temp))
  (check-equal? (signal-scale temp) 1/100)
  (check-equal? (signal-offset temp) 250)
  (check-equal? (signal-unit temp) "degK")
  (check-equal? (signal-receivers temp) '(PCM1 FOO))
  ;; message comment arrived via CM_ BO_ 496
  (define cm (for/first ([e (in-list (dbc-entries motohawk))]
                         #:when (and (entry-cm? e) (equal? (entry-cm-target e) (list 'bo 496))))
               (entry-cm-comment e)))
  (check-equal? cm "Example message used as template in MotoHawk models."))

;; ---------------------------------------------------------------------------
;; cantools cross-check: encode/decode on motohawk ExampleMessage

(test-case "cantools cross-check: ExampleMessage encode"
  (define m (find-message motohawk 'ExampleMessage))
  ;; cantools 40.7.1: encode({'Temperature': 250.47, 'AverageRadius': 1.5,
  ;;                          'Enable': 1}) -> 9e05e00000000000
  (check-equal? (message-encode m (hash 'Temperature 250.47
                                        'AverageRadius 1.5
                                        'Enable 1))
                (bytes #x9e #x05 #xe0 #x00 #x00 #x00 #x00 #x00)))

(test-case "cantools cross-check: ExampleMessage decode"
  (define m (find-message motohawk 'ExampleMessage))
  ;; cantools: decode(bytes.fromhex('01b0') + zeros) ->
  ;;   Enable 'Disabled', AverageRadius 0.0, Temperature 243.6
  (define d (message-decode m (bytes #x01 #xb0 0 0 0 0 0 0)))
  (check-equal? (hash-ref d 'Temperature) 243.6)
  (check-equal? (hash-ref d 'AverageRadius) 0.0)
  (check-equal? (hash-ref d 'Enable) 0)
  ;; with choices decoded, Enable becomes its text (VAL_ table in the file)
  (define d2 (message-decode m (bytes #x01 #xb0 0 0 0 0 0 0) #:decode-choices? #t))
  (check-equal? (hash-ref d2 'Enable) "Disabled"))

(test-case "motorola sawtooth walk (anchored on cantools 40.x)"
  ;; Temperature: Motorola 0|12@0- signed.  cantools single-bit probes:
  ;; raw bit k -> (byte,pos): 0->(2,5) 1->(2,6) 2->(2,7) 3->(1,0) ...
  ;; 10->(1,7) 11->(0,0);  decode(01 b0) raw -640;  decode(00 05 e0) raw 47.
  (define m (find-message motohawk 'ExampleMessage))
  (define temp (message-signal m 'Temperature))
  (check-equal? (signal-raw-extract temp (bytes #x01 #xb0 0 0 0 0 0 0)) -640)
  (check-equal? (signal-raw-extract temp (bytes 0 #x05 #xe0 0 0 0 0 0)) 47)
  (check-equal? (signal-raw-extract temp (bytes #x9e #x05 #xe0 0 0 0 0 0)) 47)
  ;; insert/extract round-trip of every single-bit raw value (the signal
  ;; is signed, so the top bit reads back negative)
  (for ([k (in-range 12)])
    (define payload (make-bytes 8 0))
    (define out (signal-raw-insert temp payload (arithmetic-shift 1 k)))
    (check-equal? (signal-raw-extract temp out)
                  (if (= k 11) -2048 (arithmetic-shift 1 k)))))

;; ---------------------------------------------------------------------------
;; abs.dbc: Intel signals, VAL_TABLE_, messages without signals

(define abs-db (load-sample "abs.dbc"))

(test-case "cantools cross-check: BREMSE_33 Intel decode"
  (define m (find-message abs-db 'BREMSE_33))
  (check-pred message? m)
  ;; cantools: decode(08 00 10 00 20 00 30 00) ->
  ;;   FL 0.125, FR 0.25, RL 0.5, RR 0.75
  (define d (message-decode m (bytes #x08 #x00 #x10 #x00 #x20 #x00 #x30 #x00)))
  (check-equal? (hash-ref d 'whlspeed_FL) 0.125)
  (check-equal? (hash-ref d 'whlspeed_FR) 0.25)
  (check-equal? (hash-ref d 'whlspeed_RL) 0.5)
  (check-equal? (hash-ref d 'whlspeed_RR) 0.75)
  ;; and the encode side agrees byte for byte
  (check-equal? (message-encode m d) (bytes #x08 #x00 #x10 #x00 #x20 #x00 #x30 #x00)))

(test-case "messages without signals and VAL_TABLE_"
  (define m (find-message abs-db 'BREMSE_10))
  (check-pred message? m)
  (check-equal? (message-signals m) '())
  (define vt (for/first ([e (in-list (dbc-entries abs-db))]
                         #:when (and (entry-val-table? e)
                                     (eq? (entry-val-table-name e) 'ABS_fault_info)))
               e))
  (check-equal? (entry-val-table-pairs vt)
                (list (cons 2 "active faults stored")
                      (cons 1 "inactive faults stored")
                      (cons 0 "no faults stored"))))

;; ---------------------------------------------------------------------------
;; choices.dbc: VAL_ value tables

(define choices-db (load-sample "choices.dbc"))

(test-case "cantools cross-check: VAL_ choices"
  (define m (find-message choices-db 'Foo))
  (check-pred message? m)
  (check-equal? (message-id m) 0)
  (define sig (message-signal m 'Foo))
  (define choices (signal-choices sig))
  (check-equal? (hash-ref choices 0) "With space")
  (check-equal? (hash-ref choices -5) "A negative value")
  (check-equal? (hash-ref choices 6) "reserved")
  ;; cantools: decode(bytes(6) padded) -> 'reserved'
  (check-equal? (hash-ref (message-decode m (bytes 6 0 0 0 0 0 0 0)
                                          #:decode-choices? #t)
                          'Foo)
                "reserved")
  (check-equal? (hash-ref (message-decode m (bytes 6 0 0 0 0 0 0 0)) 'Foo) 6))

;; ---------------------------------------------------------------------------
;; floating_point.dbc: SIG_VALTYPE_

(define fp-db (load-sample "floating_point.dbc"))

(test-case "cantools cross-check: IEEE float signals"
  ;; Message1 Signal1: 64 bits, SIG_VALTYPE_ : 2 -> double
  (define m1 (find-message fp-db 'Message1))
  (check-equal? (message-id m1) 1024)
  (define s1 (message-signal m1 'Signal1))
  (check-equal? (signal-float-kind s1) 'ieee)
  (check-equal? (signal-bit-count s1) 64)
  ;; cantools: decode(CD CC CC 3D 9A 99 99 3E ...) -> 3.814698724013965e-07
  (check-equal? (signal-decode s1 (bytes #xCD #xCC #xCC #x3D #x9A #x99 #x99 #x3E))
                3.814698724013965e-07)
  ;; Message2 Signal1: 32 bits, SIG_VALTYPE_ : 1 -> float32
  (define m2 (find-message fp-db 'Message2))
  (define m2s1 (message-signal m2 'Signal1))
  (check-equal? (signal-float-kind m2s1) 'ieee)
  (check-equal? (signal-bit-count m2s1) 32)
  ;; cantools: encode({'Signal1': 0.5, 'Signal2': 0.25}) -> 0000003f0000803e
  (check-equal? (message-encode m2 (hash 'Signal1 0.5 'Signal2 0.25))
                (bytes #x00 #x00 #x00 #x3f #x00 #x00 #x80 #x3e)))

;; ---------------------------------------------------------------------------
;; attributes.dbc: BA_DEF_ / BA_DEF_DEF_ / BA_

(define attrs-db (load-sample "attributes.dbc"))

(test-case "attributes"
  (define ba-defs (for/list ([e (in-list (dbc-entries attrs-db))]
                             #:when (entry-ba-def? e))
                    e))
  (check-pred pair? ba-defs)
  (define gen-msg-cycle
    (for/first ([e ba-defs] #:when (equal? (entry-ba-def-name e) "GenMsgCycleTime"))
      e))
  (check-equal? (entry-ba-def-object gen-msg-cycle) 'bo)
  (check-equal? (entry-ba-def-kind gen-msg-cycle) 'int)
  (define bas (for/list ([e (in-list (dbc-entries attrs-db))]
                         #:when (entry-ba? e))
                e))
  (check-pred pair? bas)
  ;; TheMessage (id 57) message with an applied attribute value
  (define m (find-message attrs-db 'TheMessage))
  (check-equal? (message-id m) 57))

;; ---------------------------------------------------------------------------
;; round trip

(test-case "round trip: parse(serialize(parse(x))) == parse(x)"
  (for ([name (in-list '("motohawk.dbc" "abs.dbc" "choices.dbc"
                         "floating_point.dbc" "attributes.dbc"))])
    (define db (load-sample name))
    (define text (serialize-dbc db))
    (define db2 (parse-dbc text))
    (check-equal? db2 db (format "round trip diverged for ~a" name))))

(test-case "round trip of a hand-built file with extended mux and floats"
  (define text #<<EOF
VERSION "1.0"


NS_ : 
	NS_DESC_
	CM_
	BA_DEF_
	BA_
	VAL_
	BA_DEF_DEF_

BS_:

BU_: ECM DASH


BO_ 234 Multiplexed: 8 ECM
 SG_ Switch M : 0|8@1+ (1,0) [0|255] "" DASH
 SG_ Level m1 : 8|16@1+ (0.1,0) [0|100] "ltr" DASH
 SG_ Plain : 24|32@1+ (1,0) [0|255] "" DASH

SG_MUL_VAL_ 234 Level Switch 100-200;

CM_ SG_ 234 Level "Fuel level in litres or counts";

VAL_ 234 Switch 2 "two" 1 "one" ;

BA_DEF_ BO_  "GenMsgCycleTime" INT 0 65535;
BA_DEF_DEF_  "GenMsgCycleTime" 0;
BA_ "GenMsgCycleTime" BO_ 234 100;

SIG_VALTYPE_ 234 Plain : 1;

EV_ EnvCounter: 0 [0|0] "" 0 1 DUMMY_NODE_VECTOR0 Vector__XXX;
EOF
    )
    (define db (parse-dbc text))
    ;; switch = 1 -> Level m1 active with scale 0.1
    (define m (find-message db 'Multiplexed))
    (check-equal? (message-id m) 234)
    (define encoded (message-encode m (hash 'Switch 1 'Level 5 'Plain 7)))
    (define d (message-decode m encoded))
    (check-equal? (hash-ref d 'Switch) 1)
    (check-= (hash-ref d 'Level) 5.0 1e-9)
    (check-equal? (hash-ref d 'Plain) 7.0)
    ;; extended mux ranges: Switch 150 activates Level m1 (100-200)
    (define encoded2 (message-encode m (hash 'Switch 150 'Level 3 'Plain 9)))
    (check-= (hash-ref (message-decode m encoded2) 'Level) 3.0 1e-9)
    ;; switch 250: no Level variant active
    (define encoded3 (message-encode m (hash 'Switch 250 'Plain 9)))
    (check-false (hash-has-key? (message-decode m encoded3) 'Level))
    ;; float signal: Plain declared double via SIG_VALTYPE_
    (check-equal? (signal-float-kind (message-signal m 'Plain)) 'ieee)
    (check-equal? (hash-ref (message-decode m encoded) 'Plain) 7.0)
    ;; choices attached
    (check-equal? (signal-choices (message-signal m 'Switch))
                  (hash 2 "two" 1 "one"))
    ;; signal comment attached
    (check-equal? (signal-comment (message-signal m 'Level))
                  "Fuel level in litres or counts")
    ;; extended mux ranges survive
    (check-equal? (signal-mux-ranges (message-signal m 'Level)) (list (cons 100 200)))
    ;; cycle time attribute
    (define ba (for/first ([e (in-list (dbc-entries db))]
                           #:when (and (entry-ba? e)
                                       (equal? (entry-ba-name e) "GenMsgCycleTime")))
                 e))
    (check-equal? (entry-ba-target ba) (list 'bo 234))
    (check-equal? (entry-ba-value ba) 100)
    ;; env var
    (define ev (for/first ([e (in-list (dbc-entries db))] #:when (entry-ev? e)) e))
    (check-equal? (entry-ev-name ev) 'EnvCounter)
    ;; full round trip
    (define db2 (parse-dbc (serialize-dbc db)))
    (check-equal? db2 db))
