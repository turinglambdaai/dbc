#lang racket/base

;; DBC parser: tokens (tokenizer.rkt) -> ordered `dbc` entries.  Tolerant of
;; the format's dialects (optional object types, empty sections, comments
;; spanning lines); unrecognized entries are kept verbatim for round-tripping.

(require racket/contract
         racket/list
         racket/match
         racket/string
         "model.rkt"
         "tokenizer.rkt")

(provide (contract-out
          [parse-dbc (-> string? dbc?)]
          [decimal->exact (-> string? rational?)]
          [word->number (-> string? (or/c exact-integer? rational? #f))]
          [dbc-symbol (-> string? symbol?)]))

;; ---------------------------------------------------------------------------
;; token stream cursor

(struct cursor (tokens pos))

(define (cur c) (list-ref (cursor-tokens c) (cursor-pos c)))
(define (advance c) (cursor (cursor-tokens c) (add1 (cursor-pos c))))
(define (at-end? c) (>= (cursor-pos c) (length (cursor-tokens c))))

(define (peek-punct? c ch)
  (and (not (at-end? c)) (token-punct? (cur c) ch)))

(define (peek-word? c)
  (and (not (at-end? c)) (token-word? (cur c))))

(define (peek-string? c)
  (and (not (at-end? c)) (token-string? (cur c))))

(define (expect-punct c ch)
  (unless (peek-punct? c ch)
    (error 'parse-dbc "expected ~a, got ~a"
           ch (if (at-end? c) "end of input" (token-text (cur c)))))
  (advance c))

(define (expect-word c)
  (unless (peek-word? c)
    (error 'parse-dbc "expected a word, got ~a"
           (if (at-end? c) "end of input" (token-text (cur c)))))
  (define t (cur c))
  (values (token-text t) (advance c)))

(define (expect-string c)
  (unless (peek-string? c)
    (error 'parse-dbc "expected a quoted string, got ~a"
           (if (at-end? c) "end of input" (token-text (cur c)))))
  (define t (cur c))
  (values (token-text t) (advance c)))

(define (dbc-symbol s) (string->symbol s))

;; ---------------------------------------------------------------------------
;; numbers: keep everything exact

(define (decimal->exact s)
  (define m (regexp-match #px"^([+-]?)(\\d*)(?:\\.(\\d*))?$" s))
  (unless m
    (error 'decimal->exact "not a decimal number: ~a" s))
  (define sign (if (equal? (list-ref m 1) "-") -1 1))
  (define int-part (if (equal? (list-ref m 2) "") "0" (list-ref m 2)))
  (define frac-part (or (list-ref m 3) ""))
  (if (equal? frac-part "")
      (* sign (string->number int-part))
      (* sign
         (/ (+ (* (string->number int-part) (expt 10 (string-length frac-part)))
               (string->number frac-part))
            (expt 10 (string-length frac-part))))))

(define (word->number s)
  (cond [(regexp-match? #px"^[+-]?\\d+$" s) (string->number s)]
        [(regexp-match? #px"^[+-]?\\d*\\.\\d+$" s) (decimal->exact s)]
        [else #f]))

(define (token->value t)
  (match (token-kind t)
    ['string (token-text t)]
    ['word (word->number (token-text t))]
    [_ #f]))

(define (expect-number c)
  (unless (peek-word? c)
    (error 'parse-dbc "expected a number, got ~a"
           (if (at-end? c) "end of input" (token-text (cur c)))))
  (define n (word->number (token-text (cur c))))
  (unless n (error 'parse-dbc "expected a number, got ~a" (token-text (cur c))))
  (values n (advance c)))

;; ---------------------------------------------------------------------------
;; SG_ entry (after the SG_ keyword)

(define (parse-signal-entry c)
  (define-values (name c1) (expect-word c))
  ;; optional mux indicator: M, mN, mNM, NM (extended multiplexor switch)
  (define-values (mux mux-ranges c2)
    (cond
      [(peek-word? c1)
       (define w (token-text (cur c1)))
       (cond
         [(equal? w "M")
          (values 'multiplexor '() (advance c1))]
         [(regexp-match #px"^(\\d+)M$" w)
          (values 'multiplexor '() (advance c1))]
         [(regexp-match #px"^m(\\d+)$" w)
          (values (string->number (second (regexp-match #px"^m(\\d+)$" w)))
                  '() (advance c1))]
         [(regexp-match #px"^m(\\d+)M$" w)
          (values (string->number (second (regexp-match #px"^m(\\d+)M$" w)))
                  '() (advance c1))]
         [else (values #f '() c1)])]
      [else (values #f '() c1)]))
  (define c3 (expect-punct c2 #\:))
  (define-values (start-text c4) (expect-word c3))
  (define c5 (expect-punct c4 #\|))
  (define-values (len-text c6) (expect-word c5))
  (define c7 (expect-punct c6 #\@))
  ;; byte order, then sign glued ("1-") or standalone ("+", "-")
  (define-values (little-endian? signed? c8)
    (cond
      [(and (peek-word? c7) (regexp-match? #px"^[01][+-]$" (token-text (cur c7))))
       (define m (regexp-match #px"^([01])([+-])$" (token-text (cur c7))))
       (values (equal? (second m) "1") (equal? (third m) "-") (advance c7))]
      [(and (peek-word? c7) (regexp-match? #px"^[01]$" (token-text (cur c7))))
       (define little (equal? (token-text (cur c7)) "1"))
       (define c* (advance c7))
       (define sign
         (cond
           [(peek-word? c*) (token-text (cur c*))]
           [(peek-punct? c* #\+) "+"]
           [(peek-punct? c* #\-) "-"]
           [else (error 'parse-dbc "expected sign in signal ~a" name)]))
       (unless (member sign '("+" "-"))
         (error 'parse-dbc "expected + or - in signal ~a, got ~a" name sign))
       (values little (equal? sign "-") (advance c*))]
      [else (error 'parse-dbc "malformed byte order in signal ~a" name)]))
  (define c9 (expect-punct c8 #\())
  (define-values (scale-text c10) (expect-word c9))
  (define c11 (expect-punct c10 #\,))
  (define-values (offset-text c12) (expect-word c11))
  (define c13 (expect-punct c12 #\)))
  (define c14 (expect-punct c13 #\[))
  (define-values (min-text c15) (expect-word c14))
  (define c16 (expect-punct c15 #\|))
  (define-values (max-text c17) (expect-word c16))
  (define c18 (expect-punct c17 #\]))
  (define-values (unit c19) (expect-string c18))
  ;; receivers: comma separated words, possibly absent
  (define-values (receivers c-final)
    (let loop ([c c19] [acc (list)])
      (cond
        [(and (peek-word? c) (not (at-entry-keyword? c)))
         (define w (token-text (cur c)))
         (define c1x (advance c))
         (if (peek-punct? c1x #\,)
             (loop (advance c1x) (cons (dbc-symbol w) acc))
             (loop c1x (cons (dbc-symbol w) acc)))]
        [else (values (reverse acc) c)])))
  (signal (dbc-symbol name)
          mux mux-ranges
          (string->number start-text)
          (string->number len-text)
          little-endian?
          signed?
          (decimal->exact scale-text)
          (decimal->exact offset-text)
          (word->number min-text)
          (word->number max-text)
          unit
          receivers
          #f #f #f)
  (values (signal (dbc-symbol name)                                     mux mux-ranges                                     (string->number start-text)                                     (string->number len-text)                                     little-endian?                                     signed?                                     (decimal->exact scale-text)                                     (decimal->exact offset-text)                                     (word->number min-text)                                     (word->number max-text)                                     unit                                     receivers                                     #f #f #f)          c-final))
;; ---------------------------------------------------------------------------
;; other entry parsers: c -> (values entry c')

(define (parse-comment-entry c)
  (cond
    [(and (peek-word? c) (equal? (token-text (cur c)) "BU_"))
     (define c1 (advance c))
     (define-values (node c2) (expect-word c1))
     (define-values (text c3) (expect-string c2))
     (define c4 (expect-punct c3 #\;))
     (values (entry-cm (list 'bu (dbc-symbol node)) text) c4)]
    [(and (peek-word? c) (equal? (token-text (cur c)) "BO_"))
     (define c1 (advance c))
     (define-values (id-text c2) (expect-word c1))
     (define-values (text c3) (expect-string c2))
     (define c4 (expect-punct c3 #\;))
     (values (entry-cm (list 'bo (string->number id-text)) text) c4)]
    [(and (peek-word? c) (equal? (token-text (cur c)) "SG_"))
     (define c1 (advance c))
     (define-values (id-text c2) (expect-word c1))
     (define-values (sig c3) (expect-word c2))
     (define-values (text c4) (expect-string c3))
     (define c5 (expect-punct c4 #\;))
     (values (entry-cm (list 'sg (string->number id-text) (dbc-symbol sig)) text) c5)]
    [(and (peek-word? c) (equal? (token-text (cur c)) "EV_"))
     (define c1 (advance c))
     (define-values (name c2) (expect-word c1))
     (define-values (text c3) (expect-string c2))
     (define c4 (expect-punct c3 #\;))
     (values (entry-cm (list 'ev (dbc-symbol name)) text) c4)]
    [else
     ;; bare comment (bus comment)
     (define-values (text c2) (expect-string c))
     (define c3 (expect-punct c2 #\;))
     (values (entry-cm '(bus) text) c3)]))

;; BA_DEF_ [BU_|BO_|SG_|EV_] "<name>" <INT|HEX|FLOAT|STRING|ENUM> ... ;
(define (parse-ba-def-entry c)
  (define-values (object c1)
    (if (and (peek-word? c)
             (member (token-text (cur c)) '("BU_" "BO_" "SG_" "EV_")))
        (values (dbc-symbol (string-downcase (substring (token-text (cur c)) 0 2)))
                (advance c))
        (values #f c)))
  (define-values (name c2) (expect-string c1))
  (define-values (kind-text c3) (expect-word c2))
  (define kind (case kind-text
                 [("INT") 'int] [("HEX") 'hex] [("FLOAT") 'float]
                 [("STRING") 'string] [("ENUM") 'enum]
                 [else (error 'parse-dbc "unknown BA_DEF_ type ~a" kind-text)]))
  (case kind
    [(string)
     (define c4 (expect-punct c3 #\;))
     (values (entry-ba-def object name kind #f #f '()) c4)]
    [(enum)
     (let loop ([c c3] [enums '()])
       (cond
         [(peek-string? c)
          (define-values (s c2) (expect-string c))
          (if (peek-punct? c2 #\,)
              (loop (advance c2) (cons s enums))
              (values (entry-ba-def object name kind #f #f (reverse (cons s enums)))
                      (expect-punct c2 #\;)))]
         [else (error 'parse-dbc "ENUM expects quoted strings")]))]
    [else
     (define-values (min-text c4) (expect-word c3))
     (define-values (max-text c5) (expect-word c4))
     (define c6 (expect-punct c5 #\;))
     (values (entry-ba-def object name kind (word->number min-text) (word->number max-text) '())
             c6)]))

;; BA_ "<name>" [target] <value> ;
(define (parse-ba-entry c)
  (define-values (name c1) (expect-string c))
  (cond
    [(and (peek-word? c1) (member (token-text (cur c1)) '("BU_" "BO_" "SG_" "EV_")))
     (define object (dbc-symbol (string-downcase (substring (token-text (cur c1)) 0 2))))
     (define c2 (advance c1))
     (define-values (target c3)
       (case object
         [(bu) (define-values (w c*) (expect-word c2)) (values (list 'bu (dbc-symbol w)) c*)]
         [(bo) (define-values (w c*) (expect-word c2)) (values (list 'bo (string->number w)) c*)]
         [(ev) (define-values (w c*) (expect-word c2)) (values (list 'ev (dbc-symbol w)) c*)]
         [else
          (define-values (idw c*) (expect-word c2))
          (define-values (sigw c**) (expect-word c*))
          (values (list 'sg (string->number idw) (dbc-symbol sigw)) c**)]))
     (define value (token->value (cur c3)))
     (define c4 (advance c3))
     (expect-punct c4 #\;)
     (values (entry-ba name target value) (advance c4))]
    [else
     (define value (token->value (cur c1)))
     (expect-punct (advance c1) #\;)
     (values (entry-ba name #f value) (advance (advance c1)))]))

;; pairs: <number> "<text>" ... until a non-number token
(define (parse-value-pairs c)
  (let loop ([c c] [pairs-rev '()])
    (if (and (peek-word? c) (word->number (token-text (cur c))))
        (let* ([v (word->number (token-text (cur c)))]
               [c1 (advance c)])
          (define-values (text c2) (expect-string c1))
          (loop c2 (cons (cons v text) pairs-rev)))
        (values (reverse pairs-rev) c))))

;; VAL_ <id> <signal> pairs ;  |  VAL_ <env-var-name> pairs ;
(define (parse-val-entry c)
  (cond
    [(and (peek-word? c) (word->number (token-text (cur c))))
     (define id (word->number (token-text (cur c))))
     (define c1 (advance c))
     (define-values (sig c2) (expect-word c1))
     (define-values (pairs c3) (parse-value-pairs c2))
     (define c4 (expect-punct c3 #\;))
     (values (entry-val id (dbc-symbol sig) pairs) c4)]
    [(peek-word? c)
     (define name (dbc-symbol (token-text (cur c))))
     (define c1 (advance c))
     (define-values (pairs c2) (parse-value-pairs c1))
     (define c3 (expect-punct c2 #\;))
     (values (entry-val name #f pairs) c3)]
    [else (error 'parse-dbc "malformed VAL_ entry")]))

;; EV_ <name> : <type> [<min>|<max>] "<unit>" <persistence> <callback>
;;              <access-type> <nodes...> ;
(define (parse-ev-entry c)
  (define-values (name c1) (expect-word c))
  (define c1b (expect-punct c1 #\:))
  (define-values (type c2) (expect-number c1b))
  (define c2b (expect-punct c2 #\[))
  (define-values (min-text c3) (expect-word c2b))
  (define c3b (expect-punct c3 #\|))
  (define-values (max-text c4) (expect-word c3b))
  (define c4b (expect-punct c4 #\]))
  (define-values (unit c5) (expect-string c4b))
  (define-values (persistence c6) (expect-number c5))
  (define-values (callback c7) (expect-number c6))
  (define-values (access-type c8) (expect-word c7))
  (let node-loop ([c c8] [nodes-rev '()])
    (cond
      [(peek-punct? c #\;)
       (values (entry-ev (dbc-symbol name) type (word->number min-text) (word->number max-text)
                         unit persistence callback (dbc-symbol access-type)
                         (reverse nodes-rev))
               (advance c))]
      [(peek-word? c)
       (node-loop (advance c) (cons (dbc-symbol (token-text (cur c))) nodes-rev))]
      [else (error 'parse-dbc "unexpected token in EV_ access nodes")])))

;; ---------------------------------------------------------------------------
;; enrichment: attach SIG_VALTYPE_ / VAL_ / CM_ SG_ to signal structs


;; ---------------------------------------------------------------------------
;; enrichment: attach SIG_VALTYPE_ / VAL_ / CM_ SG_ information to signal
;; structs so the codec sees floats, choices and comments

(define (new-signal-for db msg sig)
  (for/fold ([sig sig])
            ([e (in-list (dbc-entries db))])
    (cond
      [(and (entry-sig-valtype? e)
            (= (entry-sig-valtype-id e) (message-id msg))
            (eq? (entry-sig-valtype-signal-name e) (signal-name sig)))
       (signal-with-float sig 'ieee)]
      [(and (entry-sig-mul-val? e)
            (= (entry-sig-mul-val-id e) (message-id msg))
            (eq? (entry-sig-mul-val-signal-name e) (signal-name sig)))
       (struct-copy signal sig
                    [mux-ranges (entry-sig-mul-val-ranges e)])]
      [(and (entry-val? e)
            (exact-integer? (entry-val-id e))
            (= (entry-val-id e) (message-id msg))
            (eq? (entry-val-signal-name e) (signal-name sig))
            (pair? (entry-val-pairs e)))
       (signal-with sig
                    #:choices
                    (for/hash ([p (in-list (entry-val-pairs e))])
                      (values (car p) (cdr p))))]
      [(and (entry-cm? e)
            (let ([t (entry-cm-target e)])
              (and (list? t) (= (length t) 3) (eq? (car t) 'sg)
                   (= (cadr t) (message-id msg))
                   (eq? (caddr t) (signal-name sig)))))
       (signal-with-comment sig (entry-cm-comment e))]
      [else sig])))

(define (patch-message db msg)
  (struct-copy message msg
               [signals
                (for/list ([sig (in-list (message-signals msg))])
                  (new-signal-for db msg sig))]))

(define (enrich db)
  (struct-copy dbc db
               [entries
                (for/list ([e (in-list (dbc-entries db))])
                  (if (entry-bo? e)
                      (entry-bo (patch-message db (entry-bo-message e)))
                      e))]))

;; ---------------------------------------------------------------------------
;; main loop

(define +entry-keywords+
  (list "VERSION" "NS_" "BS_" "BU_" "BO_" "SG_" "CM_" "BA_DEF_" "BA_DEF_DEF_"
        "BA_" "VAL_" "VAL_TABLE_" "SIG_VALTYPE_" "SG_MUL_VAL_" "EV_"
        "BO_TX_BU_" "BA_REL_" "BA_DEF_REL_" "SGTYPE_" "SIG_GROUP_"))

(define +ns-symbols+
  (list "NS_DESC_" "CM_" "BA_DEF_" "BA_" "VAL_" "CAT_DEF_" "CAT_" "FILTER"
        "BA_DEF_DEF_" "EV_DATA_" "ENVVAR_DATA_" "SGTYPE_" "SGTYPE_VAL_"
        "BA_DEF_SGTYPE_" "BA_SGTYPE_" "SIG_TYPE_REF_" "VAL_TABLE_" "SIG_GROUP_"
        "SIG_VALTYPE_" "SIGTYPE_VALTYPE_" "BO_TX_BU_" "BA_DEF_REL_" "BA_REL_"
        "BA_DEF_DEF_REL_" "BU_SG_REL_" "BU_EV_REL_" "BU_BO_REL_" "SG_MUL_VAL_"))

(define (at-entry-keyword? c)
  (and (peek-word? c) (member (token-text (cur c)) +entry-keywords+) #t))

(define (parse-dbc text)
  (enrich (parse-entries (cursor (tokenize text) 0) #f (list))))

(define (parse-entries c version entries-rev)
  (cond
    ((at-end? c)
     (dbc version (reverse entries-rev)))
    (else
     (define kw (token-text (cur c)))
     (define c1 (advance c))
     (cond
       ((equal? kw "VERSION")
        (define-values (v c2) (expect-string c1))
        (parse-entries c2 v (cons (entry-version v) entries-rev)))

       ((equal? kw "NS_")
        (define c2 (if (peek-punct? c1 #\:) (advance c1) c1))
        (let ns-loop ((c3 c2) (syms (list)))
          (cond
            ((at-end? c3)
             (parse-entries c3 version (cons (entry-ns (reverse syms)) entries-rev)))
            ((and (peek-word? c3) (member (token-text (cur c3)) +ns-symbols+))
             (ns-loop (advance c3) (cons (dbc-symbol (token-text (cur c3))) syms)))
            (else
             (parse-entries c3 version (cons (entry-ns (reverse syms)) entries-rev))))))

       ((equal? kw "BS_")
        (let bs-loop ((c2 c1))
          (cond
            ((at-end? c2)
             (parse-entries c2 version (cons (entry-bs) entries-rev)))
            ((at-entry-keyword? c2)
             (parse-entries c2 version (cons (entry-bs) entries-rev)))
            (else (bs-loop (advance c2))))))

       ((equal? kw "BU_")
        (define c2 (if (peek-punct? c1 #\:) (advance c1) c1))
        (let bu-loop ((c3 c2) (nodes (list)))
          (cond
            ((at-end? c3)
             (parse-entries c3 version (cons (entry-bu (reverse nodes)) entries-rev)))
            ((at-entry-keyword? c3)
             (parse-entries c3 version (cons (entry-bu (reverse nodes)) entries-rev)))
            ((peek-word? c3)
             (bu-loop (advance c3) (cons (dbc-symbol (token-text (cur c3))) nodes)))
            (else
             (parse-entries c3 version (cons (entry-bu (reverse nodes)) entries-rev))))))

       ((equal? kw "BO_")
        (define-values (id-text c2) (expect-word c1))
        (define-values (name-token c3) (expect-word c2))
        (expect-punct c3 #\:)
        (define c4 (advance c3))
        (define-values (dlc c5) (expect-number c4))
        (define transmitter
          (and (peek-word? c5)
               (not (equal? (token-text (cur c5)) "SG_"))
               (dbc-symbol (token-text (cur c5)))))
        (define c6 (if transmitter (advance c5) c5))
        (define raw-id (word->number id-text))
        (define extended? (not (zero? (bitwise-and raw-id #x80000000))))
        (define base-msg
          (message (bitwise-and raw-id #x1FFFFFFF) extended?
                   (dbc-symbol name-token)
                   dlc transmitter (list)))
        (let signal-loop ((c c6) (msg base-msg))
          (cond
            ((and (peek-word? c) (equal? (token-text (cur c)) "SG_"))
             (define-values (sig c-next) (parse-signal-entry (advance c)))
             (signal-loop c-next (message-with-signal msg (signal-name sig) sig)))
            (else
             (parse-entries c version (cons (entry-bo msg) entries-rev))))))

       ((equal? kw "SG_")
        (error 'parse-dbc "unexpected SG_ outside a BO_ message"))

       ((equal? kw "CM_")
        (define-values (entry c2) (parse-comment-entry c1))
        (parse-entries c2 version (cons entry entries-rev)))

       ((equal? kw "BA_DEF_")
        (define-values (entry c2) (parse-ba-def-entry c1))
        (parse-entries c2 version (cons entry entries-rev)))

       ((equal? kw "BA_DEF_DEF_")
        (define-values (name c2) (expect-string c1))
        (define value (token->value (cur c2)))
        (expect-punct (advance c2) #\;)
        (parse-entries (advance (advance c2)) version
                       (cons (entry-ba-def-def name value) entries-rev)))

       ((equal? kw "BA_")
        (define-values (entry c2) (parse-ba-entry c1))
        (parse-entries c2 version (cons entry entries-rev)))

       ((equal? kw "VAL_")
        (define-values (entry c2) (parse-val-entry c1))
        (parse-entries c2 version (cons entry entries-rev)))

       ((equal? kw "VAL_TABLE_")
        (define-values (name c2) (expect-word c1))
        (define-values (pairs c3) (parse-value-pairs c2))
        (define c4 (expect-punct c3 #\;))
        (parse-entries c4 version
                       (cons (entry-val-table (dbc-symbol name) pairs) entries-rev)))

       ((equal? kw "SIG_VALTYPE_")
        (define-values (id c2) (expect-number c1))
        (define-values (sig-name c3) (expect-word c2))
        (define c4 (expect-punct c3 #\:))
        (define kind
          (cond
            ((equal? (token-text (cur c4)) "0") 'float)
            (else 'double)))
        (define c5 (advance c4))
        (parse-entries (expect-punct c5 #\;) version
                       (cons (entry-sig-valtype id (dbc-symbol sig-name) kind) entries-rev)))

       ((equal? kw "SG_MUL_VAL_")
        (define-values (id c2) (expect-number c1))
        (define-values (sig-name c3) (expect-word c2))
        (define-values (switch c4) (expect-word c3))
        (let range-loop ((c5 c4) (ranges (list)))
          (cond
            ((peek-punct? c5 #\;)
             (parse-entries (advance c5) version
                            (cons (entry-sig-mul-val id (dbc-symbol sig-name)
                                                     (dbc-symbol switch) (reverse ranges))
                                  entries-rev)))
            (else
             (define-values (lo c6) (expect-number c5))
             (define c7 (expect-punct c6 #\-))
             (define-values (hi c8) (expect-number c7))
             (range-loop (if (peek-punct? c8 #\,) (advance c8) c8)
                         (cons (cons lo hi) ranges))))))

       ((equal? kw "EV_")
        (define-values (entry c2) (parse-ev-entry c1))
        (parse-entries c2 version (cons entry entries-rev)))

       (else
        (let unknown-loop ((c2 c1) (parts (list kw)))
          (cond
            ((at-end? c2)
             (parse-entries c2 version
                            (cons (entry-unparsed (string-join (reverse parts) " ")) entries-rev)))
            ((token-punct? (cur c2) #\;)
             (parse-entries (advance c2) version
                            (cons (entry-unparsed (string-join (reverse parts) " ")) entries-rev)))
            (else
             (define t (cur c2))
             (unknown-loop (advance c2)
                           (cons (if (eq? (token-kind t) 'string)
                                     (string-append "\"" (token-text t) "\"")
                                     (token-text t))
                                 parts))))))))))

