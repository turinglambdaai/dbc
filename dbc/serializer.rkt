#lang racket/base

;; DBC serializer: ordered `dbc` entries -> text.  Round-trip guarantee:
;; parse(serialize(parse(text))) is model-equal to parse(text) (whitespace
;; is normalized; string escapes are re-applied).

(require racket/contract
         racket/match
         racket/string
         "model.rkt")

(provide (contract-out
          [serialize-dbc (-> dbc? string?)]
          [exact-rational->string (-> rational? string?)]
          [escape-dbc-string (-> string? string?)]))

;; 1/100 -> "0.01"; 5 -> "5"; -3/8 -> "-0.375" (terminating decimals only,
;; which is all DBC files contain).
(define (exact-rational->string r)
  (cond [(integer? r) (number->string r)]
        [else
         (define k
           (or (for/first ([k (in-range 1 30)]
                           #:when (integer? (* r (expt 10 k))))
                 k)
               (error 'exact-rational->string "non-terminating decimal ~a" r)))
         (define digits (number->string (abs (* r (expt 10 k)))))
         (define padded
           (string-append (make-string (max 0 (- (add1 k) (string-length digits))) #\0)
                          digits))
         (define int-part (substring padded 0 (max 1 (- (string-length padded) k))))
         (define frac-part (substring padded (string-length int-part)))
         (format "~a~a.~a"
                 (if (negative? r) "-" "")
                 (string-trim-left-zeros int-part)
                 frac-part)]))

(define (string-trim-left-zeros s)
  (define trimmed (string-trim s "0" #:left? #t #:right? #f))
  (if (string=? trimmed "") "0" trimmed))

(define (escape-dbc-string s)
  (string-append "\""
                 (string-replace (string-replace s "\\" "\\\\") "\"" "\\\"")
                 "\""))

(define (entry->string e)
  (match e
    [(entry-version text) (format "VERSION ~a\n" (escape-dbc-string text))]
    [(entry-ns syms)
     (string-append "NS_ :\n"
                    (string-join (for/list ([s syms]) (format "\t~a" s)) "\n")
                    "\n")]
    [(entry-bs) "BS_:\n"]
    [(entry-bu nodes)
     (string-append "BU_: " (string-join (map symbol->string nodes) " ") "\n")]
    [(entry-val-table name pairs)
     (format "VAL_TABLE_ ~a ~a ;\n"
             name
             (string-join (for/list ([p pairs])
                            (format "~a ~a" (car p) (escape-dbc-string (cdr p))))
                          " "))]
    [(entry-bo msg)
     (string-append (format "BO_ ~a ~a: ~a ~a\n"
                            (if (message-extended? msg)
                                (+ (message-id msg) #x80000000)
                                (message-id msg))
                            (message-name msg)
                            (message-dlc msg)
                            (or (message-transmitter msg) 'Vector__XXX))
                    (string-join
                     (for/list ([s (message-signals msg)])
                       (signal-line (message-id msg) s))
                     ""))]
    [(entry-cm target comment)
     (match target
       [(list 'bu node) (format "CM_ BU_ ~a ~a;\n" node (escape-dbc-string comment))]
       [(list 'bo id) (format "CM_ BO_ ~a ~a;\n" id (escape-dbc-string comment))]
       [(list 'sg id sig) (format "CM_ SG_ ~a ~a ~a;\n" id sig (escape-dbc-string comment))]
       [(list 'ev name) (format "CM_ EV_ ~a ~a;\n" name (escape-dbc-string comment))]
       [(list 'bus) (format "CM_ ~a;\n" (escape-dbc-string comment))])]
    [(entry-ba-def object name kind minimum maximum enums)
     (define object-text
       (case object
         [(bu) "BU_"] [(bo) "BO_"] [(sg) "SG_"] [(ev) "EV_"]
         [else ""]))
     (define head
       (format "BA_DEF_ ~a ~a ~a"
               object-text
               (escape-dbc-string name)
               (case kind [(int) "INT"] [(hex) "HEX"] [(float) "FLOAT"]
                 [(string) "STRING"] [(enum) "ENUM"])))
     (define tail
       (case kind
         [(string) ""]
         [(enum) (if (null? enums)
                     ""
                     (string-append " "
                                    (string-join (map escape-dbc-string enums) ",")))]
         [else (if minimum
                   (format " ~a ~a" (exact-rational->string minimum)
                           (exact-rational->string maximum))
                   "")]))
     (string-append head tail ";\n")]
    [(entry-ba-def-def name value)
     (format "BA_DEF_DEF_ ~a ~a;\n"
             (escape-dbc-string name)
             (cond [(string? value) (escape-dbc-string value)]
                   [(rational? value) (exact-rational->string value)]
                   [else (format "~a" value)]))]
    [(entry-ba name target value)
     (define target-text
       (match target
         [#f ""]
         [(list 'bu node) (format " BU_ ~a" node)]
         [(list 'bo id) (format " BO_ ~a" id)]
         [(list 'ev name*) (format " EV_ ~a" name*)]
         [(list 'sg id sig) (format " SG_ ~a ~a" id sig)]))
     (format "BA_ ~a~a ~a;\n"
             (escape-dbc-string name)
             target-text
             (cond [(string? value) (escape-dbc-string value)]
                   [(rational? value) (exact-rational->string value)]
                   [else (format "~a" value)]))]
    [(entry-val id signal-name pairs)
     (if signal-name
         (format "VAL_ ~a ~a ~a ;\n"
                 id signal-name
                 (string-join (for/list ([p pairs])
                                (format "~a ~a" (car p) (escape-dbc-string (cdr p))))
                              " "))
         (format "VAL_ ~a ~a ;\n"
                 id
                 (string-join (for/list ([p pairs])
                                (format "~a ~a" (car p) (escape-dbc-string (cdr p))))
                              " ")))]
    [(entry-sig-valtype id sig kind)
     (format "SIG_VALTYPE_ ~a ~a : ~a;\n" id sig (case kind [(float) 0] [(double) 1]))]
    [(entry-sig-mul-val id sig switch ranges)
     (format "SG_MUL_VAL_ ~a ~a ~a ~a;\n"
             id sig switch
             (string-join (for/list ([r ranges])
                            (format "~a-~a" (car r) (cdr r)))
                          ","))]
    [(entry-ev name type minimum maximum unit persistence callback access-type access-nodes)
     (format "EV_ ~a : ~a [~a|~a] ~a ~a ~a ~a ~a;\n"
             name type (exact-rational->string minimum) (exact-rational->string maximum)
             (escape-dbc-string unit) persistence callback access-type
             (string-join (map symbol->string access-nodes) " "))]
    [(entry-unparsed text) (format "~a;\n" text)]
    [_ (error 'serialize-dbc "unknown entry ~a" e)]))

(define (signal-line msg-id s)
  (format " SG_ ~a~a : ~a|~a@~a~a (~a,~a) [~a|~a] ~a~a\n"
          (signal-name s)
          (case (signal-mux s)
            [(#f) ""]
            [(multiplexor) " M"]
            [else (format " m~a" (signal-mux s))])
          (signal-start-bit s)
          (signal-bit-count s)
          (if (signal-little-endian? s) "1" "0")
          (if (signal-signed? s) "-" "+")
          (exact-rational->string (signal-scale s))
          (exact-rational->string (signal-offset s))
          (if (signal-minimum s) (exact-rational->string (signal-minimum s)) 0)
          (if (signal-maximum s) (exact-rational->string (signal-maximum s)) 0)
          (escape-dbc-string (or (signal-unit s) ""))
          (let ([rx (signal-receivers s)])
            (if (null? rx)
                ""
                (string-append " "
                               (string-join (map symbol->string rx) ","))))))

(define (serialize-dbc db)
  (string-join (for/list ([e (dbc-entries db)]) (entry->string e)) ""))
