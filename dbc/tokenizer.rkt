#lang racket/base

;; Hand-rolled tokenizer for the DBC format.  Token kinds:
;;   word   — bare identifiers / numbers (raw text kept)
;;   string — quoted strings, escapes resolved, may span lines
;;   punct  — one of ( ) ; : | @ [ ]
;; Comments do not exist in DBC; a ';' terminates an entry.

(require racket/contract)

(provide (contract-out
          [tokenize (-> string? (listof token?))]
          (struct token ((kind (or/c 'word 'string 'punct))
                         (text string?)))
          [token-word? (-> token? boolean?)]
          [token-string? (-> token? boolean?)]
          [token-punct? (-> token? char? boolean?)]))

(struct token (kind text) #:transparent)

(define (token-word? t) (eq? (token-kind t) 'word))
(define (token-string? t) (eq? (token-kind t) 'string))
(define (token-punct? t c)
  (and (eq? (token-kind t) 'punct) (equal? (string-ref (token-text t) 0) c)))

(define +punct-chars+ "( );:|,@[]")

;; Is `c` one of the single-character tokens?
(define (punct-char? c) (and (memv c (string->list +punct-chars+)) #t))

(define (word-char? c)
  (and (not (char-whitespace? c))
       (not (punct-char? c))
       (not (char=? c #\"))
       ;; signs are handled separately: glued to a following number they
       ;; start a negative/positive number word, otherwise they are punct
       (not (char=? c #\-))
       (not (char=? c #\+))))

(define (tokenize s)
  (define len (string-length s))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i len) (reverse acc)]
      [else
       (define c (string-ref s i))
       (cond
         [(char-whitespace? c) (loop (add1 i) acc)]
         [(punct-char? c)
          (loop (add1 i) (cons (token 'punct (string c)) acc))]
         [(char=? c #\")
          ;; quoted string: \" and \\ are the escapes, everything else is
          ;; verbatim, including newlines
          (define-values (next tok) (read-string s (add1 i) '() i))
          (loop next (cons tok acc))]
         [(and (or (char=? c #\-) (char=? c #\+))
               (< (add1 i) len)
               ;; not glued to a preceding word (a sign between digits is
               ;; a separator, e.g. SG_MUL_VAL_ ranges "100-200")
               (let ([p (if (zero? i) #\space (string-ref s (sub1 i)))])
                 (not (or (char-numeric? p) (char-alphabetic? p))))
               (let ([n (string-ref s (add1 i))])
                 (or (char-numeric? n) (char=? n #\.))))
          ;; a sign glued to a number
          (define-values (word next) (word-span s (add1 i)))
          (loop next (cons (token 'word (string-append (string c) word)) acc))]
         [(or (char=? c #\-) (char=? c #\+))
          ;; a sign not glued to a number: a plain punct token (e.g. the
          ;; signed marker in SG_ or the range separator of SG_MUL_VAL_)
          (loop (add1 i) (cons (token 'punct (string c)) acc))]
         [(word-char? c)
          (define-values (word next) (word-span s i))
          (loop next (cons (token 'word word) acc))]
         [else (error 'tokenize "unexpected character ~s at offset ~a" c i)])])))

;; Reads a quoted string whose opening quote precedes `i`.  Returns the
;; position after the closing quote and the resolved string token.
(define (read-string s i chars i0)
  (define len (string-length s))
  (cond
    [(>= i len)
     (error 'tokenize "unterminated string starting at offset ~a" i0)]
    [else
     (define cj (string-ref s i))
     (cond
       [(char=? cj #\\)
        (if (>= (add1 i) len)
            (error 'tokenize "dangling escape at end of input")
            (let ([next (string-ref s (add1 i))])
              (if (or (char=? next #\") (char=? next #\\))
                  (read-string s (+ i 2) (cons next chars) i0)
                  (read-string s (+ i 2) (cons cj (cons next chars)) i0))))]
       [(char=? cj #\")
        (values (add1 i) (token 'string (list->string (reverse chars))))]
       [else (read-string s (add1 i) (cons cj chars) i0)])]))

;; Longest run of word characters starting at i; returns the substring and
;; the offset after it.
(define (word-span s i)
  (define len (string-length s))
  (let loop ([j i])
    (if (and (< j len) (word-char? (string-ref s j)))
        (loop (add1 j))
        (values (substring s i j) j))))
