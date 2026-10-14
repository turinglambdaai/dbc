#lang racket/base

;; Signal codec: raw bit extraction/insertion for both bit orders, the
;; physical conversion (scale/offset, signed, IEEE float), and mux-aware
;; message decode/encode.
;;
;; Bit numbering: bit n lives in byte (quotient n 8) at bit position
;; (remainder n 8), counting from the LSB -- bit 39 is byte 4, bit 7.
;;
;; Intel (@1, little-endian): the value's LSB is at `start`; the value
;; occupies bits start, start+1, ..., start+len-1, accumulated LSB-first.
;;
;; Motorola (@0, big-endian): the value's MSB is at `start`; the remaining
;; bits walk the sawtooth -- from a byte's LSB the walk jumps to the next
;; byte's MSB (+15), otherwise it descends one position (-1) -- accumulated
;; MSB-first.  This reproduces cantools' placement exactly (verified bit by
;; bit against cantools 40.x).

(require racket/contract
         racket/match
         "model.rkt")

(provide (contract-out
          [signal-raw-extract (-> signal? bytes? (or/c exact-integer? real?))]
          [signal-raw-insert (-> signal? bytes? exact-integer? bytes?)]
          [signal-decode (-> signal? bytes? (or/c exact-integer? rational? real?))]
          [signal-encode (-> signal? bytes? real? bytes?)]
          [message-signal (-> message? symbol? (or/c signal? #f))]
          [mux-switch-value (-> message? bytes? (or/c exact-integer? #f))]
          [signal-active? (-> signal? (or/c exact-integer? #f) boolean?)]
          [message-decode
           (->* (message? bytes?)
                (#:decode-choices? boolean?)
                (hash/c symbol? (or/c exact-integer? rational? real? string?)))]
          [message-encode
           (->* (message? (hash/c symbol? real?))
                (#:base (or/c bytes? #f)
                 #:dlc (or/c #f exact-nonnegative-integer?))
                bytes?)]))

;; ---------------------------------------------------------------------------
;; raw bit extraction / insertion

(define (bit-ref payload n)
  (bitwise-and (arithmetic-shift (bytes-ref payload (quotient n 8))
                                 (- (remainder n 8)))
               1))

;; The sawtooth walk for Motorola signals: from a byte's LSB (position 0)
;; the next bit is the next byte's MSB, otherwise one position down.
(define (walk-step n)
  (if (zero? (remainder n 8))
      (+ n 15)
      (sub1 n)))

(define (extract-raw-bits sig payload)
  (define start (signal-start-bit sig))
  (define len (signal-bit-count sig))
  (if (signal-little-endian? sig)
      ;; Intel: LSB-first accumulation over consecutive bit numbers
      (for/fold ([v 0]) ([i (in-range len)])
        (bitwise-ior v (arithmetic-shift (bit-ref payload (+ start i)) i)))
      ;; Motorola: sawtooth walk, value accumulated MSB-first
      (let walk ([i 0] [n start] [v 0])
        (cond
          [(= i len) v]
          [else
           (walk (add1 i)
                 (walk-step n)
                 (bitwise-ior (arithmetic-shift v 1) (bit-ref payload n)))]))))

(define (set-dbc-bit! payload n bitv)
  (define byte (quotient n 8))
  (define bit (remainder n 8))
  (define v (bytes-ref payload byte))
  (bytes-set! payload byte
              (if (= bitv 1)
                  (bitwise-ior v (arithmetic-shift 1 bit))
                  (bitwise-and v (bitwise-not (arithmetic-shift 1 bit))))))

(define (insert-raw-bits! payload sig raw)
  (define start (signal-start-bit sig))
  (define len (signal-bit-count sig))
  (if (signal-little-endian? sig)
      ;; Intel: bit i of raw goes to bit number start+i
      (for ([i (in-range len)])
        (define bitv (bitwise-and (arithmetic-shift raw (- i)) 1))
        (set-dbc-bit! payload (+ start i) bitv))
      ;; Motorola: sawtooth walk, raw bits MSB-first
      (let loop ([i 0] [n start])
        (cond
          [(= i len) payload]
          [else
           ;; raw bit (len-1-i): shift right by (len-1-i)
           (define bitv (bitwise-and (arithmetic-shift raw (- (+ i 1) len)) 1))
           (set-dbc-bit! payload n bitv)
           (loop (add1 i) (walk-step n))])))
  payload)

(define (signal-raw-extract sig payload)
  (define unsigned (extract-raw-bits sig payload))
  (cond
    [(eq? (signal-float-kind sig) 'ieee)
     (define width (if (= (signal-bit-count sig) 64) 8 4))
     (floating-point-bytes->real (integer->bytes unsigned width) #t)]
    [(signal-signed? sig)
     (define bits (signal-bit-count sig))
     (if (>= unsigned (arithmetic-shift 1 (sub1 bits)))
         (- unsigned (arithmetic-shift 1 bits))
         unsigned)]
    [else unsigned]))

(define (signal-raw-insert sig payload raw)
  (define len (signal-bit-count sig))
  (define unsigned
    (cond
      [(eq? (signal-float-kind sig) 'ieee)
       (define width (if (= (signal-bit-count sig) 64) 8 4))
       (bytes->integer (real->floating-point-bytes (exact->inexact raw) width #t))]
      [(signal-signed? sig)
       (modulo raw (arithmetic-shift 1 len))]
      [else raw]))
  (insert-raw-bits! payload sig unsigned))

(define (integer->bytes v width)
  (list->bytes
   (for/list ([i (in-range (sub1 width) -1 -1)])
     (bitwise-and (arithmetic-shift v (* -8 i)) #xFF))))

(define (bytes->integer bs)
  (for/fold ([v 0]) ([b (in-bytes bs)]) (+ (arithmetic-shift v 8) b)))

;; ---------------------------------------------------------------------------
;; physical conversion

(define (signal-decode sig payload)
  (define raw (signal-raw-extract sig payload))
  (cond
    ;; floats pass through already converted
    [(eq? (signal-float-kind sig) 'ieee) raw]
    ;; identity scaling keeps the raw integer (as cantools does)
    [(and (= (signal-scale sig) 1) (zero? (signal-offset sig))) raw]
    [else (exact->inexact (+ (* raw (signal-scale sig)) (signal-offset sig)))]))

(define (signal-encode sig payload value)
  (define raw
    (cond
      [(eq? (signal-float-kind sig) 'ieee)
       ;; value is already the float; signal-raw-insert converts
       value]
      [else
       ;; physical -> raw: (value - offset) / scale, rounded half to even
       ;; (python `round`, so encodings agree with cantools)
       (define exact-value
         (cond [(rational? value) value]
               [else (inexact->exact value)]))
       (round (/ (- exact-value (signal-offset sig)) (signal-scale sig)))]))
  (signal-raw-insert sig payload (if (exact-integer? raw) raw (inexact->exact raw))))

;; ---------------------------------------------------------------------------
;; message level

(define (message-signal msg name)
  (for/first ([s (in-list (message-signals msg))]
              #:when (eq? (signal-name s) name))
    s))

;; The value of the multiplexor switch signal (the one named with M), or #f.
(define (mux-switch-value msg payload)
  (for/first ([s (in-list (message-signals msg))]
              #:when (eq? (signal-mux s) 'multiplexor))
    (let ([v (signal-raw-extract s payload)])
      (if (exact-integer? v) v #f))))

;; Is `sig` present in a frame whose multiplexor value is `switch`?
;; Plain muxed signals (mN) match on N; extended-mux signals (with
;; SG_MUL_VAL_ ranges) match on N or when the switch falls in a range.
(define (signal-active? sig switch)
  (match (signal-mux sig)
    [#f #t]
    ['multiplexor #t]
    [(? integer? n)
     (or (and switch (equal? switch n))
         (in-mux-ranges? sig switch))]
    [_ #f]))

(define (in-mux-ranges? sig switch)
  (define ranges (signal-mux-ranges sig))
  (or (null? ranges)
      (and switch
           (for/or ([r (in-list ranges)])
             (and (>= switch (car r)) (<= switch (cdr r)))))))

(define (message-decode msg payload
                        #:decode-choices? [decode-choices? #f])
  (when (< (bytes-length payload) (message-dlc msg))
    (error 'message-decode
           "payload of ~a byte(s) is shorter than ~a's DLC ~a"
           (bytes-length payload) (message-name msg) (message-dlc msg)))
  (define switch (mux-switch-value msg payload))
  ;; same-name muxed variants may both be active under extended ranges;
  ;; hash-set folds them into one entry (last active wins)
  (for/fold ([acc (hasheq)])
            ([s (in-list (message-signals msg))]
             #:when (signal-active? s switch))
    (hash-set acc
              (signal-name s)
              (let ([v (signal-decode s payload)])
                (if (and decode-choices?
                         (exact-integer? v)
                         (signal-choices s)
                         (hash-ref (signal-choices s) v #f))
                    (hash-ref (signal-choices s) v)
                    v)))))

;; Encode `values` into a DLC-sized buffer (zero initialized unless #:base).
;; Signals not present in `values` are left at their buffer bits; signal
;; names absent from the message are an error.
(define (message-encode msg values
                        #:base [base #f]
                        #:dlc [dlc #f])
  (define size (or dlc (message-dlc msg) (bytes-length (or base #""))))
  (define out (if base (bytes-copy base) (make-bytes size 0)))
  ;; The multiplexor switch is written first (when present) so muxed
  ;; signals see the right switch value regardless of hash iteration
  ;; order.
  (define switch-name
    (for/first ([s (in-list (message-signals msg))]
                #:when (eq? (signal-mux s) 'multiplexor))
      (signal-name s)))
  (define pairs
    (if switch-name
        (let ([ps (hash->list values)])
          (append (filter (lambda (p) (eq? (car p) switch-name)) ps)
                  (filter (lambda (p) (not (eq? (car p) switch-name))) ps)))
        (hash->list values)))
  (for ([pair (in-list pairs)])
    (define name (car pair))
    (define sig (message-signal msg name))
    (unless sig
      (error 'message-encode "message ~a has no signal ~a" (message-name msg) name))
    (unless (signal-active? sig (mux-switch-value msg out))
      (error 'message-encode
             "signal ~a is multiplexed and not active under the current switch" name))
    (set! out (signal-encode sig out (cdr pair))))
  out)
