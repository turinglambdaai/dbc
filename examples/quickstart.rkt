#lang racket/base
;; Fully synthetic CAN DBC, no bus interface or external fixture needed.
(require dbc)

(define db
  (parse-dbc #<<DBC
VERSION "1.0"

BO_ 37 ExampleMessage: 8 Vector__XXX
 SG_ VehicleSpeed : 39|16@1+ (0.01,0) [-100|100] "kph" Vector__XXX
 SG_ Temperature : 0|12@0- (0.01,250) [-40|215] "degC" Vector__XXX

VAL_ 37 VehicleSpeed 0 "standstill" 1 "moving" ;
DBC
))

(define msg
  (for/first ([e (in-list (dbc-entries db))] #:when (entry-bo? e))
    (entry-bo-message e)))
(unless (and (message? msg) (eq? (message-name msg) 'ExampleMessage))
  (error 'dbc-quickstart "DBC message not found"))

(define encoded
  (message-encode msg (hash 'VehicleSpeed 0.5 'Temperature 250.47)))
(unless (= (bytes-length encoded) 8)
  (error 'dbc-quickstart "encoded CAN frame must be 8 bytes"))
(define decoded (message-decode msg encoded))
(unless (and (hash-has-key? decoded 'VehicleSpeed)
             (hash-has-key? decoded 'Temperature))
  (error 'dbc-quickstart "signals missing after CAN frame round-trip"))

(define choices (message-decode msg (make-bytes 8 0) #:decode-choices? #t))
(unless (equal? (hash-ref choices 'VehicleSpeed) "standstill")
  (error 'dbc-quickstart "VAL_ choice lookup failed"))
(unless (equal? (parse-dbc (serialize-dbc db)) db)
  (error 'dbc-quickstart "DBC grammar serialization lost information"))
(displayln "DBC quickstart OK")
