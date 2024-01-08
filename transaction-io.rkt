#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: transaction-io.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.09.12
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Defining the I/O transaction object structure
;;;;

;; This is the building-block to build the Input/Output (I/O) objects that are
;; part of one transaction. The Input objects are the source of original
;; resources involved (spent) in the transaction, all owned by the sender.
;; Output objects are created in the transaction building process and are partly
;; owned by the recipient (the amount that was sent or transacted) and partly
;; owned by the sender (the remaining total amount of the original Input
;; objects). 
(struct transaction-io
  (transaction-hash value owner timestamp)
  #:prefab) ; The 'previously fabricated' keyword allows us to display and
            ; serialize this kind of structure.


;;;;
;;;; Building I/O transactions objects
;;;;


;; Generates a I/O transaction object.
;; Note: "owner" needs be a wallet struct.
(define (make-transaction-io value owner)
  ; "current-milliseconds" returns a time value (int) since 00:00UTC/Jan-01-1970
  (let ((timestamp (current-milliseconds)))
    (transaction-io
     (calculate-transaction-io-hash value owner timestamp) value owner
     timestamp)))


;; Calculates a transaction hash using SH256 and the data's transaction as
;; input. This implementation normalizes all data as string type to facilitate
;; the data manipulation and storage.
;; This procedures returns a hexdecimal hash number.
(define (calculate-transaction-io-hash value owner timestamp)
      (bytes->hex-string
         ;; Converts strings to bytes to use with SHA256 procedure
         (sha256 (bytes-append
                    (string->bytes/utf-8 (number->string value))
                    ; Converts a serialized object to string
                    (string->bytes/utf-8 (~a (serialize owner)))
                    (string->bytes/utf-8  (number->string timestamp))))))


;; Defines a predicate to verify the transaction hash validity
(define (valid-transaction-io? t-in)
  (equal? (transaction-io-transaction-hash t-in)
          (calculate-transaction-io-hash (transaction-io-value t-in)
                                         (transaction-io-owner t-in)
                                         (transaction-io-timestamp  t-in))))


;;;;
;;;; Requirements
;;;;

;; Run 'raco pkg install <package>' or use 'File > Package Manager' in DrRacket
;; to install the package dependecies.
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (struct-out transaction-io) make-transaction-io valid-transaction-io?)

