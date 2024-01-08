#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: transaction.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.09.12
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; About Transactions
;;;;
;;;; A transaction is a data structure containing:
;;;;
;;;;   - A source wallet and its respective signature (of the sender)
;;;;   - A recipient wallet
;;;;   - The amount to be transferred
;;;;   - Input objects that compound the source resources
;;;;   - Output objects as the resulting of the transfer
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Defining the transaction structure
;;;;

;; Every transaction has a list of inputs and outputs objects. See more details
;; in the "transaction-io.rkt" file.
(struct transaction
  (signature from to value inputs outputs)
  #:prefab) ; The 'previously fabricated' keyword allows us to display and
            ; serialize this kind of structure.


;;;;
;;;; Building, processing and signing transactions
;;;;

;; Sets a procedure to supports the genesis block creation as well the future
;; value transfers. Retuns a transaction structure.
;; Note: "inputs" need be a list, as well "from" and "to" are wallet structs.
(define (make-transaction from to value inputs)
  (transaction "" from to value inputs '()))

;; Receives a partially formed transaction object, sets its Output objects and
;; signs it. Retuns a complete formed and signed transaction.
(define (process-transaction t)
  (letrec
      ((inputs (transaction-inputs t))
       (outputs (transaction-outputs t))
       (value (transaction-value t))
       ; Sums the value of the Inputs objects. These are the source of resources
       ; that support the transaction.
       (inputs-sum
        (foldr + 0 (map (lambda (i) (transaction-io-value i)) inputs)))
       ; Calculates the remaining resources from the Input objects. The leftover
       ; is the difference between the total of the input resources and the
       ; transaction value. So "inputs-sum" >= "value".
       (leftover (- inputs-sum value))
       ; Generates two Outputs objects. A transaction must have two outputs:
       ; (a) An output for the spent value. That isthe amount that is sent for
       ; the recipient; and (b) An output for the letover to be kept owned by
       ; the sender.
       (new-outputs
        (list (make-transaction-io value (transaction-to t))
              (make-transaction-io leftover (transaction-from t)))))
      ; Appends the outputs and signs the transaction
      (transaction
        (sign-transaction (transaction-from t)
                          (transaction-to t)
                          (transaction-value t))
        (transaction-from t)
        (transaction-to t)
        value
        inputs
        (append new-outputs outputs))))

;; Generates a digital signature for the sender by hashing and encrypting the
;; data transaction.
;; Retuns a HEX string of bytes.
(define (sign-transaction from to value)
  (let ((privkey (wallet-private-key from))
        (pubkey (wallet-public-key from)))
    (bytes->hex-string
     ; Generates a transaction signature by hashing and encrypting an appended
     ; set of data plus the sender's (wallet) private-key. The "digest/sign"
     ; procedure is equivalent to calling "digest" and "pk-sign-digest"
     ; respectively.
     (digest/sign
       (datum->pk-key (hex-string->bytes privkey) 'PrivateKeyInfo)
       'sha1
       (bytes-append
         ; normalizes all data as string and converts to bytes
         (string->bytes/utf-8 (~a (serialize from)))
         (string->bytes/utf-8 (~a (serialize to)))
         (string->bytes/utf-8 (number->string value)))))))

;; Checks the processed transaction validity
(define (valid-transaction? t)
  (let ((sum-inputs (foldr + 0 (map (lambda (t) (transaction-io-value t))
                         (transaction-inputs t))))
        (sum-outputs (foldr + 0 (map (lambda (t) (transaction-io-value t))
                         (transaction-outputs t)))))
       ; Returns #t only if all statments are #t
       (and
         ; Is a valid signature?
         (valid-transaction-signature? t)
         ; Are the output ok?
         (true-for-all? valid-transaction-io? (transaction-outputs t))
         ; Are the total input sum greater than or equal total output sum?
         ; This is a fundamental condition to avoid the double-spending problem!
         ; Note: I don't understand why sum-inputs and sum-outputs shouldn't be
         ; exactly equal...
         (>= sum-inputs sum-outputs))))

;; Checks the signature validity for a transaction sender
(define (valid-transaction-signature? t)
  (let ((pubkey (wallet-public-key (transaction-from t))))
       ; Verifies a transaction signature by hashing and encrypting an appended
       ; set of transaction data plus the sender's (wallet) public-key. The
       ; "digest/verify" procedure is equivalent to calling "digest" and
       ; "pk-verify-digest" respectively and retuns #t or #f. 
       (digest/verify
         (datum->pk-key (hex-string->bytes pubkey) 'SubjectPublicKeyInfo)
         'sha1
         (bytes-append
           ; normalizes all data as string and converts to bytes
           (string->bytes/utf-8 (~a (serialize (transaction-from t))))
           (string->bytes/utf-8 (~a (serialize (transaction-to t))))
           (string->bytes/utf-8 (number->string (transaction-value t))))
         (hex-string->bytes (transaction-signature t)))))


;;;;
;;;; Requirements
;;;;

;; Run 'raco pkg install <package>' or use 'File > Package Manager' in DrRacket
;; to install the package dependecies.
(use-all-factories!)
(require "transaction-io.rkt")
(require "utils.rkt")
(require "wallet.rkt")
(require crypto)
(require crypto/all)
(require (only-in file/sha1 hex-string->bytes))
(require racket/serialize)


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (all-from-out "transaction-io.rkt")
         (struct-out transaction)
         make-transaction
         process-transaction
         valid-transaction?)
