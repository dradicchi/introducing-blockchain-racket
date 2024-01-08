#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: block.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.09.12
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Introducing Blocks and Transactions
;;;;
;;;; As the name points, blocks are the building unit of the blockchain, which
;;;; is a set of ordered (in time) blocks. A block is defined as a current and a
;;;; previous hash number, that allow to link it with your previous and next
;;;; block on the chain, as well the timestamp that it was generated and the
;;;; the transaction data. The current hash is derived from the previous hash
;;;; and the other block data, so is possible to use a hashing algorithm to
;;;; verify the validity of a block. In turn, a transaction is a dataset
;;;; containing a source wallet and the respective signature, a recipient wallet
;;;; and an amount to be transferred.
;;;;
;;;;
;;;; Introducing Mining and Proof-of-Work systems
;;;;
;;;; Concerning a distributed ledger formed by a blockchain, we define "mining a
;;;; block" as the process of recording a new current valid block containing new
;;;; confirmed transactions. Many of the relevant cryptocurrency projects adopt
;;;; a mining algorithm based on a proof-of-work (PoW) system, which is a way of
;;;; establishing trust by demanding one or more interested parties in to apply
;;;; computational power in the process to create and validate a block. This
;;;; project uses a PoW inspired on Hashcash algorithm, created by Adam Back.
;;;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Defining block and (internal) transaction structures
;;;;

(struct block
  ;; The nonce parameter is needed for the PoW algorithm
  (current-hash previous-hash transaction timestamp nonce)
  #:prefab) ; The 'previously fabricated' keyword allows us to display and
            ; serialize this kind of structure.

;; In this implementation, each block carries only one transaction
(struct transaction
  (signature from to value)
  #:prefab) 


;;;;
;;;; Calculating the current block hash
;;;;

;; Calculates a current block hash using SH256 and the data's block as input.
;; This implementation normalizes all data as string type to facilitate the
;; data manipulation and storage.
;; This procedures returns a hexdecimal hash number.
(define (calculate-block-hash previous-hash timestamp transaction nonce)
      (bytes->hex-string
       ;; Converts strings to bytes to use with SHA256 procedure
       (sha256 (bytes-append
               (string->bytes/utf-8 previous-hash)
               (string->bytes/utf-8 (number->string timestamp))
               ; Converts a serialized object to string
               (string->bytes/utf-8 (~a (serialize transaction)))
               (string->bytes/utf-8 (number->string nonce))))))

;; Defines a predicate to verify the block validity
(define (valid-block? bl)
  (equal? (block-current-hash bl)
          (calculate-block-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))


;;;;
;;;; The mining process and the PoW system
;;;;

;; Sets the difficult of algorithm.
;; Generates a string which length equal the "difficulty" value and is composed
;; by sequences of "byte" value. The values definitions are arbitrary.
(define difficulty 2)
(define byte 32)
(define target (bytes->hex-string (make-bytes difficulty byte)))

;; Mines a new current block.
;; Note: Here, we kept the implementation proposed in the book, but we thinking
;; that "mine-block" and "make-and-mine-block" can be merged.
(define (mine-block transaction previous-hash)
  ; "current-milliseconds" returns a time value (int) since 00:00UTC/Jan-01-1970
  (make-and-mine-block previous-hash (current-milliseconds) transaction 1))

(define (make-and-mine-block previous-hash timestamp transaction nonce)
  (let ((current-hash (calculate-block-hash
                       previous-hash timestamp transaction nonce)))
    ; Tests if the block is mined. If it is true, returns the new current block.
    ; Otherwise, operates recursively until to find a valid mined block.
    ; The procedure increases the nonce value (adds 1) until SHA256 produces a
    ; hash that matches the target (see "mined-block?" procedure).
    (if (mined-block? current-hash)
        (block current-hash previous-hash transaction timestamp nonce)
        (make-and-mine-block previous-hash timestamp transaction (+ nonce 1)))))


;; A block will be considered mined if the hash matches the target, given the
;; difficulty. Thus, given a random hash, we consider it to be valid if its
;; first "n" bytes, skipping the first sequence (start is arbitrarily 1), match
;; the target, with "n" being the "difficulty" value.
(define (mined-block? block-hash)
  (equal? (subbytes (hex-string->bytes block-hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))


;;;;
;;;; Requirements
;;;;

;; Run 'raco pkg install <package>' or use 'File > Package Manager' in DrRacket
;; to install the package dependecies.
(require (only-in file/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (struct-out block) mine-block valid-block? mined-block?)

