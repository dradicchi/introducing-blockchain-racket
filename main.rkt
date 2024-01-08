#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: main.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.21
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Starts a blockchain
;;;;

;; Checks if there is a blockchain converted file in the project directory
(when (file-exists? "blockchain.data")
  (begin
    (printf "Found 'blockchain.data', reading...\n")
    (print-blockchain (file->struct "blockchain.data"))
    (exit)))

;; Initializes the coin-base and two wallets    
(define coin-base (make-wallet))
(define wallet-a (make-wallet))
(define wallet-b (make-wallet))

;; Assembles the genesis transaction and respective UTXO
(printf "Making genesis transaction...\n")
(define genesis-t (make-transaction coin-base wallet-a 100 '()))
(define utxo (list (make-transaction-io 100 wallet-a)))

;; Proccesses and mines the first block
(printf "Mining genesis block...\n")
(define blockchain (init-blockchain genesis-t "1337cafe" utxo))
(print-wallets blockchain wallet-a wallet-b)


;;;;
;;;; Transaction record
;;;;

;; Second block
;; The book has an error in the value transfer (must be 20 instead of 2)
(printf "Mining second transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-a wallet-b 20))
(print-wallets blockchain wallet-a wallet-b)

;; Third block
;; The book has an error in the value transfer (must be 10 instead of 1)
(printf "Mining third transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-b wallet-a 10))
(print-wallets blockchain wallet-a wallet-b)

;; Fourth attempt. A double spend trying...
;; The book has an error in the value transfer (must be more than 60)
(printf "Attempting to mine fourth (not-valid) transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-b wallet-a 61))
(print-wallets blockchain wallet-a wallet-b)


;;;;
;;;; Checks and store data
;;;;

;; Checks the blockchain
(printf "Blockchain is valid: ~a\n\n" (valid-blockchain? blockchain))
(print-blockchain blockchain)

;; Converts and stores the blockchain
(struct->file blockchain "blockchain.data")
(printf "Exported blockchain to 'blockchain.data'...\n")


;;;;
;;;; Requirements
;;;;

(require "main-helper.rkt")





