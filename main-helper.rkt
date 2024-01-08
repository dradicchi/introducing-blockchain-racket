#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: main-helper.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.21
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Printing the blockchain
;;;;

;; Formats a readable summary for a transaction
(define (format-transaction t)
  (format "...~a... sends ...~a... an amount of ~a."
          (substring (wallet-public-key (transaction-from t)) 64 80)
          (substring  (wallet-public-key  (transaction-to t)) 64 80)
          (transaction-value t)))

;; Prints block details
(define (print-block bl)
  (printf "Block information\n=================Hash:\t~a\nHash_p:\t~a\nStamp:\
t~a\nNonce:\t~a\nData: \t~a\n"
          (block-current-hash bl)
          (block-previous-hash bl)
          (block-timestamp bl)
          (block-nonce bl)
          (format-transaction (block-transaction bl))))

;; Prints the entire blockchain
(define (print-blockchain b)
  (for ((block (blockchain-blocks b)))
       (print-block block)
       (newline)))

;; Prints the balance for two given wallets
(define (print-wallets b wallet-a wallet-b)
  (printf "\nWallet A balance: ~a\nWallet B balance: ~a\n\n"
          (balance-wallet-blockchain b wallet-a)
          (balance-wallet-blockchain b wallet-b)))

  
;;;;
;;;; Requirements
;;;;

(require "blockchain.rkt")
(require "utils.rkt")


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (all-from-out "blockchain.rkt")
         (all-from-out "utils.rkt")
         format-transaction
         print-block
         print-blockchain
         print-wallets)
