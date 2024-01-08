#lang racket
;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: test-blockchain-operating.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.19
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Tests blockchain operating
;;;;

(define (test-blockchain-op)
  (letrec

    ; Creates a coinbase wallet as well two common wallets
    ((coinb (make-wallet)) 
     (wl-01 (make-wallet))
     (wl-02 (make-wallet))
     (wl-03 (make-wallet))

     ; Assembles the genesis transaction and its resultant UTXO list
     (genesis-t (make-transaction coinb wl-01 10000 '()))
     (utxo (list (make-transaction-io 10000 wl-01)))

     ; Starts the blockchain.
     ; This statement processes the genesis transaction and mines the first
     ; block. We adopted an arbitrary seed-hash.
     (blockchain-001 (init-blockchain genesis-t "123abcd" utxo))
     
     ; Assembles and processes another simple transaction and adds a
     ; transaction/block with "add-transaction-to-blockchain".
     ;(tx002 (process-transaction
     ;               (make-transaction wl-01 wl-02 40
     ;                 (filter (lambda (t) (equal? wl-01
     ;                                             (transaction-io-owner t)))
     ;                         (blockchain-utxo blockchain-001)))))
    
     ;(blockchain-002 (add-transaction-to-blockchain blockchain-001 tx002))

     ; Sends money with "send-money-blockchain"
     (blockchain-002 (send-money-blockchain blockchain-001 wl-01 wl-02 4000))

     ; Sends money with "send-money-blockchain"
     (blockchain-003 (send-money-blockchain blockchain-002 wl-01 wl-03 2500))     

     ; Sends money with "send-money-blockchain"
     (blockchain-004 (send-money-blockchain blockchain-003 wl-03 wl-02 1000))

     ; Attempts to send more money than is in a wallet (double spend test)
     (blockchain-005 (send-money-blockchain blockchain-004 wl-01 wl-02 50000)))

     ;(blockchain-006 (add-transaction-to-blockchain blockchain-005 '())))

    
    ; Prints the reports
    (begin
        ; Report 01
        (displayln "-------- Int 001 ---------")
        (display "Is valid? ")
        (displayln (valid-blockchain? blockchain-001))
        (display "TXs #: ")
        (displayln (length (blockchain-blocks blockchain-001)))
        (display "UTXOs #: ")
        (displayln (length (blockchain-utxo blockchain-001)))
        (display "WL-01 UTXO list: ")
        (displayln (utxo-list blockchain-001 wl-01))
        (display "WL-02 UTXO list: ")
        (displayln (utxo-list blockchain-001 wl-02))
        (display "WL-03 UTXO list: ")
        (displayln (utxo-list blockchain-001 wl-03))         
        (display "WL-01 balance: ")
        (displayln (balance-wallet-blockchain blockchain-001 wl-01))
        (display "WL-02 balance: ")
        (displayln (balance-wallet-blockchain blockchain-001 wl-02))
        (display "WL-03 balance: ")
        (displayln (balance-wallet-blockchain blockchain-001 wl-03))        
        ; Report 02
        (displayln "-------- Int 002 ---------")
        (display "Is valid? ")
        (displayln (valid-blockchain? blockchain-002))
        (display "TXs #: ")
        (displayln (length (blockchain-blocks blockchain-002)))
        (display "UTXOs #: ")
        (displayln (length (blockchain-utxo blockchain-002)))
        (display "WL-01 UTXO list: ")
        (displayln (utxo-list blockchain-002 wl-01))
        (display "WL-02 UTXO list: ")
        (displayln (utxo-list blockchain-002 wl-02))
        (display "WL-03 UTXO list: ")
        (displayln (utxo-list blockchain-002 wl-03))        
        (display "WL-01 balance: ")
        (displayln (balance-wallet-blockchain blockchain-002 wl-01))
        (display "WL-02 balance: ")
        (displayln (balance-wallet-blockchain blockchain-002 wl-02))
        (display "WL-03 balance: ")
        (displayln (balance-wallet-blockchain blockchain-002 wl-03))        
        ; Report 03
        (displayln "-------- Int 003 ---------")
        (display "Is valid? ")
        (displayln (valid-blockchain? blockchain-003))
        (display "TXs #: ")
        (displayln (length (blockchain-blocks blockchain-003)))
        (display "UTXOs #: ")
        (displayln (length (blockchain-utxo blockchain-003)))
        (display "WL-01 UTXO list: ")
        (displayln (utxo-list blockchain-003 wl-01))
        (display "WL-02 UTXO list: ")
        (displayln (utxo-list blockchain-003 wl-02))
        (display "WL-03 UTXO list: ")
        (displayln (utxo-list blockchain-003 wl-03))         
        (display "WL-01 balance: ")
        (displayln (balance-wallet-blockchain blockchain-003 wl-01))
        (display "WL-02 balance: ")
        (displayln (balance-wallet-blockchain blockchain-003 wl-02))
        (display "WL-03 balance: ")
        (displayln (balance-wallet-blockchain blockchain-003 wl-03))        
        ; Report 04
        (displayln "-------- Int 004 ---------")
        (display "Is valid? ")
        (displayln (valid-blockchain? blockchain-004))
        (display "TXs #: ")
        (displayln (length (blockchain-blocks blockchain-004)))
        (display "UTXOs #: ")
        (displayln (length (blockchain-utxo blockchain-004)))   
        (display "WL-01 UTXO list: ")
        (displayln (utxo-list blockchain-004 wl-01))
        (display "WL-02 UTXO list: ")
        (displayln (utxo-list blockchain-004 wl-02))
        (display "WL-03 UTXO list: ")
        (displayln (utxo-list blockchain-004 wl-03))        
        (display "WL-01 balance: ")
        (displayln (balance-wallet-blockchain blockchain-004 wl-01))
        (display "WL-02 balance: ")
        (displayln (balance-wallet-blockchain blockchain-004 wl-02))
        (display "WL-03 balance: ")
        (displayln (balance-wallet-blockchain blockchain-004 wl-03))
        ; Report 05
        (displayln "-------- Int 005 ---------")
        (display "Is valid? ")
        (displayln (valid-blockchain? blockchain-005))
        (display "TXs #: ")
        (displayln (length (blockchain-blocks blockchain-005)))
        (display "UTXOs #: ")
        (displayln (length (blockchain-utxo blockchain-005)))   
        (display "WL-01 UTXO list: ")
        (displayln (utxo-list blockchain-005 wl-01))
        (display "WL-02 UTXO list: ")
        (displayln (utxo-list blockchain-005 wl-02))
        (display "WL-03 UTXO list: ")
        (displayln (utxo-list blockchain-005 wl-03))        
        (display "WL-01 balance: ")
        (displayln (balance-wallet-blockchain blockchain-005 wl-01))
        (display "WL-02 balance: ")
        (displayln (balance-wallet-blockchain blockchain-005 wl-02))
        (display "WL-03 balance: ")
        (displayln (balance-wallet-blockchain blockchain-005 wl-03))
        ; Report 06
        ;(displayln "-------- Int 006 ---------")
        ;(display "Is valid? ")
        ;(displayln (valid-blockchain? blockchain-006))
        ;(display "TXs #: ")
        ;(displayln (length (blockchain-blocks blockchain-006)))
        ;(display "UTXOs #: ")
        ;(displayln (length (blockchain-utxo blockchain-006)))   
        ;(display "WL-01 UTXO list: ")
        ;(displayln (utxo-list blockchain-006 wl-01))
        ;(display "WL-02 UTXO list: ")
        ;(displayln (utxo-list blockchain-006 wl-02))
        ;(display "WL-03 UTXO list: ")
        ;(displayln (utxo-list blockchain-006 wl-03))        
        ;(display "WL-01 balance: ")
        ;(displayln (balance-wallet-blockchain blockchain-006 wl-01))
        ;(display "WL-02 balance: ")
        ;(displayln (balance-wallet-blockchain blockchain-006 wl-02))
        ;(display "WL-03 balance: ")
        ;(displayln (balance-wallet-blockchain blockchain-006 wl-03))
        ; Ends the test
        (displayln "-------- SUMMARY --------")
        (print-blockchain blockchain-005)
        (displayln "-------- END ------------"))))


;;;;
;;;; Support procedures
;;;;

;; Generates a list with values from UTXO objects to a specific wallet
(define (utxo-list b w)
  (map transaction-io-value
   (filter (lambda (t) (equal? w (transaction-io-owner t)))
                (blockchain-utxo b))))


;;;;
;;;; Requirements
;;;;

;; Requiring all RKT files
(require "blockchain.rkt")
(require "main-helper.rkt")






