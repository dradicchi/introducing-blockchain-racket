#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: blockchain.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.12
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Defining the blockchain structure
;;;;

;; The "blocks" accepts a list of processed valid blocks and "utxo" accepts a
;; list of IO objects, that represents the unspent transaction outputs, used to
;; resemble the balance of wallets.
(struct blockchain (blocks utxo)
  #:prefab) ; The 'previously fabricated' keyword allows us to display and
            ; serialize this kind of structure.


;;;;
;;;; Blockchain initialization
;;;;


;; Defines a procedure to initialize the blockchain, mining the genesis block.
;; This procedure accepts the genesis transaction and the respective hash, as
;; well the UTXO list.
;;
;; A step recipe to start a blockchain:
;;
;; (1) Sets the "from" and "to" wallets
;;
;;     > (define coinbase (make-wallet))
;;     > (define recipient (make-wallet))
;;
;; (2) Assembles the genesis transaction
;;
;;     > (define genesis-t (make-transaction coinbase recipient 100 '()))
;;
;; (3) Assembles the UTXO list
;;
;;     > (define utxo (list (make-transaction-io 100 recipient)))
;;
;; (4) Finally, starts the new blockchain
;;
;;     > (define blockchain (init-blockchain genesis-t "123abcd" utxo))
;;
(define (init-blockchain t seed-hash utxo)
  (blockchain (cons
                 ; To start the blockchain, we can adopt an arbitrary seed-hash.
                 ; Note: "mine block" returns a block struct that need to insert
                 ; in an empty list.
                 (mine-block (process-transaction t) seed-hash)
                 '())
              utxo))


;;;;
;;;; Blockchain rewards
;;;;

;; Our mining reward starts at 1 coins for the first block
;; and halves on every 210.000 new blocks.
;; Returns a number.
(define (mining-reward-factor blocks)
  ; "(floor x)" returns the largest integer that is no more than x
  (/ 50 (expt 2 (floor (/ (length blocks) 210000)))))


;;;;
;;;; Blockchain growing
;;;;

;; Accepts a blockchain, a value to send as well the sender and recipent
;; wallets. Retuns a new updated blockchain struct.
;;
;; The operational flow to treat a new transaction:
;;
;;     (1) Assemble (make) a new transaction struct
;;
;;     (2) Process this transaction
;;
;;     (3) Mine the new block
;;
;;     (4) Add in the blockchain
;;
(define (send-money-blockchain b from to value)
  (letrec
      ;; Recovers all current UTXO related to sender.
      ;; Note: in transaction processing this implementation uses and
      ;; replace all current UTXO sender objects with new updated similar objects.
      ((my-ts (filter (lambda (t) (equal? from (transaction-io-owner t)))
                      (blockchain-utxo b)))
       ;; Mounts the base transaction
       (t (make-transaction from to value my-ts)))

      (if (transaction? t)
          (let
              ;; Processes the transaction. So if this is valid and the sender
              ;; has sufficient funds includes it in the blockchain.
              ((processed-transaction (process-transaction t)))
              (if (and (>= (balance-wallet-blockchain b from) value)
                       (valid-transaction? processed-transaction))
                (add-transaction-to-blockchain b processed-transaction)
                b))
          (add-transaction-to-blockchain b '())))) 
                               

;; Defines a procedure to appends a new block and respective UTXO.
;; Accepts a initialized blockchain and a new transaction and retuns a new
;; updated blockchain struct.
(define (add-transaction-to-blockchain b t)
  (letrec
      
      ; Gets the new mined block
      ((hashed-blockchain (mine-block t (block-current-hash
                                             (car (blockchain-blocks b)))))

       ; Extracts IO object from the transaction
       (processed-inputs (transaction-inputs t))
       (processed-outputs (transaction-outputs t))

       ; We can treat UTXO list as a set (a list which its members are unique).
       ; Generates a uptaded UTXO list by replacing the old IO objects (which
       ; was used as the fund source in the transaction) for the Output objects,
       ; resulted of transaction processing. 
       (utxo (set-union processed-outputs (set-subtract (blockchain-utxo b)
                                                        processed-inputs)))

       ; Appends the new block in the blockchain block list
       (new-blocks (cons hashed-blockchain (blockchain-blocks b)))

       ; Caculates the mining rewards, generates a IO object and appends it in
       ; the uptaded UTXO list. In this implementation, rewards are paid to the
       ; sender.
       (utxo-rewarded (cons (make-transaction-io
                                (mining-reward-factor new-blocks)
                                (transaction-from t))
                             utxo)))

    ; Returns a updated blockchain struct.
    ; Note that the original blockchain remains unchanged.
    (blockchain new-blocks utxo-rewarded)))


;;;;
;;;; Blockchain validity
;;;;

;; Tests the validity of blockchain structures
(define (valid-blockchain? b)
  (let ([blocks (blockchain-blocks b)])
       (and
        
         ; Tests individually each block, specially if each current-hash block
         ; is valid.
         (true-for-all? valid-block? blocks)
         ; Tests if blocks are correctly ordered in blockchain block list
         (equal? (drop-right (map block-previous-hash blocks) 1)
                 (cdr (map block-current-hash blocks)))
         
         ; Tests individually each transaction about its signature and validity
         ; of outputs.
         ;(true-for-all? valid-transaction?
         ;               (map (lambda (block) (block-transaction block))
         ;                    blocks)) ; Why does it use a lambda statement?

         ; Alternative procedure without lambda statement
         (true-for-all? valid-transaction? (map block-transaction blocks)) 

         ; Tests individually if each block are correctly mined.
         (true-for-all? mined-block? (map block-current-hash blocks)))))


;;;;
;;;; Wallet balance
;;;;

;; Returns the total balance from a wallet
(define (balance-wallet-blockchain b w)
  (letrec
    ; Extracts the UTXO list from the blockchain  
    ((utxo (blockchain-utxo b))
     ; Returns a list of only IO objects related to "w" wallet
     (my-ts (filter (lambda (t) (equal? w (transaction-io-owner t))) utxo)))
    ; Generates a list of balances related to "w" and sums them all up
    (foldr + 0 (map (lambda (t) (transaction-io-value t)) my-ts))))


;;;;
;;;; Requirements
;;;;

(require "block.rkt")
(require "transaction.rkt")
(require "utils.rkt")
(require "wallet.rkt")


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (all-from-out "block.rkt")
         (all-from-out "transaction.rkt")
         (all-from-out "wallet.rkt")
         (struct-out blockchain)
         init-blockchain
         send-money-blockchain
         add-transaction-to-blockchain
         balance-wallet-blockchain
         valid-blockchain?)
