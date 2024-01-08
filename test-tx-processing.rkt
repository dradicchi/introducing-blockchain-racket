#lang racket
;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: test-tx-processing.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.11
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; A transaction processing test 
;;;;

(define (test-tx-processing)
  (letrec
      
      ; Creates two test wallets
      ((wl01 (make-wallet)) ; Sender (From)
       (wl02 (make-wallet)) ; Recipient (To)
       
       ; Generates an input objects list for the test transaction
       (txin-wl01 (list (make-transaction-io 100.00 wl01)
                        (make-transaction-io 50.00 wl01)))
       
       ; Inits the test transaction.
       (tx-wl01->wl02 (make-transaction wl01 wl02 130.00 txin-wl01))

       ; Process the test transaction
       (txp-wl01->wl02 (process-transaction tx-wl01->wl02)))
      
      (begin
        ; Prints a report
        (displayln "-------- TEST RESULT ---------")
        (display "wallets : ")
        (displayln (true-for-all? wallet? (list wl01 wl02)))
        (display "txin-wl01 : ")
        (displayln (true-for-all? valid-transaction-io? txin-wl01))
        (display "tx-wl01->wl02 : ")
        (displayln (transaction? tx-wl01->wl02))
        (display "txp-wl01->wl02 : ")
        (displayln (valid-transaction? txp-wl01->wl02))
        (display "Outputs value: ")
        (displayln (map (lambda (tx-io) (transaction-io-value tx-io))
                        (transaction-outputs txp-wl01->wl02)))
        (displayln "------------------------------"))))


;;;;
;;;; Requirements
;;;;

;; Requiring all RKT files
(require "wallet.rkt")
(require "utils.rkt")
(require "transaction-io.rkt")
(require "transaction.rkt")


