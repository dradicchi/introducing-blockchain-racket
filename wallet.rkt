#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: wallet.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.09.12
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Introducing Wallets
;;;;
;;;; A wallet is defined as a pair of a private and a public keys, that are used
;;;; to send or receive amounts respectively. You use the private key to assign
;;;; transactions wich sending your coins to other wallets. In same way, you
;;;; provide the public key so that third-party wallets send coins to you. The
;;;; public key also permits to investigate the blockchain and find out the
;;;; balance of the wallet.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; The wallet structure
;;;;

(struct wallet
  (private-key public-key)
  #:prefab) ; The 'previously fabricated' keyword allows us to display and
            ; serialize this kind of structure.


;;;;
;;;; Initializing a wallet
;;;;

;; Inits a wallet by generating and serializing your private/public keys.
;; This procedure returns a wallet type.
(define (make-wallet)
  (letrec
      ;; Using a RSA implementation algorithm to generates the keys
      ((rsa-impl (get-pk 'rsa libcrypto-factory))
       (privkey (generate-private-key rsa-impl '((nbits 512))))
       (pubkey (pk-key->public-only-key privkey)))

    (wallet
     ;; Serializes the private and public keys to bytes
     (bytes->hex-string (pk-key->datum privkey 'PrivateKeyInfo))
     (bytes->hex-string (pk-key->datum pubkey 'SubjectPublicKeyInfo)))))


;;;;
;;;; Requirements
;;;;

;; Run 'raco pkg install <package>' or use 'File > Package Manager' in DrRacket
;; to install the package dependecies.
(require crypto)
(require crypto/all)


;;;;
;;;; Packaging Definitions
;;;;

;; Exporting the struct to allow access at your built-in procedures
(provide (struct-out wallet) make-wallet)

