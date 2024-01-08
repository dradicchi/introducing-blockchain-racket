#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: smart-contracts.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.10.24
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Introducing Smart Contracts
;;;;
;;;; Smart Contracts are a underlying concept of the cryptoeconomy, derived from
;;;; the programmable ability of most criptocurrencies, including Bitcoin and
;;;; this project. That is, users can write scripts (short pieces of code) to
;;;; define requirements that must be satisfied to a transaction can be
;;;; completed. Typically, crypto projects adopts a special purpose scripting
;;;; language to allow non-programmers users to write their own contracts
;;;; easily.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Tests a transaction and a related contract concomitantly
(define (valid-transaction-contract? t c)
  (and (eval-contract t c)
       (valid-transaction? t)))

;; Defines a procedure to evaluate a contract assembled with a basic
;; S-expression. Receives a transaction and a contract and returns a number,
;; a string, a true/false value or a transaction data. Some match clauses adopts
;; a recursive implementation to be able to evaluate nested S-expressions.
(define (eval-contract t c)
  (match c
    ((? number? x) x)
    ((? string? x) x)
    (`() #t)
    (`true #t)
    (`false #f)
    (`(if ,co ,tr ,fa) (if (eval-contract t co)
                           (eval-contract t tr)
                           (eval-contract t fa)))
    (`(+ ,l ,r) (+ (eval-contract t l) (eval-contract t r)))
    (`from (transaction-from t))
    (`to (transaction-to t))
    (`value (transaction-value t))
    (`(+ ,l ,r) (+ (eval-contract t l) (eval-contract t r)))
    (`(* ,l ,r) (* (eval-contract t l) (eval-contract t r)))
    (`(- ,l ,r) (- (eval-contract t l) (eval-contract t r)))
    (`(= ,l ,r) (= (eval-contract t l) (eval-contract t r)))
    (`(> ,l ,r) (> (eval-contract t l) (eval-contract t r)))
    (`(< ,l ,r) (< (eval-contract t l) (eval-contract t r)))
    (`(and ,l ,r) (and (eval-contract t l) (eval-contract t r)))
    (`(or ,l ,r) (or (eval-contract t l) (eval-contract t r)))
    (else #f)))


;;;;
;;;; Requirements
;;;;

(require "transaction.rkt")


;;;;
;;;; Packaging Definitions
;;;;

(provide valid-transaction-contract?)
