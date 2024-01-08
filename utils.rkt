#lang racket

;;;;
;;;; Introducing Blockchain with Lisp
;;;; Boro Sitnikovski
;;;;
;;;; ISBN-13: 978-1-4842-6969-5
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; File: utils.rkt
;;;;
;;;; Edited by dradicchi at gmail.com
;;;; 2022.09.19
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;;;; Defining some useful procedures
;;;;

;; Tests a list and returns true if only all elements of the list test true to
;; an informed predicate. 
(define (true-for-all? pred list)
  (cond ((empty? list) #t)
        ((pred (car list)) (true-for-all? pred (cdr list)))
        (else #f)))

;; Exports a struct, writing it into a file
(define (struct->file object file)
  ; Replace an existing file with a new one
  (let ((out (open-output-file file #:exists 'replace)))
    (write (serialize object) out)
    (close-output-port out)))

;; Returns a struct that is recovered from a file
(define (file->struct file)
  (letrec ((in (open-input-file file))
           (result (read in)))
    (close-input-port in)
    (deserialize result)))


;;;;
;;;; Requirements
;;;;

;; Run 'raco pkg install <package>' or use 'File > Package Manager' in DrRacket
;; to install the package dependecies.
(require racket/serialize)


;;;;
;;;; Packaging Definitions
;;;;

(provide true-for-all? struct->file file->struct)

