#lang info

(define version "0.2")

(define collection 'multi)

(define deps
  '("base"
    "struct-update-lib"
    "struct-update-doc"))
(define build-deps
  '())

(define implies
  '("struct-update-lib"
    "struct-update-doc"))
