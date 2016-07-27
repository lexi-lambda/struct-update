#lang info

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
