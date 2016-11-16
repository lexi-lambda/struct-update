#lang racket/base

(require struct-update
         rackunit
         rackunit/spec)

(struct super (a b c) #:transparent)
(struct sub super (c a t) #:transparent)
(struct subsub sub (b a) #:transparent)

(define-struct-updaters super)
(define-struct-updaters sub)
(define-struct-updaters subsub)

(describe "define-struct-updaters"
  ;; Updaters for non-super-parts of instances of structs
  (it "generates functional setter and updater functions"
    (check-equal? (super-a-set (super 0 1 2) 5) (super 5 1 2))
    (check-equal? (super-b-update (super 3 1 4) add1) (super 3 2 4)))
  (it "generates setter and updater functions for the sub struct"
    (check-equal? (sub-a-set (sub 0 1 2 3 4 5) 6) (sub 0 1 2 3 6 5))
    (check-equal? (sub-c-update (sub 3 1 4 1 5 9) add1) (sub 3 1 4 2 5 9)))
  (it "generates setters and updaters for the subsub struct"
    (check-equal? (subsub-a-set (subsub 0 1 2 3 4 5 6 7) 8)
                  (subsub 0 1 2 3 4 5 6 8))
    (check-equal? (subsub-b-update (subsub 3 1 4 1 5 9 2 7) add1)
                  (subsub 3 1 4 1 5 9 3 7)))
  
  ;; Updaters for super-parts of instances of sub-structs
  (it "generates setters and updaters for super-fields within the sub struct"
    (check-equal? (sub-super-a-set (sub 0 1 2 3 4 5) 6)
                  (sub 6 1 2 3 4 5))
    (check-equal? (sub-super-c-update (sub 1 2 4 8 7 6) add1)
                  (sub 1 2 5 8 7 6)))
  (it "generates setters and updaters for sub-fields within the subsub struct"
    (check-equal? (subsub-sub-c-set (subsub 0 1 2 3 4 5 6 7) 8)
                  (subsub 0 1 2 8 4 5 6 7))
    (check-equal? (subsub-sub-a-update (subsub 3 1 4 1 5 9 2 7) add1)
                  (subsub 3 1 4 1 6 9 2 7)))
  (it "generates setters and updaters for super-fields within the subsub struct"
    (check-equal? (subsub-super-c-set (subsub 0 1 2 3 4 5 6 7) 8)
                  (subsub 0 1 8 3 4 5 6 7))
    (check-equal? (subsub-super-a-update (subsub 3 1 4 1 5 9 2 7) add1)
                  (subsub 4 1 4 1 5 9 2 7)))
  )
