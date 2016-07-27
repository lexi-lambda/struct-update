#lang racket/base

(require rackunit
         rackunit/spec)

(module point racket/base
  (provide (struct-out point))
  (struct point (x y) #:transparent))
  
(module updaters racket/base
  (require (submod ".." point)
           struct-update)
  (provide (struct+updaters-out point))
  (define-struct-updaters point))

(module indirection racket/base
  (require struct-update)
  (provide indirect-define-struct-updaters)
  (define-syntax-rule (indirect-define-struct-updaters name)
    (define-struct-updaters name)))

(describe "define-struct-updaters"
  (it "generates functional setter and updater functions"
    (local-require (submod "." updaters))
    (check-equal? (point-x-set (point 0 0) 5) (point 5 0))
    (check-equal? (point-y-update (point 3 1) add1) (point 3 2)))

  (it "introduces identifiers with the struct-id’s lexical context"
    (local-require (submod "." point)
                   (submod "." indirection))
    (indirect-define-struct-updaters point)
    (void point-x-set)))
