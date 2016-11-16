#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/provide-transform
                     racket/syntax
                     (rename-in syntax/parse [attribute @])
                     syntax/parse/class/struct-id))

(provide define-struct-updaters
         struct-updaters-out
         struct+updaters-out)

; Unfortunately, struct-copy from racket/base does not work here, so we have to write some hairy
; positional update logic ourselves. The struct-copy form assigns significance to identifier names to
; allow users to specify fields without qualifying them completely, passing something like ‘x’ instead
; of ‘point-x’. In addition to causing problems if a structure type binding is renamed, it prevents
; updating supertype fields. This sacrifice is made for some additional convenience when used by a
; programmer directly, but it isn’t robust enough to use as the implementation of another macro. See
; racket/racket#1399 for a discussion of the issue.

(begin-for-syntax
  (define-syntax-class struct-id/set+update
    #:description #f
    #:attributes [constructor-id [set-id 1] [update-id 1] [accessor-id 1]
                  [pre-accessor-id 2] [post-accessor-id 2]]
    [pattern struct:struct-id
      ; we can’t actually copy a struct without having access to all its fields, since we need to be
      ; able to copy over all of its old values when we construct a new one
      #:fail-unless (@ struct.all-fields-visible?)
                    "not all structure fields are visible from the structure type"
      ; we need to synthesize the -set and -update identifiers, and we need to provide enough
      ; information with them such that we can properly copy over the other fields when we actually
      ; perform the functional update
      #:attr constructor-id (@ struct.constructor-id)
      #:with ([set-id update-id accessor-id [pre-accessor-id ...] [post-accessor-id ...]] ...)
             (for/list ([(accessor-id index) (in-indexed (in-list (@ struct.accessor-id)))])
                (let-values ([[pre current+post] (split-at (@ struct.accessor-id)
                                                           index)])
                  (cond
                    [(< index (@ struct.num-supertype-fields))
                     (list (format-id #'struct "~a-~a-set" #'struct accessor-id #:source #'struct #:props #'struct)
                           (format-id #'struct "~a-~a-update" #'struct accessor-id #:source #'struct #:props #'struct)
                           accessor-id pre (rest current+post))]
                    [else
                     (list (format-id #'struct "~a-set" accessor-id #:source #'struct #:props #'struct)
                           (format-id #'struct "~a-update" accessor-id #:source #'struct #:props #'struct)
                           accessor-id pre (rest current+post))])))]))

(define-syntax (define-struct-updaters stx)
  (with-disappeared-uses
   (syntax-parse stx
     [(_ struct:struct-id/set+update)
      (record-disappeared-uses (list #'struct))
      #'(begin
          (begin
            (define (struct.set-id instance value)
              (struct.constructor-id (struct.pre-accessor-id instance) ...
                                     value
                                     (struct.post-accessor-id instance) ...))
            (define (struct.update-id instance proc)
              (struct.constructor-id (struct.pre-accessor-id instance) ...
                                     (proc (struct.accessor-id instance))
                                     (struct.post-accessor-id instance) ...)))
          ...)])))

(define-syntax struct-updaters-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ struct:struct-id/set+update)
        (expand-export #'(combine-out struct.set-id ... struct.update-id ...) modes)]))))

(define-syntax struct+updaters-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ struct:struct-id/set+update)
        (expand-export #'(combine-out (struct-out struct) (struct-updaters-out struct)) modes)]))))
