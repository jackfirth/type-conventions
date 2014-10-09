#lang racket

(provide add-convention
         add-convention-rest
         ref-convention
         ref-convention-rest)

;;;; Each convention maps a symbol representing a typed arg to a symbol representing a type ;;;;

(define conventions (make-hash))
(define conventions-rest (make-hash))

(define ((add conventions) arg-stx type-stx)
  (hash-set! conventions (syntax->datum arg-stx) (syntax->datum type-stx)))

(define add-convention (add conventions))
(define add-convention-rest (add conventions-rest))

(define ((ref conventions kind) arg-stx)
  (datum->syntax arg-stx
                 (hash-ref conventions
                           (syntax->datum arg-stx)
                           (thunk
                            (raise-syntax-error 'type-conventions
                                                (format "no ~a convention '~a'"
                                                        kind
                                                        (syntax->datum arg-stx))
                                                arg-stx)))))

(define ref-convention (ref conventions "argument"))
(define ref-convention-rest (ref conventions-rest "rest argument"))