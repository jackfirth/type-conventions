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

(define ((ref conventions) arg-stx)
  (datum->syntax arg-stx (hash-ref conventions (syntax->datum arg-stx))))

(define ref-convention (ref conventions))
(define ref-convention-rest (ref conventions-rest))