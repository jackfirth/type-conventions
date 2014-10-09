#lang typed/racket

(require (for-syntax syntax/parse "formals-class.rkt"))

(provide define-type-convention
         define-type-conventions
         define-rest-type-convention
         define-rest-type-conventions
         (rename-out [define:: define:]))

;;;; TODO ;;;;
;
;   Allow definition form to define partially-applicable functions, e.g. (define: ((f x y) z) : Number) ...)
;   Create λ-form for anonymous functions with types, e.g. (λ (f x y) ...)
;   Come up with some form to define conventions for parametric types, e.g. (define: (V) (f (thunk V)) ...)
;

; Helper parser

(begin-for-syntax
  (define (convention-parser #:rest [rest #f])
    (syntax-parser
     [(_ type-expr arg-id:id ...)
      (with-syntax ([add-convention-func (if rest #'add-convention-rest #'add-convention)])
        #'(begin-for-syntax
            (add-convention-func #'arg-id #'type-expr) ...))])))

; Single convention forms, each defines one convention for one type

(define-syntax define-type-convention (convention-parser))
(define-syntax define-rest-type-convention (convention-parser #:rest #t))

; Multi convention forms, each defines arbitrarily many conventions for arbitrarily many types

(define-syntax define-type-conventions
  (syntax-parser
   [(_ (type-expr arg-id:id ...) ...)
    #'(begin (define-type-convention type-expr arg-id ...) ...)]))

(define-syntax define-rest-type-conventions
  (syntax-parser
   [(_ (type-expr arg-id:id ...) ...)
    #'(begin (define-rest-type-convention type-expr arg-id ...) ...)]))

; Definition form that recognizes type conventions, shadows define: on export

(define-syntax define::
  (syntax-parser
    [(_ (type-arg:id ...) formals:maybe-type-formals body ...)
     #'(begin
         (: formals.id (All (type-arg ...) formals.contract-expr))
         (define formals.typeless ... body ...))]
    [(_ formals:maybe-type-formals body ...)
     #'(begin
         (: formals.id formals.contract-expr)
         (define formals.typeless ... body ...))]))
