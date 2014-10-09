#lang typed/racket

(require "type-args.rkt"
         typed/rackunit)

; Definition of types and their conventional argument forms

(define-type Bit (U Zero One))

(define-type-conventions
  [Boolean bool]
  [Bit bit]
  [(Listof Bit) bits]
  [(Pairof Bit (Listof Bit)) nonempty-bits])

; Rest args commonly pluralize types because a rest arg declared to have type T is bound to a value of type (Listof T)

(define-rest-type-conventions
  [Bit bits])

;;;; These definitions have strongly typed arguments, where types are assumed based on the defined arg conventions ;;;;

(define: (flip bit) : Bit
  (if (zero? bit) 1 0))

(define: (bit->bool bit) : Boolean
  (not (zero? bit)))

(define: (bool->bit bool) : Bit
  (if bool 1 0))

(module+ test
  (check-eqv? (flip 1) 0)
  (check-eqv? (flip 0) 1)
  (check-eq? (bit->bool 1) #t)
  (check-eq? (bit->bool 0) #f)
  (check-eqv? (bool->bit #t) 1)
  (check-eqv? (bool->bit #f) 0))

; Conventions can be overridden if necessary, though this probably isn't a good idea to do if you can avoid it

(define: (flip-bool [bit : Boolean]) : Boolean
  (not bit))

(module+ test
  (check-eq? (flip-bool #t) #f)
  (check-eq? (flip-bool #f) #t))

; Rest arg form expands properly to [bits : Bit] and does not clash with the [bits : (Listof Bit)] type for non-rest args

(define: (bit-or . bits) : Bit
  (bool->bit (ormap bit->bool bits)))

(module+ test
  (check-eqv? (bit-or 1 0 0) 1)
  (check-eqv? (bit-or 0 0 0) 0)
  (check-eqv? (bit-or 0 1 0) 1)
  (check-eqv? (bit-or 1 1 1) 1))

; Conventions are particularly useful for more complex types. nonempty-bits is of type (Pairof Bit (Listof Bit)), thereby typechecking that
; this function is never passed an empty list

(define: (first-bit nonempty-bits) : Bit
  (first nonempty-bits))

(module+ test
  (check-eqv? (first-bit '(0)) 0)
  (check-eqv? (first-bit '(1 0 1)) 1))

; Keyword and optional arguments expand properly, and non-convention args can be provided. Convention args have no support for type parameters,
; but non-convention args still function properly with them. You can override a convention to use a parametric type if necessary

(define: (A B) (bit-if [thunk-f : (-> A)]
                       [thunk-g : (-> B)]
                       #:bit [bit 1]) : (U A B)
  (if (bit->bool bit) (thunk-f) (thunk-g)))

(module+ test
  (check-eq? (bit-if (thunk 'a) (thunk 'b)) 'a)
  (check-eq? (bit-if (thunk 'a) (thunk 'b) #:bit 0) 'b)
  (check-eq? (bit-if (thunk 'a) (thunk 'b) #:bit 1) 'a))

; Non-rest arg bits was defined to have type (Listof Bit) and expands properly despite having the same name as the rest-arg convention
; Rest arg and normal arg convention names are deliberately differentiated.

(define: (V) (map-bits [f : (-> Bit V)] bits) : (Listof V)
  (map f bits))

(module+ test
  (check-equal? (map-bits bit->bool '(1 0 0 1)) '(#t #f #f #t)))