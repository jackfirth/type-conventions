#lang racket

(provide syntax-or-list->list
         map-syntax
         zip-flat
         zip-stxs-flat
         append-stxs)

;;;; Syntax helper functions ;;;;

; Turn a syntax object or a list of syntax objects into a list of syntax objects

(define (syntax-or-list->list stx-or-list)
  (if (list? stx-or-list)
      stx-or-list
      (syntax->list stx-or-list)))

; Map a function to a syntax object's contents

(define (map-syntax f stxs) (map f (syntax->list stxs)))

; Zip lists together, then flatten zipped lists to one list
; (zip-flat '(1 2 3) '(a b c)) -> '(1 a 2 b 3 c)

(define (zip-flat . lsts) (apply append (apply map (cons list lsts))))

; Does the same, but for syntax objects or lists of syntax objects
; (zip-stxs-flat #'(1 2 3) (list #'a #'b #'c)) -> (list #'1 #'a #'2 #'b #'3 #'c)

(define (zip-stxs-flat . stxs) (apply zip-flat (map syntax-or-list->list stxs)))

; Append the contents of syntax objects into a list of syntax objects
; (append-stxs #'(1 2 3) #'(a b c)) -> (list #'1 #'2 #'3 #'a #'b #'c)

(define (append-stxs . stxs) (apply append (map syntax->list stxs)))

