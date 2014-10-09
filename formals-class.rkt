#lang racket

(require [for-template typed/racket/base/no-check]
         syntax/parse
         rackunit
         "stx-test-helpers.rkt"
         "stx-helpers.rkt"
         "conventions-data.rkt")

;;;; Syntax classes for parsing define:: forms with default types based on arg names ;;;;

; Only provided forms needed are the main-sequence args class and the optional rest-arg class

(provide maybe-type-formals
         [all-from-out "conventions-data.rkt"]
         [for-template [all-from-out typed/racket/base/no-check]])

; Setup for tests

(module+ test
  (add-convention #'test #'Test)
  (add-convention #'test2 #'Test)
  (add-convention #'test3 #'Test)
  (add-convention-rest #'test #'Test))

; General form, takes a function to turn an id into a type expression as a parameter

(define-syntax-class (maybe-type-arg ref-convention-func)
  (pattern (~or id:id [id:id : type-expr])
           #:attr type
           (if (attribute type-expr) #'type-expr (ref-convention-func #'id))))

; Normal form

(define-syntax-class maybe-type-arg-norm
  (pattern (~var arg (maybe-type-arg ref-convention))
           #:attr id #'arg.id
           #:attr type #'arg.type))

(module+ test
  (test-syntax-class maybe-type-arg-norm
    #'test
    #'[test : Test]
    #'[test : (Test V)]
    #'[test : (-> Number Number)]))

; Rest form

(define-syntax-class maybe-type-arg-rest
  (pattern (~var arg (maybe-type-arg ref-convention-rest))
           #:attr id #'arg.id
           #:attr type #'arg.type))

(module+ test
  (test-syntax-class maybe-type-arg-rest
    #'test
    #'[test : Test]
    #'[test : (Test V)]
    #'[test : (-> Number Number)]))

; Keyword argument form

(define-splicing-syntax-class maybe-type-kwarg
  (pattern (~seq kw:keyword arg:maybe-type-arg-norm)
           #:attr id #'arg.id
           #:attr type #'arg.type))

(module+ test
  (test-splicing-syntax-class maybe-type-kwarg
    #'(#:test test)
    #'(#:test [test : Test])
    #'(#:test [test : (Test V)])
    #'(#:test [test : (-> Number Number)])))

; Optional argument form

(define-syntax-class maybe-type-arg-opt
  (pattern (~or [id:id default-expr] [id:id : type-expr default-expr])
           #:attr type
           (if (attribute type-expr) #'type-expr (ref-convention #'id))
           #:attr form #'[id default-expr]))

(module+ test
  (test-syntax-class maybe-type-arg-opt
    #'[test 1]
    #'[test : Test 1]
    #'[test : (Test V) 1]
    #'[test : (-> Number Number) 1]))

; Optional keyword argument form

(define-splicing-syntax-class maybe-type-kwarg-opt
  (pattern (~seq kw:keyword kwarg:maybe-type-arg-opt)
           #:attr id #'kwarg.id
           #:attr type #'kwarg.type
           #:attr form #'kwarg.form))

(module+ test
  (test-splicing-syntax-class maybe-type-kwarg-opt
    #'(#:test [test 1])
    #'(#:test [test : Test 1])
    #'(#:test [test : (Test V) 1])
    #'(#:test [test : (-> Number Number) 1])))

; Helper for stitching together the attributes of normal args and keywords args in the maybe-type-args class

(define (append-args norm-args keywords1 keywords2 kwargs1 kwargs2)
  (append (syntax->list norm-args)
          (zip-stxs-flat (append-stxs keywords1 keywords2)
                         (append-stxs kwargs1 kwargs2))))

(module+ test
  (check-stx-list-datum
   (append-args #'(1 2 3) #'(#:a #:b) #'(#:c #:d) #'(a b) #'(c d))
   (syntax->list #'(1 2 3 #:a a #:b b #:c c #:d d))))

; Splicing syntax class for a sequence of args

(define-splicing-syntax-class maybe-type-args
  (pattern
   (~seq (~or kwarg1:maybe-type-kwarg
              opt-kwarg1:maybe-type-kwarg-opt
              arg:maybe-type-arg-norm)
         ...
         (~or kwarg2:maybe-type-kwarg
              opt-kwarg2:maybe-type-kwarg-opt
              opt-arg:maybe-type-arg-opt)
         ...)
   #:attr [types 1]
   (append-args #'(arg.type ...) #'(kwarg1.kw ...) #'(kwarg2.kw ...) #'(kwarg1.type ...) #'(kwarg2.type ...))
   #:attr [opt-types 1]
   (append-args #'(opt-arg.type ...) #'(opt-kwarg1.kw ...) #'(opt-kwarg2.kw ...) #'(opt-kwarg1.type ...) #'(opt-kwarg2.type ...))
   #:attr [forms 1]
   (append-args #'(arg.id ...) #'(kwarg1.kw ...) #'(kwarg2.kw ...) #'(kwarg1.id ...) #'(kwarg2.id ...))
   #:attr [opt-forms 1]
   (append-args #'(opt-arg.form ...) #'(opt-kwarg1.kw ...) #'(opt-kwarg2.kw ...) #'(opt-kwarg1.form ...) #'(opt-kwarg2.form ...))))

(module+ test
  (test-splicing-syntax-class maybe-type-args
    #'(test)
    #'(test test2)
    #'([test : Test])
    #'(test [test2 : Test])
    #'([test 1])
    #'([test : Test 1])
    #'(test [test2 : Test 1])
    #'(test [test2 1])
    #'(#:test test)
    #'(#:test test test2)
    #'(#:test [test : Test])
    #'(#:test [test 1])
    #'(#:test [test : Test 1])
    #'(#:test [test 1] test2)
    #'(#:test [test : Test 1] test2)
    #'([test : (-> Test Test)])))

; Class for a full set of formals with a named identifier

(define-splicing-syntax-class maybe-type-formals
  (pattern
   (~seq (~or (id:id args:maybe-type-args ~rest rest-args:maybe-type-arg-rest)
              (id:id args:maybe-type-args))
         :
         result-type)
   #:attr [typeless 1]
   (list (if (attribute rest-args)
             #'(id args.forms ... args.opt-forms ... . rest-args.id)
             #'(id args.forms ... args.opt-forms ...))
         #':
         #'result-type)
   #:attr contract-expr
   (if (attribute rest-args)
       #'(->* (args.types ...) (args.opt-types ...) #:rest rest-args.type result-type)
       #'(->* (args.types ...) (args.opt-types ...) result-type))))

(module+ test
  
  (syntax-parse #'(define (f test) : Test test)
    [(_ formals:maybe-type-formals body ...)
     (begin
       (displayln #'formals)
       #t)])
  
  (test-splicing-syntax-class maybe-type-formals
    #'((f test) : Any)
    #'((f [test : Test]) : Any)

    #'((f [test 1]) : Any)
    #'((f [test : Test 1]) : Any)

    #'((f #:test test) : Any)
    #'((f #:test [test : Test]) : Any)

    #'((f #:test [test 1]) : Any)
    #'((f #:test [test : Test 1]) : Any)
    
    #'((f . test) : Any))
  
  (test-splicing-syntax-class-list-attribute maybe-type-formals typeless
    
    [#'((f test) : Any) (list #'(f test) #': #'Any)]
    [#'((f [test : Test]) : Any) (list #'(f test) #': #'Any)]
    
    [#'((f [test 1]) : Any) (list #'(f [test 1]) #': #'Any)]
    [#'((f (test : Test 1)) : Any) (list #'(f [test 1]) #': #'Any)]
    
    [#'((f #:test test) : Any) (list #'(f #:test test) #': #'Any)]
    [#'((f #:test [test : Test]) : Any) (list #'(f #:test test) #': #'Any)]
    
    [#'((f #:test [test 1]) : Any) (list #'(f #:test [test 1]) #': #'Any)]
    [#'((f #:test [test : Test 1]) : Any) (list #'(f #:test [test 1]) #': #'Any)]
    
    [#'((f . test) : Any) (list #'(f . test) #': #'Any)]
    
    [#'((f test #:test2 [test2 : Test 1] . test) : Any) (list #'(f test #:test2 [test2 1] . test) #': #'Any)])
  
  (test-splicing-syntax-class-attribute maybe-type-formals contract-expr
    [#'((f test) : Any) #'(->* (Test) () Any)]
    [#'((f [test : Test]) : Any) #'(->* (Test) () Any)]
    
    [#'((f [test 1]) : Any) #'(->* () (Test) Any)]
    [#'((f [test : Test 1]) : Any) #'(->* () (Test) Any)]
    
    [#'((f #:test test) : Any) #'(->* (#:test Test) () Any)]
    [#'((f #:test [test : Test]) : Any) #'(->* (#:test Test) () Any)]
    
    [#'((f #:test [test 1]) : Any) #'(->* () (#:test Test) Any)]
    [#'((f #:test [test : Test 1]) : Any) #'(->* () (#:test Test) Any)]
    
    [#'((f . test) : Any) #'(->* () () #:rest Test Any)]))