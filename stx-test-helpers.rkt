#lang racket

(require rackunit
         syntax/parse
         racket/syntax
         (for-syntax syntax/parse
                     racket/syntax))

(provide syntax-parses?
         syntax-class-parses?
         syntax-splicing-class-parses?
         check-syntax-class
         check-splicing-syntax-class
         test-syntax-class
         test-splicing-syntax-class
         syntax-datum-equal?
         check-stx-datum
         syntax-list-datum-equal?
         check-stx-list-datum
         check-syntax-class-attribute
         test-syntax-class-attribute
         check-splicing-syntax-class-attribute
         test-splicing-syntax-class-attribute
         check-syntax-class-list-attribute
         test-syntax-class-list-attribute
         check-splicing-syntax-class-list-attribute
         test-splicing-syntax-class-list-attribute)

; Syntax parse predicate macros

(define-syntax-rule (syntax-parses? stx pattern)
  (syntax-parse stx [pattern #t] [_ #f]))

(define-syntax-rule (syntax-class-parses? stx class)
  (syntax-parses? stx (~var a class)))

(define-syntax-rule (syntax-splicing-class-parses? stx class)
  (syntax-parses? stx ((~var a class))))

; Syntax parse check marcos

(define-syntax-rule (check-syntax-class class stx)
  (check-true (syntax-class-parses? stx class)))

(define-syntax-rule (check-splicing-syntax-class class stx)
  (check-true (syntax-splicing-class-parses? stx class)))

; Syntax parse test macros

(define-syntax-rule (test-syntax-class class stx ...)
  (test-begin (check-syntax-class class stx) ...))

(define-syntax-rule (test-splicing-syntax-class class stx ...)
  (test-begin (check-splicing-syntax-class class stx) ...))

; Syntax equality checking (only concered with symbol-level equality, not equal identifiers)

(define (syntax-datum-equal? stx1 stx2)
  (equal? (syntax->datum stx1) (syntax->datum stx2)))

(define (syntax-list-datum-equal? stx-list1 stx-list2)
  (map syntax-datum-equal? stx-list1 stx-list2))

(define-syntax-rule (check-stx-datum stx-testing stx-expected)
  (check syntax-datum-equal? stx-expected stx-testing))

(define-syntax-rule (check-stx-list-datum stx-list-testing stx-list-expected)
  (check syntax-list-datum-equal? stx-list-expected stx-list-testing))

; Syntax class attribute checking

(define-syntax check-syntax-class-attribute
  (syntax-parser
    [(_ stx class attr expected)
     (with-syntax* ([(a) (generate-temporaries #'(a))]
                    [attr-id (format-id #'a "~a.~a" #'a (syntax->datum #'attr))])
       #'(check-stx-datum
          (syntax-parse stx
            [(~var a class)
             (attribute attr-id)])
          expected))]))

(define-syntax check-syntax-class-list-attribute
  (syntax-parser
    [(_ stx class attr expected)
     (with-syntax* ([(a) (generate-temporaries #'(a))]
                    [attr-id (format-id #'a "~a.~a" #'a (syntax->datum #'attr))])
       #'(check-stx-list-datum
          (syntax-parse stx
            [(~var a class)
             (attribute attr-id)])
          expected))]))

(define-syntax-rule (test-syntax-class-attribute class attr [stx expected] ...)
  (test-begin (check-syntax-class-attribute stx class attr expected) ...))

(define-syntax-rule (test-syntax-class-list-attribute class attr [stx expected] ...)
  (test-begin (check-syntax-class-list-attribute stx class attr expected) ...))

(define-syntax check-splicing-syntax-class-attribute
  (syntax-parser
    [(_ stx class attr expected)
     (with-syntax* ([(a) (generate-temporaries #'(a))]
                    [attr-id (format-id #'a "~a.~a" #'a (syntax->datum #'attr))])
       #'(check-stx-datum
          (syntax-parse stx
            [((~var a class))
             (attribute attr-id)])
          expected))]))

(define-syntax check-splicing-syntax-class-list-attribute
  (syntax-parser
    [(_ stx class attr expected)
     (with-syntax* ([(a) (generate-temporaries #'(a))]
                    [attr-id (format-id #'a "~a.~a" #'a (syntax->datum #'attr))])
       #'(check-stx-list-datum
          (syntax-parse stx
            [((~var a class))
             (attribute attr-id)])
          expected))]))

(define-syntax-rule (test-splicing-syntax-class-attribute class attr [stx expected] ...)
  (test-begin (check-splicing-syntax-class-attribute stx class attr expected) ...))

(define-syntax-rule (test-splicing-syntax-class-list-attribute class attr [stx expected] ...)
  (test-begin (check-splicing-syntax-class-list-attribute stx class attr expected) ...))