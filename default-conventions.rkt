#lang typed/racket

(require "type-args.rkt")

(define-type-conventions
  [Number x y z]
  [Boolean bool boolean]
  [String s str]
  [Char c char]
  [Symbol sym symbol]
  [Natural n m]
  [Index i j])
