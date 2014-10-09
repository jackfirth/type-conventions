type-conventions
================

*A Typed Racket package for painless function definitions*

It is frequently the case in a statically typed language that the type of a variable can be determined solely from it's name, such as a variable of type `Chair` named `chair`. Due to Typed Racket's support for macros, this pattern can be abstracted away. This package provides several forms for this purpose. For example:

    #lang typed/racket
    (require type-conventions)
    (define-type-convention Number x)
    (define: (f x) : Number
      (+ x x))

In the above, the `define-type-convetion` and `define:` forms are used to assume that the function argument `x` has type Number. There exists a shorthand form of `define-type-convention` for defining many convetions at once, named `define-type-conventions`:

    (define-type-conventions
      [Number x y z]
      [Boolean bool boolean])

Any non-parametric type can be associated with a convention. Note however, that it is very common to pluralize the names of rest-arguments, for example:

    (define: (f . xs) : Number
      (apply + xs))

A rest arg contains a list, but it does not have the type `List`, rather it has the type `(Listof A)` where a is the type given in the definition form. A rest arg declared with type `Number` contains a list of numbers. Because of this, conventional names for rest arguments must be defined explicitly with the `define-rest-type-convention` form:

    (define-rest-type-convention Number xs)
    (define: (f . xs) : Number
      (apply + xs))

A corresponding shorthand `define-rest-type-conventions` is also provided.

While conventionally typed arguments cannot be parametric, the `define:` form accepts explicitly typed arguments in addition to conventionally typed ones, and the explicitly typed arguments can be parametrically typed:

    (define: (V) (pair-to-num x [v : V]) : (Pairof Number V)
      (pair x V))

The `define:` form also supports optional arguments, keyword arguments, and optional keyword arguments:

    (define-type-conventions [Number x y])
    (define (f x #:y y) : Number
      (+ x y))
    (define (g x [y 1]) : Number
      (+ x y))
    (define (h x #:y [y 1]) : Number
      (+ x y))

The form also allows for explicitly typed defaults values:

    (define: (f [x : Number 1]) : Number
      (add1 x))

As of this writing, the `define:` form does *not* support the following:

1. Partially applicable functions, e.g. (define: ((f x) y) : Number (+ x y))
2. Anonymous functions, e.g. (lambda: (f x) x)
3. Parametrically typed shorthands.
4. Explicitly typed rest arguments.

This may change in a future release, but no guarantees are given with the exception that due to syntactic limitations, explicitly typed rest arguments are likely impossible to implement in a sensible way.
