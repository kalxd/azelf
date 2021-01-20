#lang racket/base

(require racket/generic
         (only-in "../internal/macro.rkt"
                  macro/id))

(provide gen:Functor
         Functor?
         Functor/c)

(define-generics Functor
  (fmap f Functor)
  #:defaults
  ([sequence? (define (fmap f Functor)
                (for/list ([x Functor])
                  (f x)))]
   [procedure? (define fmap compose)]))
