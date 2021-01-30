#lang racket/base

(require racket/generic
         "./monoid.rkt"
         (rename-in racket/base
                    (foldl list-foldl)))

(provide gen:Foldable
         Foldable?
         foldl)

(define (normal-foldl f acc xs)
  (for/fold ([acc acc])
            ([x xs])
    (f acc x)))

(define-generics Foldable
  (foldl f acc Foldable)

  #:defaults
  ([list? (define foldl normal-foldl)]
   [vector? (define foldl normal-foldl)]
   [string? (define foldl normal-foldl)]))
