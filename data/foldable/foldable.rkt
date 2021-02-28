#lang racket/base

(require racket/generic)

(provide gen:Foldable
         Foldable?
         Foldable/c
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
