#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract
         racket/match
         "../semigroup/semigroup.rkt"
         "../monoid/monoid.rkt")

(export-from "./foldable.rkt")

(define/contract (any f xs)
  (-> (-> any/c boolean?)
      Foldable?
      boolean?)
  (define (g acc x)
    (or acc (f x)))
  (foldl g #f xs))

(define/contract (all f xs)
  (-> (-> any/c boolean?)
      Foldable?
      boolean?)
  (define (g acc x)
    (and acc (f x)))
  (foldl g #t xs))
