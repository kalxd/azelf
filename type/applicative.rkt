#lang racket/base

(require racket/generic
         racket/contract
         (only-in racket/function
                  identity))

(require "../internal/curry.rkt"
         "../internal/function.rkt")

(require "./functor.rkt")

(provide gen:Applicative
         Applicative?
         applicative:ap
         <*>
         *>
         <*)

(define-generics Applicative
  (applicative:ap f Applicative)

  #:defaults ([procedure?
               (define/contract (applicative:ap f g)
                 (-> (-> any/c any/c any/c)
                     (-> any/c any/c)
                     (-> any/c any/c))
                 (λ (a)
                   (f a (g a))))]
              [list?
               (define/contract (applicative:ap fs xs)
                 (-> (listof (-> any/c any/c))
                     list?
                     list?)
                 (for*/list ([f fs]
                             [x xs])
                   (f x)))]))

(define <*> (curry/n 2 applicative:ap))

(define/curry/contract (*> fa fb)
  (-> Applicative? Applicative? Applicative?)
  (<*> (<$ identity fa) fb))

(define/curry/contract (liftA2 f fa fb)
  (-> procedure?
      Applicative?
      Applicative?
      Applicative?)
  (<*> (map f fa) fb))

(define/curry/contract (<* fa fb)
  (-> Applicative? Applicative? Applicative?)
  (liftA2 const fa fb))
