#lang racket/base

(require racket/generic
         racket/match
         racket/contract)
(require (prefix-in base:: racket/base)
         (only-in racket/vector
                  vector-map))
(require "../internal/curry.rkt"
         "./function.rkt")

(provide gen:Functor
         Functor?
         map
         <$>
         <#>
         <$
         $>)

(define-generics Functor
  (functor:map f Functor)

  #:defaults ([list? (define functor:map base::map)]
              [pair?
               (define/contract (functor:map f ma)
                 (-> (-> any/c any/c)
                     (cons/c any/c any/c)
                     (cons/c any/c any/c))
                 (match-define (cons a b) ma)
                 (cons a (f b)))]
              [vector? (define functor:map vector-map)]
              [hash?
               (define/contract (functor:map f ma)
                 (-> (-> any/c any/c) hash? hash?)
                 (for/hash ([(k v) ma])
                   (values k (f v))))]))

(define map (curry/n 2 functor:map))
(define <$> map)
(define <#> (flip map))

(define/curry/contract (<$ a ma)
  (-> any/c Functor? Functor?)
  (map (const a) ma))

(define $> (flip <$))
