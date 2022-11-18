#lang racket/base

(require racket/generic
         racket/contract)

(require "../internal/curry.rkt"
         "../internal/function.rkt")

(provide gen:Monad
         Monad?
         monad:bind
         >>=
         =<<
         >>
         <<)

(define-generics Monad
  (monad:bind Monad f)

  #:defaults ([list?
               (define/contract (monad:bind ma f)
                 (-> list?
                     (-> any/c list?)
                     list?)
                 (for*/list ([a ma])
                   (f a)))]
              [procedure?
               (define/contract (monad:bind f g)
                 (-> procedure? procedure? procedure?)
                 (λ (r)
                   (define a (f r))
                   (g a r)))]))

(define >>= (curry/n 2 monad:bind))
(define =<< (flip >>=))

(define/curry/contract (>> ma mb)
  (-> Monad? Monad? Monad?)
  (>>= ma (const mb)))

(define << (flip >>))
