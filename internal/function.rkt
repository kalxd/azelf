#lang racket/base

(require (only-in racket/function
                  identity
                  curry))
(require "./curry.rkt")

(provide const
         identity
         flip)

(define/curry (const a b)
  a)

(define/curry (flip f a b)
  (f b a))

(module+ test
  (define/curry (f a b)
    (- a b))
  (define g (flip f)))
