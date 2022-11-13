#lang racket/base

(require (only-in racket/function
                  identity
                  curry))
(require "../internal/curry.rkt")

(provide const
         identity
         flip)

(define/curry (const a b)
  a)

(define (flip f)
  (define (flipped b a)
    (f a b))
  (curry flipped))


(module+ test
  (define/curry (f a b)
    (- a b))
  (define g (flip f)))
