#lang racket/base

(require racket/match
         "../data/maybe.rkt"
         "../syntax/curry.rkt")

(provide (all-defined-out))

(define/curry (traverse acc f xs)
  (match xs
    [(list) acc]
    [(list x xs ...)
     (match (f x)
       [(Just a)
        (let ([acc- (maybe-map (λ (acc)
                                 (append acc (list a)))
                               acc)])
          (traverse acc- f xs))]
       [_ nothing])]))
