#lang racket/base

(require "../syntax/curry.rkt"
         "../data/maybe.rkt"

         racket/contract
         racket/match
         (only-in racket/list
                  empty))

(provide zip
         traverse)

(define/curry/contract (zip xs ys)
  (-> (listof any/c) (listof any/c)
      (listof pair?))
  (for/list ([x xs]
             [y ys])
    (cons x y)))

(define/curry (private/traverse acc f xs)
  (match xs
    [(list) acc]
    [(list x xs ...)
     (match (f x)
       [(Just a)
        (let ([acc- (maybe-map (λ (acc)
                                 (append acc (list a)))
                               acc)])
          (private/traverse acc- f xs))]
       [_ nothing])]))

(define/curry/contract (traverse f xs)
  (-> (-> any/c (Maybe/c any/c))
      (listof any/c)
      (Maybe/c (listof any/c)))
  (private/traverse (Just empty) f xs))
