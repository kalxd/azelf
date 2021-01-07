#lang racket/base

(require racket/contract
         racket/list
         racket/match)

(provide (all-defined-out))

(module inner racket/base
  (require racket/list)
  (provide (all-defined-out))

  (define (_zip-list xs ys acc)
    (if (or (empty? xs)
            (empty? ys))
        acc
        (let ([x (first xs)]
              [xxs (rest xs)]
              [y (first ys)]
              [yys (rest ys)])
          (_zip-list xxs
                     yys
                     (append acc (list (cons x y))))))))

(require 'inner)

(define/contract (zip xs ys)
  (-> (listof any/c)
      (listof any/c)
      (listof (cons/c any/c any/c)))
  (_zip-list xs ys empty))
