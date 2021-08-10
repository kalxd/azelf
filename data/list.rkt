#lang racket/base

(require "../syntax/curry.rkt"

         racket/contract)

(provide (all-defined-out))

(curry/contract (zip xs ys)
  (-> (listof any/c) (listof any/c)
      (listof pair?))
  (for/list ([x xs]
             [y ys])
    (cons x y)))
