#lang racket/base

(require "../syntax/pipeline.rkt"
         "../internal/keyword.rkt"
         "../data/maybe.rkt"
         "../data/maybe-syntax.rkt"
         "../syntax/curry.rkt")

(require racket/match)

(provide (all-defined-out))

(define/curry (traverse acc f xs)
  (match xs
    [(list) (Just acc)]
    [(list x xs ...)
     (maybe/do
      (a <- (f x))
      (let acc- = (->> (list a)
                       (append acc it)))
      (traverse acc- f xs))]))
