#lang racket/base

(require "./pipeline.rkt"
         "./curry.rkt"
         "./keyword.rkt"
         "../prelude/maybe.rkt")

(require racket/match)

(provide (all-defined-out))

(define concat (curry/n 2 append))

(define/curry (traverse acc f xs)
  (match xs
    [(list) (Just acc)]
    [(list x xs ...)
     (maybe/do
      (a <- (f x))
      (->> (list a)
           (concat acc)
           (traverse it f xs)))]))

(define/curry (filter-map acc f xs)
  (match xs
    [(list) acc]
    [(list x xs ...)
     (match (f x)
       [(Just x) (->> (list x)
                      (concat acc)
                      (filter-map it f xs))]
       [_ (filter-map acc f xs)])]))
