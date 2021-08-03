#lang racket/base

(require racket/contract
         racket/match

         "../syntax/curry.rkt")

(provide Nothing
         Just
         Maybe/c)

(struct nothing [])

(struct just [value]
  #:transparent)

(define/contract (Maybe/c a)
  (-> any/c contract?)
  (or/c nothing? (struct/c just a)))

(define/contract Nothing
  (Maybe/c any/c)
  (nothing))

(define/contract Just
  (-> any/c (Maybe/c any/c))
  just)

(curry/contract (maybe-map f a)
  (-> (-> any/c any/c)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(just a)
     (just (f a))]
    [else a]))

(define/curry (maybe-unwrap b a)
  (match a
    [(just a) a]
    [else b]))

(curry/contract (maybe-then f a)
  (-> (-> any/c (Maybe/c any/c))
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(just a) (f a)]
    [else a]))
