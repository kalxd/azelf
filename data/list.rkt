#lang racket/base

(require "../syntax/curry.rkt"
         "../syntax/match.rkt"
         "../data/maybe.rkt"

         racket/contract
         racket/match
         (only-in racket/function
                  curry)

         (prefix-in list:: racket/list)
         (prefix-in inter-list:: "../internal/list.rkt")

         (rename-in racket/base
                    [foldl base::foldl]
                    [foldr base::foldr]
                    [map base::map]
                    [filter base::filter]))

(provide (all-defined-out))

(define/match1/contract head
  (-> (listof any/c) (Maybe/c any/c))
  [(list) nothing]
  [(list a) (Just a)]
  [(list a _) (Just a)])

(define foldl (curry/n 3 base::foldl))
(define foldr (curry/n 3 base::foldr))
(define map (curry/n 2 base::map))

(define/curry/contract (zip-with f xs ys)
  (-> (-> any/c any/c any/c)
      (listof any/c)
      (listof any/c)
      (listof any/c))
  (for/list ([x xs]
             [y ys])
    (f x y)))

(define/contract zip
  (-> (listof any/c) (listof any/c)
      (listof pair?))
  (zip-with cons))

(define filter (curry base::filter))

(define/curry (reject f xs)
  (define g
    (compose not
             f))
  (filter g xs))

(define/contract traverse
  (-> (-> any/c (Maybe/c any/c))
      (listof any/c)
      (Maybe/c (listof any/c)))
  (inter-list::traverse (Just list::empty)))
