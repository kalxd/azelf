#lang racket/base

(require racket/match
         racket/contract

         (prefix-in base:: racket/base))

(require "../type/array.rkt"
         "../internal/curry.rkt"
         "../internal/match.rkt")

(module inner racket/base
  (require racket/match)

  (provide (all-defined-out))

  (define (replicate n a xs)
    (match n
      [0 xs]
      [_ (replicate (sub1 n) a (cons a xs))])))

(require (prefix-in inner:: 'inner))

(provide (all-defined-out))

;;; 合并 ;;;
(define/curry/contract (++ xs ys)
  (-> Array? Array? Array?)
  (match-define (Array as ...) xs)
  (match-define (Array bs ...) ys)
  (array (base::append as bs)))

(define/curry/contract (: x xs)
  (-> any/c Array? Array?)
  (match xs
    [(Array ls ...) (apply array (cons x ls))]))

(define/curry/contract (<:> x xs)
  (-> any/c Array? Array?)
  (++ xs (singleton x)))

(define/contract (list->array xs)
  (-> list? Array?)
  (apply array xs))

(define/contract mempty
  (Array/c any/c)
  (array))

(define/contract (singleton x)
  (-> any/c (Array/c any/c))
  (array x))

(define/curry/contract (replicate n a)
  (-> (or/c zero? positive?) any/c Array?)
  (list->array (inner::replicate n a (list))))

;;; end ;;;

;;; 解构 ;;;
(define/match1/contract array->list
  (-> Array? list?)
  [(Array xs ...) xs])
;;; end ;;;
