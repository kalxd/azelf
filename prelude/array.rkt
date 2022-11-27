#lang racket/base

(require racket/match
         racket/contract

         (prefix-in base:: racket/base)
         (prefix-in list:: racket/list))

(require "../type/array.rkt"
         "../type/maybe.rkt"
         "../internal/curry.rkt"
         "../internal/match.rkt"
         "../internal/pipeline.rkt"
         "../internal/keyword.rkt")

(provide (all-defined-out))

(module inner racket/base
  (provide (all-defined-out)))

(require (prefix-in inner:: 'inner))

;;; 合并 ;;;
(define/curry/contract (++ xs ys)
  (-> Array? Array? Array?)
  (match-define (Array as ...) xs)
  (match-define (Array bs ...) ys)
  (list->array (base::append as bs)))

(define/curry/contract (: x xs)
  (-> any/c Array? Array?)
  (++ (array x) xs))

(define/curry/contract (<:> x xs)
  (-> any/c Array? Array?)
  (++ xs (array x)))
;;; end ;;;

(define/curry/contract (repeat n a)
  (-> (or/c zero? positive?)
      any/c
      Array?)
  (list->array
   (for/list ([_ (in-range n)]) a)))

;;; 构造 ;;;
(define/curry/contract (range start end)
  (-> number? number? Array?)
  (list->array
   (for/list ([x (in-range start end)]) x)))

(define/contract empty
  (Array/c any/c)
  (array))

(define/contract (singleton x)
  (-> any/c (Array/c any/c))
  (array x))
;;; end ;;;

;;; 子列表 ;;;
(define/match1/contract head
  (-> Array? (Maybe/c any/c))
  [(Array) nothing]
  [(Array a xs ...) (Just a)])

(define/match1/contract tail
  (-> Array? (Maybe/c any/c))
  [(Array) nothing]
  [_ (->> (array->list it)
          list::last
          Just)])

(define/match1/contract uncons
  (-> Array? (Maybe/c (cons/c any/c any/c)))
  [(Array) nothing]
  [(Array a xs ...)
   (Just (cons a xs))])
;;; end ;;;

;;; 解构 ;;;
;;; end ;;;
