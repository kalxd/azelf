#lang racket/base

(require racket/match
         racket/contract

         (prefix-in base:: racket/base)
         (prefix-in list:: racket/list))

(require "../type/array.rkt"
         "../type/maybe.rkt"
         "../type/ord.rkt"
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

;;; 基本属性 ;;;
(define/match1/contract length
  (-> Array? exact-nonnegative-integer?)
  [(Array xs ...) (base::length xs)])

(define/match1/contract empty?
  (-> Array? boolean?)
  [(Array xs ...) (list::empty? xs)])
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

(define/curry/contract (take-while f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (match xs
    [(Array xs ...)
     (list->array (list::takef xs f))]))

(define/curry/contract (take n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::take xs m)))))

(define/curry/contract (take-right n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::take-right xs m)))))

(define/curry/contract (drop-while f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (match xs
    [(Array xs ...)
     (list->array (list::dropf xs f))]))

(define/curry/contract (drop n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::drop xs m)))))

(define/curry/contract (drop-right n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::drop-right xs m)))))
;;; end ;;;

;;; 解构 ;;;
;;; end ;;;
