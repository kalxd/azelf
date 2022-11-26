#lang racket/base

(require racket/match
         racket/contract

         (prefix-in base:: racket/base))

(require "../type/array.rkt"
         "../type/maybe.rkt"
         "../internal/curry.rkt"
         "../internal/match.rkt")

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

(define/contract empty
  (Array/c any/c)
  (array))

(define/contract (singleton x)
  (-> any/c (Array/c any/c))
  (array x))
;;; end ;;;

;;; 解构 ;;;
(define/match1/contract array->list
  (-> Array? list?)
  [(Array xs ...) xs])
;;; end ;;;
