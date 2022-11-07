#lang racket/base

(require racket/generic
         racket/contract)

(require (prefix-in base:: racket/base))

(require "../internal/curry.rkt"
         "../internal/match.rkt")

(provide gen:Eq
         Eq?
         eq:=
         =
         /=)

(define-generics Eq
  (eq:= Eq rhs)

  #:defaults ([number? (define eq:= base::=)]
              [string? (define eq:= string=?)]
              [char? (define eq:= char=?)]
              [bytes? (define eq:= bytes=?)]
              [symbol? (define eq:= base::eq?)]
              [pair?
               (define/generic self/= eq:=)
               (define/match/contract (eq:= xs ys)
                 (-> pair? pair? boolean?)
                 [((cons a b) (cons x y))
                  (and (self/= a x) (self/= b y))])]
              [list?
               (define/generic self/= eq:=)
               (define/contract (eq:= xs ys)
                 (-> list? list? boolean?)
                 (and (base::= (length xs)
                               (length ys))
                      (for/and ([x xs]
                                [y ys])
                        (self/= x y))))]
              [vector?
               (define/generic self/= eq:=)
               (define/contract (eq:= xs ys)
                 (-> vector? vector? boolean?)
                 (and (base::= (vector-length xs)
                               (vector-length ys))
                      (for/and ([x xs]
                                [y ys])
                        (self/= x y))))]
              [box?
               (define/generic self/= eq:=)
               (define/match/contract (eq:= x y)
                 (-> box? box? boolean?)
                 [((box x) (box y))
                  (self/= x y)])]
              [hash?
               (define/generic self/= eq:=)
               (define/contract (eq:= a b)
                 (-> hash? hash? boolean?)
                 (and (base::= (hash-count a)
                               (hash-count b))
                      (for/and ([(a av) a]
                                [(b bv) b])
                        (and (self/= a b)
                             (self/= av bv)))))]))

(define = (curry/n 2 eq:=))
(define (/= a b) (not (= a b)))
