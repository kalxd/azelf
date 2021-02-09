#lang racket/base

(require racket/generic)

(provide gen:Applicative
         Applicative/c
         Applicative?
         pure)

(define-generics Applicative
  (pure Applicative x)
  #:defaults
  ([list? (define (pure _ x) (list x))]))
