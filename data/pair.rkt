#lang racket/base

(require racket/contract
         racket/match)

(provide (all-defined-out))

(define/contract (pair->values x)
  (-> pair? any)
  (match x
    [(cons a b) (values a b)]))
