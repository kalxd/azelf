#lang typed/racket/base

(provide Nullable)

(define-type (Nullable A)
  (U 'nil A))
