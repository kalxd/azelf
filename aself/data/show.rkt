#lang racket/base

(require racket/generic
         (for-syntax racket/base))

(provide gen:Show
         Show?
         Show/c)

(define-syntax-rule (id gen-method)
  (define (gen-method value)
    value))

(define-syntax-rule (remap gen-method map-method)
  (define (gen-method x)
    (map-method x)))

(define-generics Show
  (->string Show)
  #:defaults
  [(string? (id ->string))
   (number? (remap ->string number->string))])
