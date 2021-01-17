#| 内部使用的宏 |#
#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (macro/id gen-method)
  (define (gen-method value)
    value))

(define-syntax-rule (macro/remap gen-method map-method)
  (define (gen-method x)
    (map-method x)))
