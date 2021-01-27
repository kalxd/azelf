#| 内部使用的宏 |#
#lang racket/base

(require racket/contract
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax-rule (macro/id gen-method)
  (define (gen-method value)
    value))

(define-syntax-rule (macro/remap gen-method map-method)
  (define (gen-method . args)
    (apply map-method args)))

(define-syntax (export-from stx)
  (syntax-case stx ()
    [(_ name ...)
     #'(begin
         (require name ...)
         (provide (all-from-out name ...)))]))
