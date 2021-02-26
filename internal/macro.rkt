#| 内部使用的宏 |#
#lang racket/base

(require racket/contract
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax-rule (macro/id f)
  (define (f x)
    x))

(define-syntax (export-from stx)
  (syntax-case stx ()
    [(_ name ...)
     #'(begin
         (require name ...)
         (provide (all-from-out name ...)))]))
