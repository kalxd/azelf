#| 模块相关语法。 |#
#lang racket/base

(require racket/contract
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (export-from stx)
  (syntax-case stx ()
    [(_ name ...)
     #'(begin
         (require name ...)
         (provide (all-from-out name ...)))]))
