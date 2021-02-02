#| 内部使用的宏 |#
#lang racket/base

(require racket/contract
         (for-syntax racket/base
                     racket/match))

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
     (let* ([names (syntax->datum #'(name ...))]
            [require-names (for/list ([name names])
                             (if (list? name)
                                 `(except-in ,@name)
                                 name))]
            [provide-names (for/list ([name names])
                             (if (list? name)
                                 (car name)
                                 name))])
       (with-syntax ([(require-name ...) (datum->syntax stx require-names)]
                     [(provide-name ...) (datum->syntax stx provide-names)])
         #'(begin
             (require require-name ...)
             (provide (all-from-out provide-name ...)))))]))
