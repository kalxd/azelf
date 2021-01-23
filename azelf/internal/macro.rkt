#| 内部使用的宏 |#
#lang racket/base

(require racket/contract
         (only-in "../function.rkt"
                  define/curry))

(provide (all-defined-out))

(define-syntax-rule (macro/id gen-method)
  (define (gen-method value)
    value))

(define-syntax-rule (macro/remap gen-method map-method)
  (define (gen-method . args)
    (apply map-method args)))

(define-syntax-rule (macro/remap/curry2 gen-method map-method)
  (define/curry (gen-method a b)
    (-> any any any)
    (map-method a b)))
