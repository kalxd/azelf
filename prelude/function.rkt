#lang racket/base

(require racket/contract
         (only-in racket/function
                  identity)
         "../internal/curry.rkt")

(provide const
         identity)

(define/curry (const a b)
  a)

(module+ test
  (require rackunit)

  (test-case "<function>: const"
    (check-equal? 1 (const 1 2))
    (check-equal? 1 ((const 1) 2))))
