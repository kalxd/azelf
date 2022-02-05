#lang racket/base

(require racket/contract
         (only-in racket/function
                  identity)
         "../syntax/curry.rkt")

(provide (except-out (all-defined-out)
                     fmap/n)
         identity)

(define/contract (fmap/n f . xs)
  (->* (procedure?)
       #:rest (listof any/c)
       (or/c #f any/c))
  (if (member #f xs eq?)
      #f
      (apply f xs)))

(define fmap (curry/n 2 fmap/n))
(define fmap2 (curry/n 3 fmap/n))
(define fmap3 (curry/n 4 fmap/n))
(define fmap4 (curry/n 5 fmap/n))
(define fmap5 (curry/n 6 fmap/n))

(define/curry (const a b)
  a)

(module+ test
  (require rackunit)

  (test-case "<function>: const"
    (check-equal? 1 (const 1 2))
    (check-equal? 1 ((const 1) 2))))
