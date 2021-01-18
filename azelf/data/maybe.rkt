#lang racket/base

(require racket/contract)

(provide Maybe
         Maybe?
         Maybe/c)

(struct Nothing []
  #:transparent)

(struct Just [a]
  #:transparent)

(struct Maybe [a]
  #:transparent)

(define Maybe/c
  (struct/c Maybe (or/c Nothing? Just?)))

(module+ test
  (require rackunit))
