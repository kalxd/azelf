#lang racket/base

(require racket/contract
         "../syntax/curry.rkt")

(provide (except-out (all-defined-out)
                     fmap/n))

(define/contract (fmap/n f . xs)
  (->* (procedure?)
       #:rest (listof any/c)
       (or/c #f any/c))
  (if (member #f xs eq?)
      #f
      (apply f xs)))

(define fmap (curry/n fmap/n 2))
(define fmap2 (curry/n fmap/n 3))
(define fmap3 (curry/n fmap/n 4))
(define fmap4 (curry/n fmap/n 5))
(define fmap5 (curry/n fmap/n 6))
