#lang typed/racket/base

(provide (all-defined-out))

(: inc (-> Real Real))
(define (inc x)
  (+ 1 x))
