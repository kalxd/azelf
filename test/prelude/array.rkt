#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define array-transform-ts
  (test-suite
   "Array transform"
   (test-case "<array>: span"
     (check-equal? (cons empty empty)
                   (span (> 0) (array)))
     (check-equal? (cons (array 1 2) (array 3 4 1 2 3 4))
                   (span (λ (x) (< x 3)) (array 1 2 3 4 1 2 3 4)))
     (check-equal? (cons (array 1 2 3) empty)
                   (span (λ (x) (< x 9)) (array 1 2 3)))
     (check-equal? (cons empty (array 1 2 3))
                   (span (λ (x) (< x 0)) (array 1 2 3))))))

(define task-list
  (list array-transform-ts))
