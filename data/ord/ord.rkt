#lang racket/base

(require racket/generic
         racket/contract)

(provide gen:Ord
         Ord?
         Ord/c
         compare
         LT?
         LT
         lt
         EQ?
         EQ
         eq
         GT?
         GT
         gt)

(struct LT [])
(define lt (LT))

(struct EQ [])
(define eq (EQ))

(struct GT [])
(define gt (GT))

(define ((cmp gt-f lt-f) a b)
  (cond
    [(gt-f a b) gt]
    [(lt-f a b) lt]
    [else eq]))

(define-generics Ord
  (compare Ord a)
  #:defaults
  ([number? (define compare
              (cmp > <))]
   [string? (define compare
              (cmp string>? string<?))]
   [char? (define compare
            (cmp char>? char<?))]))

(module+ test
  (require rackunit)
  (test-case "<Ord>:compare"
    (check-equal? gt (compare 3 2))
    (check-equal? lt (compare #\A #\B))
    (check-equal? eq (compare "hello" "hello"))))
