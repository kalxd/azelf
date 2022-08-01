#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define list-ts
  (test-suite
   "ext : list"

   (test-case "<list>: head"
    (check-equal? nothing (head (list)))
    (check-equal? (Just 1) (head (list 1)))
    (check-equal? (Just 1) (head (list 1 2))))

   (test-case "<list>: foldl"
    (check-equal? 0 (foldl + 0 (list)))
    (check-equal? 10 (foldl + 0 (list 1 2 3 4))))

   (test-case "<list>: foldr"
    (check-equal? 0 (foldr + 0 (list)))
    (check-equal? 10 (foldr + 0 (list 1 2 3 4)))
    (check-equal? (list 1 2 3 4) (foldr cons (list) (list 1 2 3 4))))

   (test-case "<list>: map"
    (check-equal? (list) (map add1 (list)))
    (check-equal? (list 2 3) (map add1 (list 1 2))))

   (test-case "<list>: zip"
    (check-equal? (list (cons 1 1)
                        (cons 2 2)
                        (cons 3 3))
                  (zip (list 1 2 3)
                       (list 1 2 3))))

   (test-case "<list>: filter reject"
    (check-equal? (list 2 4)
                  (filter even? (list 1 2 3 4)))
    (check-equal? (list 1 3)
                  (reject even? (list 1 2 3 4))))

   (test-case "<list>: traverse"
    (define (f1 x)
      (->maybe (and (> 5 x) x)))

    (check-equal? nothing
                  (traverse f1 (list 3 4 5 6)))

    (check-equal? (Just (list 1 2 3))
                  (traverse f1 (list 1 2 3))))))

(define task-list
  (list list-ts))
