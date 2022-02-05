#lang racket/base

(require "../syntax/curry.rkt"
         "../syntax/match.rkt"
         "../data/maybe.rkt"

         racket/contract
         racket/match

         (prefix-in list:: racket/list)
         (prefix-in inter-list:: "../internal/list.rkt")

         (rename-in racket/base
                    [foldl base::foldl]
                    [foldr base::foldr]
                    [map base::map]))

(provide (all-defined-out))

(define/match1/contract head
  (-> (listof any/c) (Maybe/c any/c))
  [(list) nothing]
  [(list a) (Just a)]
  [(list a _) (Just a)])

(module+ test
  (require rackunit)
  (test-case "<list>: head"
    (check-equal? nothing (head (list)))
    (check-equal? (Just 1) (head (list 1)))
    (check-equal? (Just 1) (head (list 1 2)))))

(define foldl (curry/n 3 base::foldl))

(module+ test
  (test-case "<list>: foldl"
    (check-equal? 0 (foldl + 0 (list)))
    (check-equal? 10 (foldl + 0 (list 1 2 3 4)))))

(define foldr (curry/n 3 base::foldr))

(module+ test
  (test-case "<list>: foldr"
    (check-equal? 0 (foldr + 0 (list)))
    (check-equal? 10 (foldr + 0 (list 1 2 3 4)))
    (check-equal? (list 1 2 3 4) (foldr cons (list) (list 1 2 3 4)))))

(define map (curry/n 2 base::map))

(module+ test
  (test-case "<list>: map"
    (check-equal? (list) (map add1 (list)))
    (check-equal? (list 2 3) (map add1 (list 1 2)))))

(define/curry/contract (zip xs ys)
  (-> (listof any/c) (listof any/c)
      (listof pair?))
  (for/list ([x xs]
             [y ys])
    (cons x y)))

(module+ test
  (require rackunit)

  (test-case "<list>: zip"
    (check-equal? (list (cons 1 1)
                        (cons 2 2)
                        (cons 3 3))
                  (zip (list 1 2 3)
                       (list 1 2 3)))))

(define/contract traverse
  (-> (-> any/c (Maybe/c any/c))
      (listof any/c)
      (Maybe/c (listof any/c)))
  (inter-list::traverse (Just list::empty)))

(module+ test
  (require rackunit)

  (test-case "<list>: traverse"
    (define (f1 x)
      (->maybe (and (> 5 x) x)))

    (check-equal? nothing
                  (traverse f1 (list 3 4 5 6)))

    (check-equal? (Just (list 1 2 3))
                  (traverse f1 (list 1 2 3)))))
