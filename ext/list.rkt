#lang racket/base

(require "../syntax/curry.rkt"
         "../syntax/match.rkt"
         "../data/maybe.rkt"

         racket/contract
         racket/match
         (prefix-in list. racket/list))

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

(define/curry (private/traverse acc f xs)
  (match xs
    [(list) acc]
    [(list x xs ...)
     (match (f x)
       [(Just a)
        (let ([acc- (maybe-map (λ (acc)
                                 (append acc (list a)))
                               acc)])
          (private/traverse acc- f xs))]
       [_ nothing])]))

(define/contract traverse
  (-> (-> any/c (Maybe/c any/c))
      (listof any/c)
      (Maybe/c (listof any/c)))
  (private/traverse (Just list.empty)))

(module+ test
  (require rackunit)

  (test-case "<list>: traverse"
    (define (f1 x)
      (->maybe (and (> 5 x) x)))

    (check-equal? nothing
                  (traverse f1 (list 3 4 5 6)))

    (check-equal? (Just (list 1 2 3))
                  (traverse f1 (list 1 2 3)))))
