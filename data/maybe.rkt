#lang racket/base

(require racket/contract
         racket/match

         "../syntax/curry.rkt")

(provide Nothing
         Just
         Nothing?
         Just?
         nothing
         Maybe/c

         maybe-map
         maybe-then
         maybe-unwrap
         ->maybe)

(struct Nothing []
  #:transparent)

(struct Just [value]
  #:transparent)

(define/contract (Maybe/c a)
  (-> any/c contract?)
  (or/c Nothing? (struct/c Just a)))

(define/contract nothing
  (Maybe/c any/c)
  (Nothing))

(curry/contract (maybe-map f a)
  (-> (-> any/c any/c)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (Just (f a))]
    [else a]))

(module+ test
  (require rackunit)

  (test-case "<maybe>: maybe-map"
    (check-equal? (Just 2) (maybe-map add1 (Just 1)))
    (check-equal? nothing ((maybe-map add1) nothing))))

(define/curry (maybe-unwrap b a)
  (match a
    [(Just a) a]
    [else b]))

(module+ test
  (test-case "<maybe>: maybe-unwrap"
    (check-equal? 1 (maybe-unwrap 2 (Just 1)))
    (check-equal? 2 (maybe-unwrap 2 nothing))))

(curry/contract (maybe-then f a)
  (-> (-> any/c (Maybe/c any/c))
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (f a)]
    [else a]))

(module+ test
  (test-case "<maybe>: maybe-then"
    (define (f x)
      (Just (add1 x)))
    (check-equal? (Just 2) (maybe-then f (Just 1)))
    (check-equal? nothing (maybe-then f nothing))))

(define/contract (->maybe a)
  (-> any/c (Maybe/c any/c))
  (if a (Just a) nothing))

(module+ test
  (test-case "<maybe>: ->maybe"
    (check-equal? (Just 1) (->maybe 1))
    (check-equal? nothing (->maybe #f))))
