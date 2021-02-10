#lang racket/base

(require racket/generic
         (only-in racket/list
                  append-map))

(provide gen:Applicative
         Applicative/c
         Applicative?
         pure
         ap)

(define-generics Applicative
  (pure Applicative x)
  (ap f Applicative)
  #:defaults
  ([list? (define (pure _ x) (list x))
          (define (ap fs xs)
            (define (map-f x)
              (for/list ([f fs])
                (f x)))
            (append-map map-f xs))]

   [procedure? (define ((pure _ x) y) x)
               (define ((ap f g) x)
                 (f x (g x)))]))

(module+ test
  (require rackunit
           racket/function)

  (test-case "<Applicative>:ap"
    (check-equal? 3 ((ap + add1) 1))
    (check-equal? '(2 2 3 3 4 4 5 5)
                  (ap (list add1 add1)
                      '(1 2 3 4))))

  (test-case "<Applicative>:pure"
    (check-equal? 1 ((pure identity 1) 10))
    (check-equal? (list 1) (pure (list 1 2 3) 1))))
