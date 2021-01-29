#lang racket/base

(require racket/generic
         (rename-in racket/base
                    (append list-append))
         (only-in racket/vector
                  vector-append))

(provide gen:Semigroup
         Semigroup/c
         Semigroup?
         <>
         append)

(define-generics Semigroup
  (<> Semigroup b)
  (append Semigroup b)
  #:defaults
  ([list? (define append list-append)]
   [vector? (define append vector-append)]
   [string? (define append string-append)]
   [procedure? (define/generic self/append append)
               (define ((append f g) x)
                 (let ([a (f x)]
                       [b (g x)])
                   (self/append a b)))])

  #:fallbacks
  [(define/generic self/append append)
   (define <> self/append)])

(module+ test
  (require rackunit)
  (test-case "<Semigroup>:append"
    (check-equal? (list 1 2)
                  (<> (list 1) (list 2)))
    (check-equal? "helloworld"
                  (<> "hello" "world"))
    (check-pred string?
                ((<> number->string number->string) 1))))
