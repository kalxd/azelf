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
         mappend)

(define-generics Semigroup
  (<> Semigroup b)
  (mappend Semigroup b)
  #:defaults
  ([list? (define mappend list-append)]
   [vector? (define mappend vector-append)]
   [string? (define mappend string-append)]
   [procedure? (define/generic self/append mappend)
               (define ((mappend f g) x)
                 (let ([a (f x)]
                       [b (g x)])
                   (self/append a b)))])

  #:fallbacks
  [(define/generic self/append mappend)
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
