#lang racket/base

(require racket/generic
         (rename-in racket/base
                    (append list-append))
         (only-in racket/vector
                  vector-append))

(provide gen:Semigroup
         Semigroup/c
         Semigroup?
         mappend)

(define-generics Semigroup
  (mappend Semigroup b)
  #:defaults
  ([list? (define mappend list-append)]
   [vector? (define mappend vector-append)]
   [string? (define mappend string-append)]
   [procedure? (define/generic self/append mappend)
               (define ((mappend f g) x)
                 (let ([a (f x)]
                       [b (g x)])
                   (self/append a b)))]))

(module+ test
  (require rackunit)
  (test-case "<Semigroup>:append"
    (check-equal? (list 1 2)
                  (mappend (list 1) (list 2)))
    (check-equal? "helloworld"
                  (mappend "hello" "world"))
    (check-pred string?
                ((mappend number->string number->string) 1))))
