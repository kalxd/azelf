#lang racket/base

(require racket/generic
         racket/contract
         racket/match)

(provide gen:Functor
         Functor?
         Functor/c
         map)

(define/contract (pair->values x)
  (-> pair? any)
  (match x
    [(cons a b) (values a b)]))

(define-generics Functor
  (map f Functor)
  #:defaults
  ([list? (define (map f Functor)
            (for/list ([x Functor])
              (f x)))]
   [procedure? (define (map f g)
                 (compose f g))]
   [pair? (define (map f x)
            (define-values (a b) (pair->values x))
            (cons a (f b)))]))

(module+ test
  (require rackunit)
  (test-case "<Functor>:map"
    (check-equal? (list 2 3)
                  (map add1 (list 1 2)))
    (check-equal? (map number? (list 1 "2" 3 "4"))
                  (map number? (list 1 "2" 3 "4")))))
