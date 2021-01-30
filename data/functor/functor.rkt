#lang racket/base

(require racket/generic
         racket/contract
         racket/match)

(provide gen:Functor
         Functor?
         Functor/c
         fmap)

(define/contract (pair->values x)
  (-> pair? any)
  (match x
    [(cons a b) (values a b)]))

(define-generics Functor
  (fmap f Functor)
  #:defaults
  ([sequence? (define (fmap f Functor)
                (for/list ([x Functor])
                  (f x)))]
   [procedure? (define fmap compose)]
   [pair? (define (fmap f x)
            (define-values (a b) (pair->values x))
            (cons a (f b)))]))

(module+ test
  (require rackunit)
  (test-case "<Functor>:fmap"
    (check-equal? (list 2 3)
                  (fmap add1 (list 1 2)))
    (check-equal? (fmap number? (list 1 "2" 3 "4"))
                  (fmap number? (list 1 "2" 3 "4")))))
