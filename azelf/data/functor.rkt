#lang racket/base

(require racket/generic
         (only-in "../internal/macro.rkt"
                  macro/id))

(provide gen:Functor
         Functor?
         Functor/c
         fmap)

(define-generics Functor
  (fmap f Functor)
  (<$> f Functor)
  #:defaults
  ([sequence? (define (fmap f Functor)
                (for/list ([x Functor])
                  (f x)))]
   [procedure? (define fmap compose)])
  #:fallbacks
  [(define/generic self/fmap fmap)
   (define <$> self/fmap)])

(module+ test
  (require rackunit)
  (test-case "<Functor>:maybe + <$>"
    (check-equal? (list 2 3)
                  (fmap add1 (list 1 2)))
    (check-equal? (fmap number? (list 1 "2" 3 "4"))
                  (<$> number? (list 1 "2" 3 "4")))))
