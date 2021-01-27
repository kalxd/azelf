#lang racket/base

(require racket/generic
         (only-in racket/function
                  const)
         (only-in "../internal/macro.rkt"
                  macro/id))

(provide gen:Functor
         Functor?
         Functor/c
         fmap
         <$>
         <$
         $>)

(define-generics Functor
  (fmap f Functor)
  (<$> f Functor)
  (<$ a Functor)
  ($> Functor a)
  #:defaults
  ([sequence? (define (fmap f Functor)
                (for/list ([x Functor])
                  (f x)))]
   [procedure? (define fmap compose)])
  #:fallbacks
  [(define/generic self/fmap fmap)
   (define <$> self/fmap)
   (define (<$ a Functor)
     (self/fmap (const a)
                Functor))
   (define ($> Functor a)
     (self/fmap (const a) Functor))])

(module+ test
  (require rackunit)
  (test-case "<Functor>:maybe + <$>"
    (check-equal? (list 2 3)
                  (fmap add1 (list 1 2)))
    (check-equal? (fmap number? (list 1 "2" 3 "4"))
                  (<$> number? (list 1 "2" 3 "4"))))

  (test-case "<Functor>:<$"
    (check-equal? (list 1 1 1)
                  (<$ 1 (list 4 5 6))))

  (test-case "<Functor>:$>"
    (check-equal? (list 1 1 1)
                  ($> (list 1 2 3) 1))))
