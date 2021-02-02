#lang racket/base

(require racket/contract
         "../functor/functor.rkt")

(provide Left
         Left?
         Right
         Right?
         Either/c)

(struct Left [value]
  #:transparent

  #:methods gen:Functor
  [(define (fmap f self) self)])

(struct Right [value]
  #:transparent

  #:methods gen:Functor
  [(define (fmap f self)
     (let* ([a (Right-value self)]
            [x (f a)])
       (Right x)))])

(define (Either/c b a)
  (or/c (struct/c Left b)
        (struct/c Right a)))

(module+ test
  (require rackunit)

  (test-case "<Either>:Functor"
    (define left (Left "hello"))
    (define right (Right 1))
    (check-equal? left (fmap add1 left))
    (check-equal? (Right 2) (fmap add1 right))))