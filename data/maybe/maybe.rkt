#lang racket/base

(require "../functor/functor.rkt"
         racket/contract)

(provide Maybe/c
         Nothing
         Nothing?
         nothing
         Just
         Just?)

(module+ test
  (require rackunit
           (only-in racket/function
                    identity)))

(struct Nothing []
  #:transparent

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (display "#<Nothing>" port))]

  #:methods gen:Functor
  [(define (fmap f self) self)])

(define nothing (Nothing))

(struct Just [a]
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (let* ([a (Just-a self)]
            [output (format "#<Just ~s>" a)])
       (display output port)))]

  #:methods gen:Functor
  [(define (fmap f self)
     (let* ([x (Just-a self)]
            [y (f x)])
       (Just y)))])

(define (Maybe/c x)
  (or/c Nothing? (struct/c Just x)))

(module+ test
  (define just (Just 1))

  (test-case "<Maybe>:Functor"
    (define value (Just 1))

    (test-equal? "Identity Just"
                 (fmap identity value)
                 value)

    (test-equal? "Identity Nothing"
                 (fmap identity nothing)
                 nothing)

    (let ([compose-f (compose number->string add1)]
          [fcompose-f (λ (x) (fmap number->string x))]
          [fcompose-g (λ (x) (fmap add1 x))])
      (test-equal? "Composition Just"
                   (fmap compose-f value)
                   ((compose fcompose-f fcompose-g) value)))))


