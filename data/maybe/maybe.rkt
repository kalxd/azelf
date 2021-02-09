#lang racket/base

(require "../eq/eq.rkt"
         "../functor/functor.rkt"
         racket/contract
         racket/generic
         racket/match)

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

  ; Eq
  #:methods gen:Eq
  [(define (= a b)
     (Nothing? b))]

  ; Functor
  #:methods gen:Functor
  [(define (map f self) self)])

(define nothing (Nothing))

(struct Just [a]
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (let* ([a (Just-a self)]
            [output (format "#<Just ~s>" a)])
       (display output port)))]

  ; Eq
  #:methods gen:Eq
  [(define/generic Eq/= =)
   (define (= a b)
     (match (cons a b)
       [(cons (Just x) (Just y)) (Eq/= x y)]
       [else #f]))]

  #:methods gen:Functor
  [(define (map f self)
     (let* ([x (Just-a self)]
            [y (f x)])
       (Just y)))])

(define (Maybe/c x)
  (or/c Nothing? (struct/c Just x)))

(module+ test
  (define just (Just 1))

  (test-case "<Maybe>:Eq"
    (check-true (= nothing nothing))
    (check-false (= nothing (Just 2)))
    (check-true (= (Just 1) (Just 1)))
    (check-true (= (Just "hello") (Just "hello")))
    (check-false (= (Just #\A) (Just #\B))))

  (test-case "<Maybe>:Functor"
    (define value (Just 1))

    (test-equal? "Identity Just"
                 (map identity value)
                 value)

    (test-equal? "Identity Nothing"
                 (map identity nothing)
                 nothing)

    (let ([compose-f (compose number->string add1)]
          [fcompose-f (λ (x) (map number->string x))]
          [fcompose-g (λ (x) (map add1 x))])
      (test-equal? "Composition Just"
                   (map compose-f value)
                   ((compose fcompose-f fcompose-g) value)))))



