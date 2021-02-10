#lang racket/base

(require "../eq/eq.rkt"
         "../ord/ord.rkt"
         "../semigroup/semigroup.rkt"
         "../monoid/monoid.rkt"
         "../functor/functor.rkt"
         "../applicative/applicative.rkt"
         "../monad/monad.rkt"
         racket/contract
         racket/generic
         racket/match)

(provide Maybe/c
         Nothing
         Nothing?
         nothing
         Just
         Just?)

(struct Nothing []
  #:transparent

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (display "#<Nothing>" port))]

  ; Eq
  #:methods gen:Eq
  [(define (= a b)
     (Nothing? b))]

  ; Ord
  #:methods gen:Ord
  [(define (compare a b)
     (if (and (Nothing? a) (Nothing? b))
         eq
         lt))]

  ; Semigroup
  #:methods gen:Semigroup
  [(define (mappend a b) b)]

  ; Monoid
  #:methods gen:Monoid
  [(define (mempty a) nothing)]

  ; Functor
  #:methods gen:Functor
  [(define (map f self) self)]

  ; Applicative
  #:methods gen:Applicative
  [(define (pure _ x) (Just x))
   (define (ap f self) self)]

  #:methods gen:Monad
  [(define (bind self _) self)])

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

  ; Ord
  #:methods gen:Ord
  [(define/generic Ord/compare compare)
   (define (compare a b)
     (match (cons a b)
       [(cons (Just x) (Just y)) (Ord/compare x y)]
       [else gt]))]

  ; Semigroup
  #:methods gen:Semigroup
  [(define/generic Semigroup/mappend mappend)
   (define (mappend a b)
     (match (cons a b)
       [(cons (Just a) (Just b))
        (Just (Semigroup/mappend a b))]
       [else a]))]

  ; Monoid
  #:methods gen:Monoid
  [(define (mempty x) nothing)]

  ; Functor
  #:methods gen:Functor
  [(define (map f self)
     (let* ([x (Just-a self)]
            [y (f x)])
       (Just y)))]

  ; Applicative
  #:methods gen:Applicative
  [(define (pure _ x) (Just x))
   (define (ap f self)
     (match (cons f self)
       [(cons (Just f) (Just a)) (Just (f a))]
       [else f]))]

  ; Monad
  #:methods gen:Monad
  [(define (bind self f)
     (define v (Just-a self))
     (f v))])

(define (Maybe/c x)
  (or/c Nothing? (struct/c Just x)))

(module+ test
  (require rackunit
           (only-in racket/function
                    identity)
           "../ord/main.rkt")

  (define just (Just 1))

  (test-case "<Maybe>:Eq"
    (check-true (= nothing nothing))
    (check-false (= nothing (Just 2)))
    (check-true (= (Just 1) (Just 1)))
    (check-true (= (Just "hello") (Just "hello")))
    (check-false (= (Just #\A) (Just #\B))))

  (test-case "<Maybe>:Ord"
    (check-true (< (Just 1) (Just 10)))
    (check-true (< nothing (Just -10))))

  (test-case "<Maybe>:Semigroup"
    (check-equal? (Just "helloworld")
                  (mappend (Just "hello") (Just "world")))
    (check-pred Just? (mappend nothing (Just 1)))
    (check-pred Just? (mappend (Just 2) nothing)))

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
                   ((compose fcompose-f fcompose-g) value))))

    (test-case "<Maybe>:Applicative"
      (check-equal? (Just 1) (pure nothing 1))
      (check-equal? (Just 1) (pure (Just 10) 1))

      (check-equal? (Just 1)
                    (ap (Just add1) (Just 0)))
      (check-equal? nothing
                    (ap (Just add1) nothing))
      (check-equal? nothing
                    (ap nothing (Just 1)))
      (check-equal? nothing
                    (ap nothing nothing)))

    (test-case "<Maybe>:Monad"
      (define (f x)
        (Just (add1 x)))
      (check-equal? (Just 2)
                    (bind (Just 1) f))

      (check-equal? nothing
                    (bind nothing f))))
