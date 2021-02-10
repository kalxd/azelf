#lang racket/base

(require "../eq/eq.rkt"
         "../ord/ord.rkt"
         "../semigroup/semigroup.rkt"
         "../functor/functor.rkt"
         "../applicative/applicative.rkt"
         "../monad/monad.rkt"
         racket/contract
         racket/match
         racket/generic)

(provide Left
         Left?
         Right
         Right?
         Either/c)

(struct Left [value]
  #:transparent

  ; Eq
  #:methods gen:Eq
  [(define/generic Eq/= =)
   (define (= a b)
     (match (cons a b)
       [(cons (Left x) (Left y)) (Eq/= x y)]
       [else #f]))]

  ; Ord
  #:methods gen:Ord
  [(define/generic Ord/compare compare)
   (define (compare a b)
     (match (cons a b)
       [(cons (Left x) (Left y)) (Ord/compare x y)]
       [else lt]))]

  ; Semigroup
  #:methods gen:Semigroup
  [(define (mappend a b) a)]

  ; Functor
  #:methods gen:Functor
  [(define (map f self) self)]

  ; Applicative
  #:methods gen:Applicative
  [(define (pure _ x) (Right x))
   (define (ap f self) self)]

  #:methods gen:Monad
  [(define (bind self _) self)])

(struct Right [value]
  #:transparent

  ; Eq
  #:methods gen:Eq
  [(define/generic Eq/= =)
   (define (= a b)
     (match (cons a b)
       [(cons (Right x) (Right y)) (Eq/= x y)]
       [else #f]))]

  ; Ord
  #:methods gen:Ord
  [(define/generic Ord/compare compare)
   (define (compare a b)
     (match (cons a b)
       [(cons (Right x) (Right y)) (Ord/compare x y)]
       [else gt]))]

  ; Semigroup
  #:methods gen:Semigroup
  [(define/generic Semigroup/mappend mappend)
   (define (mappend a b)
     (match (cons a b)
       [(cons (Right a) (Right b))
        (Right (Semigroup/mappend a b))]
       [else b]))]

  ; Functor
  #:methods gen:Functor
  [(define (map f self)
     (let* ([a (Right-value self)]
            [x (f a)])
       (Right x)))]

  ; Applicative
  #:methods gen:Applicative
  [(define (pure _ x) (Right x))
   (define (ap f self)
     (match (cons f self)
       [(cons (Right f) (Right a)) (Right (f a))]
       [else f]))]

  #:methods gen:Monad
  [(define (bind self f)
     (define v (Right-value self))
     (f v))])

(define (Either/c b a)
  (or/c (struct/c Left b)
        (struct/c Right a)))

(module+ test
  (require rackunit
           "../ord/main.rkt")

  (test-case "<Either>:Eq"
    (check-true (= (Left 1) (Left 1)))
    (check-true (= (Right "hello") (Right "hello")))
    (check-false (= (Left 1) (Right 1)))
    (check-false (= (Right "hello") (Right "world"))))

  (test-case "<Either>:Ord"
    (check-true (<= (Left 1) (Left 2)))
    (check-true (>= (Right #\Z) (Right #\A)))
    (check-true (< (Left 10) (Right 1))))

  (test-case "<Either:Semigroup"
    (check-equal? (Right "12")
                  (mappend (Right "1") (Right "2")))
    (check-equal? (Left "2")
                  (mappend (Right "1") (Left "2")))
    (check-equal? (Left "1")
                  (mappend (Left "1") (Right "2")))
    (check-equal? (Left "1")
                  (mappend (Left "1") (Left "2"))))

  (test-case "<Either>:Functor"
    (define left (Left "hello"))
    (define right (Right 1))
    (check-equal? left (map add1 left))
    (check-equal? (Right 2) (map add1 right)))

  (test-case "<Either>:Applictive"
    (check-equal? (Right 1) (pure (Left 2) 1))
    (check-equal? (Right 1) (pure (Right 10) 1))
    (check-equal? (Right 2)
                  (ap (Right add1) (Right 1)))
    (check-equal? (Left 1)
                  (ap (Right add1) (Left 1)))
    (check-equal? (Left add1)
                  (ap (Left add1) (Right 1)))
    (check-equal? (Left add1)
                  (ap (Left add1) (Right 1))))

  (test-case "<Either>:Monad"
    (define f (compose Right add1))
    (check-equal? (Right 2)
                  (bind (Right 1) f))
    (check-equal? (Left 2)
                  (bind (Left 2) f))))
