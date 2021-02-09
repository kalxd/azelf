#lang racket/base

(require "../functor/functor.rkt"
         "../eq/eq.rkt"
         "../ord/ord.rkt"
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

  ; Functor
  #:methods gen:Functor
  [(define (map f self) self)])

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

  ; Functor
  #:methods gen:Functor
  [(define (map f self)
     (let* ([a (Right-value self)]
            [x (f a)])
       (Right x)))])

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

  (test-case "<Either>:Functor"
    (define left (Left "hello"))
    (define right (Right 1))
    (check-equal? left (map add1 left))
    (check-equal? (Right 2) (map add1 right))))
