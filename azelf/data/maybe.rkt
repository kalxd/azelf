#lang racket/base

(require racket/contract
         racket/match
         "functor.rkt"
         (for-syntax racket/base))

(provide Maybe/c
         Nothing
         nothing
         Just)

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

(define/contract (maybe def f value)
  (-> any/c
      (-> any/c any/c)
      (Maybe/c any/c)
      any/c)
  (match value
    [(Just x) (f x)]
    [_ def]))

(module+ test
  (require rackunit)
  (define just (Just 1))

  (test-case "Maybe:maybe"
    (check-equal? 0 (maybe 0 add1 nothing))
    (check-equal? 2 (maybe 0 add1 just))
    (check-equal? "1" (maybe 0 number->string just)))

  (test-case "Maybe:Functor"
    (define value 1)
    (define x (fmap add1 (Just value)))
    (define y (Just (add1 value)))
    (check-equal? x y)
    (check-eq? nothing
               (fmap add1 nothing))))
