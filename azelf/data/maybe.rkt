#lang racket/base

(require racket/contract
         racket/match
         "functor.rkt"
         (for-syntax racket/base))

(provide Maybe/c
         Nothing
         Nothing?
         nothing
         Just
         Just?

         maybe
         maybe->
         ->maybe)

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

(define/contract (maybe-> x value)
  (->i ([x any/c]
        [f (Maybe/c any/c)])
       [result (x f) (or/c x any/c)])
  (match value
    [(Just a) a]
    [_ x]))

(define/contract (->maybe x)
  (->i ([x any/c])
       [result (x) (Maybe/c x)])
  (if x
      (Just x)
      nothing))

(module+ test
  (require rackunit
           (only-in racket/function
                    identity))
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
                   ((compose fcompose-f fcompose-g) value))))

  (test-case "<Maybe>:maybe"
    (check-equal? 0 (maybe 0 add1 nothing))
    (check-equal? 2 (maybe 0 add1 just))
    (check-equal? "1" (maybe 0 number->string just)))

  (test-case "<Maybe>:maybe->"
    (check-equal? 1 (maybe-> 1 nothing))
    (check-equal? 1 (maybe-> 2 (Just 1))))

  (test-case "<Maybe>:->maybe"
    (check-equal? (Just 1) (->maybe 1))
    (check-equal? nothing (->maybe #f))))
