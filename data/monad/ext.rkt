#lang racket/base

(require racket/contract
         racket/match
         (only-in racket/function
                  identity)
         "./monad.rkt")

(provide >>=
         >>=*
         =<<
         <=<
         >=>
         join)

(define >>= bind)

(define/contract (>>=* ma . fs)
  (->* (Monad?)
       #:rest
       (listof (-> any/c Monad?))
       Monad?)
  (match fs
    [(list g f ...) (apply >>=* (cons (>>= ma g) f))]
    [(list f) (>>= ma f)]
    [else ma]))

(module+ test
  (require rackunit
           racket/function
           "../maybe/maybe.rkt")
  (define my/inc
    (compose Just add1))

  (test-case "<Monad>:>>=*"
    (check-equal? (Just 3)
                  (>>=* (Just 0) my/inc my/inc my/inc))
    (check-equal? nothing
                  (>>=* nothing my/inc (const nothing) my/inc my/inc))))

(define/contract (=<< f ma)
  (-> (-> any/c Monad?)
      Monad?
      Monad?)
  (bind ma f))

(module+ test
  (test-case "<Monad>:=<<"
    (check-equal? (Just 2)
                  (=<< my/inc (Just 1)))
    (check-equal? nothing
                  (=<< my/inc nothing))))

(define/contract ((<=< f g) x)
  (-> (-> any/c Monad?)
      (-> any/c Monad?)
      (-> any/c Monad?))
  (bind (g x) f))

(module+ test
  (define ->just-string
    (compose Just number->string))

  (test-case "<Monad>:<=<"
    (check-equal? (Just "2")
                  ((<=< ->just-string my/inc) 1))))

(define/contract ((>=> f g) x)
  (-> (-> any/c Monad?)
      (-> any/c Monad?)
      (-> any/c Monad?))
  (bind (f x) g))

(module+ test
  (test-case "<Monad>:>=>"
    (check-equal? (Just "2")
                  ((>=> my/inc ->just-string) 1))))

(define/contract (join ma)
  (-> Monad? Monad?)
  (bind ma identity))

(module+ test
  (test-case "<Monad>:join"
    (check-equal? (Just 1)
                  (join (Just (Just 1))))
    (check-equal? (list 1 2 3)
                  (join (list (list 1) (list 2 3))))))
