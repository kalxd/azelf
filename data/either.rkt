#lang racket/base

(require racket/contract

         "./functor.rkt")

(provide Left
         Left?
         Right
         Right?
         Either/c

         either/catch)

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

(define/contract (either/catch action)
  (-> (-> any/c) (Either/c exn:fail? any/c))
  (with-handlers ([exn:fail? (λ (e) (Left e))])
    (let ([x (action)])
      (Right x))))

(module+ test
  (test-case "<Either>:either/catch-do"
    (check-pred Left?
                (either/catch
                 (λ ()
                   (/ 1 0))))
    (check-pred Right?
                (either/catch
                 (λ ()
                   (/ 1 1))))))