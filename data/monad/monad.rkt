#lang racket/base

(require racket/generic
         (only-in racket/list
                  append-map))

(provide gen:Monad
         Monad?
         Monad/c
         bind)

(define-generics Monad
  (bind Monad f)
  #:defaults
  ([list? (define (bind ma f)
            (append-map f ma))]))

(module+ test
  (require rackunit)

  (test-case "<Monad>:bind"
    (check-equal? (list 1 1 2 2 3 3)
                  (bind (list 1 2 3)
                        (λ (x) (list x x))))))
