#lang racket/base

(require racket/generic)

(provide gen:Monoid
         Monoid?
         Monoid/c
         mempty)

(define-generics Monoid
  (mempty Monoid)

  #:defaults
  ([string? (define (mempty _) "")]
   [list? (define (mempty _) (list))]
   [vector? (define (mempty _) (vector))]
   [procedure? (define ((mempty _) x) x)]))

(module+ test
  (require rackunit)
  (test-case "<Monoid>:mempty"
    (check-equal? "" (mempty "sb"))
    (check-equal? (list) (mempty (list 1 2 3)))))
