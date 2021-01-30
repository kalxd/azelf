#lang racket/base

(require racket/generic
         racket/contract)

(provide gen:Monoid
         Monoid?
         Monoid/c
         mempty

         guard)

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

(define/contract (guard b m)
  (-> boolean? Monoid? Monoid?)
  (if b m (mempty m)))

(module+ test
  (test-case "<Monoid>:guard"
    (check-equal? "hello" (guard #t "hello"))
    (check-equal? "" (guard #f "hello"))))
