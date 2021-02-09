#lang racket/base

(require racket/generic
         (only-in racket/list
                  append-map))

(provide gen:Apply
         Apply?
         Apply/c
         ap)

(define-generics Apply
  (ap f Apply)
  #:defaults
  ([procedure? (define ((ap f g) x)
                 (f x (g x)))]
   [list? (define (ap fs xs)
            (define (map-f x)
              (for/list ([f fs])
                (f x)))
            (append-map map-f xs))]))

(module+ test
  (require rackunit)

  (test-case "<Apply>:ap"
    (check-equal? 3 ((ap + add1) 1))
    (check-equal? '(2 2 3 3 4 4 5 5)
                  (ap (list add1 add1)
                      '(1 2 3 4)))))
