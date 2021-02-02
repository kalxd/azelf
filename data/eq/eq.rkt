#lang racket/base

(require racket/generic
         racket/bool
         (rename-in racket/base
                    (= number=?)))

(provide gen:Eq
         Eq?
         Eq/c
         =)

(define-generics Eq
  (= Eq a)
  #:defaults
  ([number? (define = number=?)]
   [string? (define = string=?)]
   [char? (define = char=?)]
   [boolean? (define = boolean=?)]
   [symbol? (define = symbol=?)]))

(module+ test
  (require rackunit)
  (test-case "<Eq>:="
    (check-true (= 1 1))
    (check-true (= "hello" "hello"))
    (check-true (= #t #t))
    (check-true (= 'sym 'sym))))
