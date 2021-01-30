#lang racket/base

(require racket/generic
         (only-in "../../internal/macro.rkt"
                  macro/id
                  macro/remap))

(provide gen:Show
         Show?
         Show/c)

(define-generics Show
  (->string Show)
  #:defaults
  [(string? (macro/id ->string))
   (number? (macro/remap ->string number->string))])

(module+ test
  (require rackunit)

  (test-case "Show typeclass"
    (check-equal? "1" (->string "1"))
    (check-equal? "1" (->string 1))))
