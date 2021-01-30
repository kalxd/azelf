#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract)

(export-from "./monoid.rkt")

(provide guard)

(define/contract (guard b m)
  (-> boolean? Monoid? Monoid?)
  (if b m (mempty m)))

(module+ test
  (require rackunit)
  (test-case "<Monoid>:guard"
    (check-equal? "hello" (guard #t "hello"))
    (check-equal? "" (guard #f "hello"))))

