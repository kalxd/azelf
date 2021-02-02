#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract)

(export-from "./eq.rkt")

(provide (all-defined-out))

(define/contract (/= a b)
  (-> Eq? Eq? boolean?)
  (not (= a b)))

(module+ test
  (require rackunit)
  (test-case "<Eq>:/="
    (check-true (/= 1 2))
    (check-true (/= "hello" "HELLO"))))
