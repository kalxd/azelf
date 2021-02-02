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

(define/contract (=* a b . rest)
  (->* (Eq? Eq?)
       ()
       #:rest (listof Eq?)
       boolean?)
  (for/and ([i (cons b rest)])
    (= a b)))

(module+ test
  (test-case "<Eq>:=*"
    (check-true (=* 1 1 1 1))
    (check-true (=* #\A #\A #\A))
    (check-false (=* 1 2 1 1))))
