#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define eq-ts
  (test-suite
   "Eq Typeclass"
   (test-case "eq primitive"
     (check-true (= 1 1))
     (check-true (= "" "")))
   (test-case "eq Maybe"
     (check-true (= nothing nothing))
     (check-true (= (Just 1) (Just 1))))))

(define task-list
  (list eq-ts))
