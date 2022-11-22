#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define eq-ts
  (test-suite
   "Eq Typeclass"
   (test-case "eq primitive"
     (check-true (= 1 1))
     (check-true (= "" "")))))

(define task-list
  (list eq-ts))
