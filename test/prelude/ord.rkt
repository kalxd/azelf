#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define ord-ts
  (test-suite
   "Ord Typeclass"
   (test-case "Ord Maybe"
     (check-true (< nothing (Just nothing)))
     (check-true (> (Just (list 1)) (Just (list)))))))

(define task-list (list ord-ts))
