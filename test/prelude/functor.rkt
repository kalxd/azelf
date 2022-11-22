#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define functor-ts
  (test-suite
   "Functor Typeclass"
   (test-case "Functor Maybe"
     (check-equal? nothing (map add1 nothing))
     (check-equal? (Just (Just 5)) (map (map add1) (Just (Just 4)))))))

(define task-list (list functor-ts))
