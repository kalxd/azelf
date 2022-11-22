#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define applicative-ts
  (test-suite
   "Applicative Typeclass"
   (test-case "Applicative Maybe"
     (check-equal? nothing (<*> (Just add1) nothing))
     (check-equal? nothing (<*> nothing (Just 10)))
     (check-equal? (Just (Just 4))
                   (<$> Just (Just 4))))))

(define task-list (list applicative-ts))
