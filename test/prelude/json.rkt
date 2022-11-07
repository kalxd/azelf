#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define ->json-ts
  (test-suite
   "prelude: ->json"
   (test-case "->json: string?"
     (check-equal? "hello world" (->json "hello world"))
     (check-equal? #t (->json #t))
     (check-equal? 10 (->json 10))
     (check-equal? (list 1 2 3 'null) (->json (list 1 2 3 nothing)))
     (check-equal? (hash 'a 1 'b 'null)
                   (->json (hash 'a 1 'b nothing))))))

(define task-list
  (list ->json-ts))
