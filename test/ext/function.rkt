#lang racket/base

(require rackunit
         "../../main.rkt")

(define function-ts
  (test-suite
   "ext : function"

   (test-case "<function>: const"
    (check-equal? 1 (const 1 2))
    (check-equal? 1 ((const 1) 2)))))

(module+ test
  (require "../internal/helper.rkt")
  (run-all-task (list function-ts)))
