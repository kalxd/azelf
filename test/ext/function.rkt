#lang s-exp "../internal/reader.rkt"

(require "../../main.rkt")

(define function-ts
  (test-suite
   "ext : function"

   (test-case "<function>: const"
    (check-equal? 1 (const 1 2))
    (check-equal? 1 ((const 1) 2)))))

(define task-list
  (list function-ts))
