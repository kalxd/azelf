#lang racket/base

(provide (all-defined-out))

; (gen-n-args 3)
; 自动生成(a0 a1 a2)这样的列表，常用于自动生成函数参数的场景。
(define (gen-n-args stx)
  (define n (syntax->datum stx))
  (for/list ([i (in-range 0 n)])
    (string->symbol (format "a~a" i))))
