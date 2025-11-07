#lang typed/racket/base

(require typed/json)

(define value : JSExpr 'null)

(: is-null? (-> JSExpr Boolean : 'null))
(define (is-null? input)
  (eq? input 'null))

(: print-null (-> 'null Void))
(define (print-null value)
  (displayln value))

(module+ main
  (define value : JSExpr 'null)

  (cond
    [(is-null? value) (print-null value)]
    [else (displayln "is not null")]))
