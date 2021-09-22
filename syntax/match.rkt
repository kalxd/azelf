#lang racket/base

(require racket/match
         racket/contract

         (only-in "../internal/keyword.rkt"
                  it)
         (for-syntax racket/base))

(define-syntax (define/contract/match stx)
  (syntax-case stx ()
    [(_ (name args ...) contract-body body ...)
     #'(define/contract (name args ...)
         contract-body
         (match* (args ...)
           body ...))]))

(module+ test
  (require rackunit)

  (test-case "<match>: define/contract/match"
    (define/contract/match (list/sum xs)
      (-> (listof positive?) (or/c zero? positive?))
      [((list)) 0]
      [((list a as ...)) (+ a (list/sum as))])

    (check-equal? 10 (list/sum (list 1 2 3 4)))
    (check-exn exn:fail:contract?
               (λ ()
                 (list/sum (list 1 -2 3 -4))))))
