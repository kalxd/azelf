#lang racket/base

(require racket/match
         racket/contract
         "./curry.rkt"

         (for-syntax racket/base))

(provide define/match/contract)

(define-syntax (define/match/contract stx)
  (syntax-case stx ()
    [(_ (name args ...) contract-body body ...)
     #'(define/curry/contract (name args ...)
         contract-body
         (match* (args ...)
           body ...))]))

(module+ test
  (require rackunit)

  (test-case "<match>: define/contract/match"
    (define/match/contract (list/sum xs)
      (-> (listof positive?) (or/c zero? positive?))
      [((list)) 0]
      [((list a as ...)) (+ a (list/sum as))])

    (check-equal? 10 (list/sum (list 1 2 3 4)))
    (check-exn exn:fail:contract?
               (λ ()
                 (list/sum (list 1 -2 3 -4))))))
