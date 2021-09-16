#lang racket/base

(require racket/match
         racket/contract
         racket/stxparam

         (only-in "../internal/keyword.rkt"
                  it)
         (for-syntax racket/base))

(define-syntax (define/match1 stx)
  (syntax-case stx ()
    [(_ name body ...)
     #'(define (name x)
         (syntax-parameterize
             ([it (make-rename-transformer #'x)])
           (match x body ...)))]))

(module+ test
  (require rackunit)

  (test-case "<match>: define/match1"
    (define/match1 f
      [1 it]
      [2 (add1 it)]
      [(list a b) (+ a b)]
      [else 0])

    (check-equal? 1 (f 1))
    (check-equal? 3 (f 2))
    (check-equal? 10 (f (list 3 7)))
    (check-equal? 0 (f "hello"))))

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
