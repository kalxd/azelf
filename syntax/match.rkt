#lang racket/base

(require racket/stxparam
         racket/match
         racket/contract
         "./curry.rkt"
         (only-in "../internal/keyword.rkt"
                  it)

         (for-syntax racket/base))

(provide define/match1
         define/match1/contract
         define/match/contract)

(define-syntax-rule (define/match1 name body ...)
  (define (name arg)
    (syntax-parameterize ([it (make-rename-transformer #'arg)])
      (match arg
        body ...))))

(module+ test
  (require rackunit)
  (test-case "<match>: define/match1"
    (define/match1 inc
      [1 0]
      [_ (add1 it)])
    (check-equal? 0 (inc 1))
    (check-equal? 3 (inc 2))))

(define-syntax-rule (define/match1/contract name contract-body body ...)
  (define/contract (name arg)
    contract-body
    (syntax-parameterize
        ([it (make-rename-transformer #'arg)])
      (match arg
        body ...))))

(module+ test
  (test-case "<match>: define/match1/contract"
    (define/match1/contract inc
      (-> positive? positive?)
      [1 10]
      [10 1]
      [2 "2"]
      [_ (add1 it)])
    (check-equal? 10 (inc 1))
    (check-equal? 1 (inc 10))
    (check-exn exn:fail:contract?
               (λ () (inc 2)))))

(define-syntax-rule (define/match/contract (name args ...) contract-body body ...)
  (define/curry/contract (name args ...)
    contract-body
    (match* (args ...)
      body ...)))

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
