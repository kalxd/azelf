#lang racket/base

(require racket/match
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
