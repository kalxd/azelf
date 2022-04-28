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

(define-syntax-rule (define/match1/contract name contract-body body ...)
  (define/contract (name arg)
    contract-body
    (syntax-parameterize
        ([it (make-rename-transformer #'arg)])
      (match arg
        body ...))))

(define-syntax-rule (define/match/contract (name args ...) contract-body body ...)
  (define/curry/contract (name args ...)
    contract-body
    (match* (args ...)
      body ...)))
