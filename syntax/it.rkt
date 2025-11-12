#lang typed/racket/base

(require racket/stxparam
         (for-syntax racket/base))

(provide it
         expand-it)

(define-syntax-parameter it
  (λ (stx)
    (raise-syntax-error (syntax-e stx)
                        "只能在特定语法中使用！")))

(define-for-syntax (check-it-syntax syns)
  (cond
    [(list? syns)
     (for/or ([x syns])
       (check-it-syntax x))]
    [else (eq? 'it syns)]))

(define-syntax (expand-it stx)
  (if (check-it-syntax (syntax->datum stx))
      (syntax-case stx ()
        [(_ fn)
         #'(λ (arg)
             (syntax-parameterize ([it (make-rename-transformer #'arg)])
               #'fn))])
      (syntax-case stx ()
        [(_ fn)
         #'fn])))

(module+ test
  (expand-it (let ([a 1]) (+ it it))))
