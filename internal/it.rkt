#|
提供it关键字。
|#

#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     racket/list))

(provide it
         expand-it)

(define-syntax-parameter it
  (λ (stx)
    (raise-syntax-error (syntax-e stx)
                        "只能在特定语法中使用！")))

; 检测代码中是否包装it关钕字。
(define-for-syntax (has-it? xs)
  (and (list? xs)
       (for/or ([x xs])
         (cond
           [(list? x) (has-it? x)]
           [else (eq? 'it x)]))))

; 展开it代码块。
(define-syntax (expand-it stx)
  (define stx-list (syntax->datum stx))
  (if (has-it? stx-list)
      (syntax-case stx ()
        [(_ (op ...))
         #'(λ (arg)
             (syntax-parameterize ([it (make-rename-transformer #'arg)])
               (op ...)))])
      (syntax-case stx ()
        [(_ fn) #'fn])))
