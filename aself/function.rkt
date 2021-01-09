#lang racket/base

(require racket/stxparam
         (for-syntax racket/base))

(provide ->>
         tap)

(define-syntax-parameter it
  (λ (stx)
    (raise-syntax-error (syntax-e stx)
                        "只能在特定几个宏中使用！")))

(define-for-syntax (has-it? xs)
  (for/or ([x xs])
    (cond
      [(list? x) (has-it? x)]
      [else (eq? 'it x)])))

(define-syntax (expand-it stx)
  (define stx-list (cdr (syntax->datum stx)))
  (if (has-it? stx-list)
      (syntax-case stx ()
        [(_ body)
         #'(λ (arg)
             (syntax-parameterize ([it (make-rename-transformer #'arg)])
               body))])
      (datum->syntax stx
                     (car stx-list))))

(define-syntax (->> stx)
  (syntax-case stx ()
    [(_ a) #'a]

    [(_ a f)
     #'(let ([g (expand-it f)])
         (g a))]

    [(_ a f fs ...)
     #'(let* ([g (expand-it f)]
              [b (g a)])
         (->> b fs ...))]))

(define (tap f)
  (λ (x)
    (f x)
    x))
