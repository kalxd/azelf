#|函数相关宏及辅助函数|#
#lang racket/base

(require racket/contract
         racket/function
         (for-syntax racket/base))

(provide define/curry)

;;; 一次性结合了define/contract和curry，意义非比寻常。
(define-syntax (define/curry stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ...)
     #'(begin
         (define g (let ()
                     (define/contract (f arg ...)
                       body ...)
                     f))
         (define name (curry g)))]))

(module+ test
  (require rackunit)

  (test-case "<functiond>: define/curry"
    (define/curry (my/sub x y)
      (->i ([x positive?]
            [y (x) (<=/c x)])
           [result positive?])
      (- x y))

    (check-equal? 1 (my/sub 2 1))
    (check-equal? 1 ((my/sub 2) 1))
    (check-exn exn:fail:contract?
               (λ ()
                 (my/sub 1 2))
               )))
