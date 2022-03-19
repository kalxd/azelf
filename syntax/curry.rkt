#|函数相关宏及辅助函数|#
#lang racket/base

(require racket/contract
         racket/function
         (for-syntax racket/base
                     racket/syntax))

(provide define/curry
         define/curry/contract
         curry/n)

; 直接定义成柯里化函数，不用再套用curry。
(define-syntax-rule (define/curry (name args ...) body ...)
  (begin
    (define f
      (let ()
        (define (name args ...)
          body ...)
        name))
    (define name (curry f))))

;;; 一次性结合了define/contract和curry，意义非比寻常。
(define-syntax-rule (define/curry/contract (name args ...) body ...)
  (begin
    (define f (let ()
                (define/contract (name args ...)
                  body ...)
                name))
    (define name (curry f))))

(define-for-syntax (gen-n-vars stx)
  (define n (syntax->datum stx))
  (for/list ([i (in-range 0 n)])
    (string->symbol (format "a~A" i))))

; 柯化里不定长参数的函数，需要指定参数个数。
(define-syntax (curry/n stx)
  (syntax-case stx ()
    [(_ n f)
     (let ([as (gen-n-vars #'n)])
       (with-syntax ([(args ...) as])
         #'(let ([f (λ (args ...) (f args ...))])
             (curry f))))]))
