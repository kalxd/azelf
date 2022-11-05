#|函数相关宏及辅助函数|#
#lang racket/base

(require racket/contract
         racket/function
         syntax/parse/define

         (for-syntax racket/base
                     (only-in racket/list
                              range)))

(provide define/curry
         define/curry/contract
         curry/n)


; 直接定义成柯里化函数，不用再套用curry。
(define-syntax-rule (define/curry (name args ...) body ...)
  (define name
    (curry (let ([name (λ (args ...) body ...)])
             name))))

;;; 一次性结合了define/contract和curry，意义非比寻常。
(define-syntax-rule (define/curry/contract (name args ...) body ...)
  (define name
    (curry (let ()
             (define/contract (name args ...)
               body ...)
             name))))

; 柯化里不定长参数的函数，需要指定参数个数。
(define-syntax-parser curry/n
  [(_ n:nat f:id)
   (with-syntax ([(args ...)
                  (generate-temporaries (range (syntax->datum #'n)))])
     #'(let ([f (λ (args ...)
                  (f args ...))])
         (curry f)))])
