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

(module+ test
  (require rackunit)

  (test-case "<Function>: define/curry"
    (define/curry (my/add x y z)
      (+ x y z))

    (check-equal? 3 (my/add 1 1 1))
    (check-equal? 3 ((my/add 1 1) 1))))

;;; 一次性结合了define/contract和curry，意义非比寻常。
(define-syntax (define/curry/contract stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ...)
     #'(begin
         (define f (let ()
                     (define/contract (name arg ...)
                       body ...)
                     name))
         (define name (curry f)))]))

(module+ test
  (require rackunit)

  (test-case "<function>: define/curry"
    (define/curry/contract (my/sub x y)
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

(define-for-syntax (gen-n-vars stx)
  (define n (syntax->datum stx))
  (for/list ([i (in-range 0 n)])
    (string->symbol (format "a~A" i))))

; 柯化里不定长参数的函数，需要指定参数个数。
(define-syntax (curry/n stx)
  (syntax-case stx ()
    [(_ f n)
     (let ([as (gen-n-vars #'n)])
       (with-syntax ([(args ...) as])
         #'(let ([f (λ (args ...) (f args ...))])
             (curry f))))]))

(module+ test
  (test-case "<function>: curry/n"
    (define test/add2 (curry/n + 2))
    (check-equal? 4 (test/add2 1 3))
    (check-equal? 4 ((test/add2 1) 3))))
