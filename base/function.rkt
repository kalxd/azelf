#|函数相关宏及辅助函数|#
#lang racket/base

(require racket/contract
         racket/function
         (for-syntax racket/base))

(provide define/curry
         curry/contract)

(define-syntax (define/curry stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ...)
     #'(begin
         (define f (let ()
                     (define (name arg ...)
                       body ...)
                     name))
         (define name (curry f)))]))

(module+ test
  (require rackunit)
  (define/curry (my/add x y z)
    (+ x y z))

  (test-case "<Function>:define/curry"
    (check-equal? 3 (my/add 1 1 1))
    (check-equal? 3 ((my/add 1 1) 1))))

;;; 一次性结合了define/contract和curry，意义非比寻常。
(define-syntax (curry/contract stx)
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
    (curry/contract (my/sub x y)
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

;;; 囹助函数集合 ;;;
(define/contract (fmap-n f . xs)
  (->* (procedure?)
       #:rest (listof any/c)
       any/c)
  (if (memv #f xs)
      #f
      (apply f xs)))
