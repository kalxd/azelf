#lang racket/base

(require "../internal/it.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide nil/do
         nil/->>)

(define-syntax nil/do
  (syntax-parser
    ; 绑定语法。
    [(_ (var:id (~and (~literal <-)) e:expr) body ...+)
     #'(let ([var e])
         (and var
              (nil/do body ...)))]

    ; 定义语法。
    [(_ ((~literal let) ([x:id e:expr] ...+)) body ...+)
     #'(let* ([x e] ...)
         (nil/do body ...))]

    ; 提前中断操作。
    [(_ ((~literal break-when) condition:expr value:expr) body ...+)
     #'(if condition
           value
           (nil/do body ...))]

    ; 提前中断操作。
    [(_ ((~literal break-unless) condition:expr value:expr) body ...+)
     #'(if condition
           (nil/do body ...)
           value)]

    ; 无论如何都不会中断的代码块。
    [(_ ((~literal !) e:expr) body ...+)
     #'(begin e (nil/do body ...))]

    ; 最后一句。
    [(_ e:expr) #'e]

    ; 多条语句
    [(_ e1:expr e2:expr ...+)
     #'(and e1 (nil/do e2 ...))]))

(module+ test
  (require rackunit)

  (test-case "<nil>: nil/do绑定用法"
    (check-equal? 2 (nil/do (a <- 1) (add1 a)))
    (check-false (nil/do (a <- #f) (add1 a))))

  (test-case "<nil>: nil/do变量定义"
    (check-true (nil/do (let ([a #t])) a))
    (check-true (nil/do (let ([a #f] [b #t])) (or a b))))

  (test-case "<nil>: nil/do提前中断"
    (check-equal? 3 (nil/do (a <- 1)
                            (break-when (= a 1) 3)
                            2))
    (check-equal? 2 (nil/do (a <- 1)
                            (break-unless (= a 1) 3)
                            2)))

  (test-case "<nil>: nil/do自由代码块"
    (check-true (nil/do (a <- #t) (! (not a)) a))))

(define-syntax nil/->>
  (syntax-parser
    #:literals [it]
    ; 多条语句
    [(_ var:expr f:expr fs:expr ...)
     #'(let ([a var])
         (and a
              (let ([g (expand-it f)])
                (nil/->> (g a) fs ...))))]

    ; 最后一句。
    [(_ e:expr) #'e]))

(module+ test
  (test-case "<nil>: nil/->>"
    (check-equal? 3
                  (nil/->> 1
                           (+ it it)
                           add1))
    (check-false (nil/->> 1
                          (λ (x) #f)
                          add1))))
