#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/match
         syntax/parse
         (for-syntax racket/base
                     syntax/parse))

(export-from "./monad.rkt"
             "./ext.rkt")

(provide <-
         monad-do)

(define-syntax (<- stx)
  (raise-syntax-error '<- "只能在monad-do块中使用！" stx))

(define-syntax monad-do
  (syntax-parser
    #:literals [<- let]
    ; 单条语句
    [(_ e:expr) #'e]
    ; let赋值语句
    [(_ (let ([x:id e:expr] ...)) . rest)
     #'(let ([x e] ...)
         (monad-do . rest))]
    ; bind语法
    [(_ [pat:id (~and arrow <-) e:expr] . rest)
     #'(bind e
             (case-lambda [(pat) (monad-do . rest)]))]
    ; 一串语句
    [(_ e:expr . rest)
     #'(begin
         e
         (monad-do . rest))]))

(module+ test
  (require rackunit
           "../maybe/maybe.rkt")

  (test-case "<Monad>:do notation"
    (check-equal? nothing
                  (monad-do (x <- (Just 1))
                            (y <- nothing)
                            (Just (+ x y))))

    (check-equal? (Just 10)
                  (monad-do (x <- (Just 1))
                            (let ([x 4]
                                  [y 6]))
                            (let ([x (+ x y)]))
                            (Just x)))))
