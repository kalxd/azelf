#lang typed/racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse)
         "./it.rkt")

(provide ->>)

(define-syntax-parser ->>
  ; 一堆废话。
  [(_ e:expr f:expr fs:expr ...+)
   #'(let ([val ((expand-it f) e)])
       (->> val fs ...))]
  ; 最后两句话。
  [(_ e:expr f:expr)
   #'((expand-it f) e)]
  ; 最后一条语句。
  [(_ e:expr) #'e])
