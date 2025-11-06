#lang typed/racket/base

(require racket/stxparam
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(provide ->>)

(define-syntax-parser pipeline->>
  ; 一堆废话。
  [(_ e:expr f:expr fs:expr ...)
   #'(pipeline->> (f e) fs ...)]
  ; 最后两句话。
  [(_ e:expr f:expr)
   #'(f e)]
  ; 最后一条语句。
  [(_ e:expr) #'e])

(define-syntax-rule (->> body ...)
  (pipeline->> body ...))
