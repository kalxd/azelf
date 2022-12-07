#lang racket/base

(require "./keyword.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(provide ->>
         <-<
         >->)

; 内部->>最基本实现。
(define-syntax-parser pipeline->>
  ; 更多语句。
  [(_ var:expr f:expr fs:expr ...+)
   #'(pipeline->> (pipeline->> var f)
                  fs ...)]

  ; 副作用语句。
  [(_ var:expr ((~datum !) eff:expr ...+) fs:expr ...)
   #'(let ([a var])
       ((expand-it (begin eff ...)) var)
       (pipeline->> a fs ...))]

  ; 只有两句。
  [(_ var:expr f:expr)
   #'((expand-it f) var)]

  ; 最后一句
  [(_ e:expr) #'e])

(define-syntax-rule (->> body ...)
  (break-wrap (pipeline->> body ...)))

(define-syntax-parser >->
  ; 只有一条语句。
  [(_ f:expr)
   #'(expand-it f)]

  ; 副作用。
  [(_ f:expr ((~literal !) eff:expr ...+) fs:expr ...+)
   #'(compose (compose>-> f)
              (λ (a)
                (pipeline->> a
                             (! eff ...)))
              (compose>-> fs ...))]

  ; 许多句。
  [(_ f:expr fs:expr ...)
   #'(compose (compose>-> fs ...)
              (compose>-> f))])

(define-syntax (<-< stx)
  (define fs (reverse (cdr (syntax->datum stx))))
  (datum->syntax stx
                 `(>-> ,@fs)))
