#lang racket/base

(require "../internal/keyword.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide ->>
         <-<
         >->)

; 内部->>最基本实现。
(define-syntax pipeline->>
  (syntax-parser
    ; 更多语句。
    [(_ var:expr f:expr fs:expr ...+)
     #'(pipeline->> (pipeline->> var f)
            fs ...)]

    ; 副作用语句。
    [(_ var:expr ((~literal !) eff:expr ...+) fs:expr ...)
     #'(let ([a var])
         ((expand-it (begin eff ...)) var)
         (pipeline->> a fs ...))]

    ; 只有两句。
    [(_ var:expr f:expr)
     #'((expand-it f) var)]

    ; 最后一句
    [(_ e:expr) #'e]))

(define-syntax-rule (->> body ...)
  (break-wrap (pipeline->> body ...)))

(module+ test
  (require rackunit)

  (test-case "<pipeline>: ->>"
    ; 常规操作，最低要求。
    (define a (->> 10
                   (λ (x) (+ 1 x))
                   (+ it it)
                   (- it 1)))
    (check-equal? 21 a)

    ; 副作用测试。
    (check-equal? 10
                  (->> 1
                       (! (add1 it))
                       (+ it 9)))

    ; 中断测试。
    (check-equal? 10
                  (->> 1
                       (when (= it 1)
                         (break 10))
                       add1))))

(define-syntax compose>->
  (syntax-parser
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
                (compose>-> f))]))

(define-syntax-rule (>-> body ...)
  (break-wrap (compose>-> body ...)))

(define-syntax (<-< stx)
  (define fs (reverse (cdr (syntax->datum stx))))
  (datum->syntax stx
                 `(>-> ,@fs)))

(module+ test
  (test-case "<pipeline>: <-<"
    ; 最低要求。
    (define f (<-< number->string
                   (+ 10 it)))

    (check-equal? "20" (f 10))

    ; 副作用测试。
    (define g (<-< add1
                   (! (+ 10 it)
                      (+ it it))
                   add1))
    (check-equal? 3 (g 1))))
