#lang racket/base

(require "../internal/keyword.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide ->>
         <-<)

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

(define-syntax (<-< stx)
  (syntax-case stx ()
    [(_ f ...)
     (let* ([fs (syntax->list #'(f ...))]
            [gs (for/list ([f fs])
                  `(expand-it ,f))])
       (with-syntax ([(fs ...) gs])
         #'(compose fs ...)))]))

(module+ test
  (test-case "<-< compose"
    (define f (<-< number->string (+ 10 it)))

    (check-equal? "20" (f 10))))
