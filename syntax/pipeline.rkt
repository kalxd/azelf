#lang racket/base

(require (only-in "../internal/keyword.rkt"
                  it
                  expand-it)
         (for-syntax racket/base))

(provide ->>
         <-<)

(define-syntax (->> stx)
  (syntax-case stx ()
    [(_ a) #'a]

    [(_ a f)
     #'(let ([g (expand-it f)])
         (g a))]

    [(_ a f fs ...)
     #'(let* ([g (expand-it f)]
              [b (g a)])
         (->> b fs ...))]))


(define-syntax (<-< stx)
  (syntax-case stx ()
    [(_ f ...)
     (let* ([fs (syntax->list #'(f ...))]
            [gs (for/list ([f fs])
                  `(expand-it ,f))])
       (with-syntax ([(fs ...) gs])
         #'(compose fs ...)))]))

(module+ test
  (require rackunit)

  (test-case "->> pipeline"
    (define a (->> 10
                   (λ (x) (+ 1 x))
                   (+ it it)
                   (- it 1)))
    (check-equal? 21 a))

  (test-case "<-< compose"
    (define f (<-< number->string (+ 10 it)))

    (check-equal? "20" (f 10))))
