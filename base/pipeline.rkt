#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     racket/list))

(provide ->>
         <-<
         it)

(define-syntax-parameter it
  (λ (stx)
    (raise-syntax-error (syntax-e stx)
                        "只能在特定几个宏中使用！")))

(define-for-syntax (has-it? xs)
  (and (list? xs)
       (for/or ([x xs])
         (cond
           [(list? x) (has-it? x)]
           [else (eq? 'it x)]))))

(define-for-syntax (expand-it f)
  (define ff (syntax->datum f))
  (if (has-it? ff)
      (syntax-case ff ()
        [(body ...)
         #'(λ (arg)
             (syntax-parameterize ([it (make-rename-transformer #'arg)])
               (body ...)))])
      f))

(define-syntax (->> stx)
  (syntax-case stx ()
    [(_ a) #'a]

    [(_ a f)
     (let ([g (expand-it #'f)])
       (with-syntax ([g g])
         #'(g a)))]

    [(_ a f fs ...)
     (let ([g (expand-it #'f)])
       (with-syntax ([g g])
         #'(let ([b (g a)])
             (->> b fs ...))))]))

(module+ test
  (require rackunit)

  (test-case "->> pipeline"
    (define a (->> 10
                   (λ (x) (+ 1 x))
                   (+ it it)
                   (- it 1)))
    (check-equal? 21 a)))

(define-syntax (<-< stx)
  (syntax-case stx ()
    [(_ f ...)
     (let* ([fs (syntax->list #'(f ...))]
            [gs (for/list ([f fs])
                  (expand-it f))])
       (with-syntax ([(fs ...) gs])
         #'(compose fs ...)))]))

(module+ test
  (test-case "<-< compose"
    (define f (<-< number->string (+ 10 it)))

    (check-equal? "20" (f 10))))
