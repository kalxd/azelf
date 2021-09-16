#lang racket/base

(require racket/stxparam
         racket/match

         "./maybe.rkt"
         (only-in "../internal/keyword.rkt"
                  break)
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide maybe/do)

(define (private/->maybe a)
  (cond
    [(maybe? a) a]
    [else (->maybe a)]))

(define-syntax maybe-do-notation
  (syntax-parser
    ; 绑定语法。
    [(_ (var:id (~and (~literal <-)) e:expr) es:expr ...+)
     #'(match (private/->maybe e)
         [(Just var) (maybe/do es ...)]
         [(Nothing) nothing])]

    ; 赋值语法。
    [(_ ((~literal let) [var:id e:expr] ...+) es:expr ...+)
     #'(let ([var e] ...)
         (maybe/do es ...))]

    ; 副作用。
    [(_ ((~literal !) es:expr ...+) rs:expr ...+)
     #'(begin es ...
              (maybe/do rs ...))]

    ; 多条语句。
    [(_ e1:expr e2:expr ...+)
     #'(let ([a (private/->maybe e1)])
         (match a
           [(Nothing) a]
           [_ (maybe/do e2 ...)]))]

    ; 最后一条语句
    [(_ e:expr) #'(private/->maybe e)]))

(define-syntax (maybe/do stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(call/cc
        (λ (k)
          (define (f . xs)
            (define value
              (match xs
                [(list) nothing]
                [(list a) (if (maybe? a) a (Just a))]
                [_ (raise-syntax-error #f "break最多接受一个值！")]))
            (k value))

          (syntax-parameterize
              ([break (make-rename-transformer #'f)])
            (maybe-do-notation body ...))))]))

(module+ test
  (require rackunit)

  (test-case "<maybe>: maybe/do"
    (check-equal? nothing
                  (maybe/do
                   (a <- nothing)
                   (b <- (Just 20))
                   (Just (+ a b))))
    (check-equal? (Just 10)
                  (maybe/do
                   (a <- (Just 1))
                   (b <- (Just 9))
                   (Just (+ b a))))
    (check-equal? (Just 3)
                  (maybe/do
                   (a <- 1)
                   (b <- 2)
                   (+ a b)))
    (check-equal? nothing
                  (maybe/do
                   (a <- 1)
                   (b <- #f)
                   (+ a b)))

    (check-equal? (Just 20)
                  (maybe/do
                   (a <- (Just 5))
                   (let [b a] [c (Just 10)])
                   (c <- c)
                   (Just (+ a b c))))

    (check-equal? (Just 10)
                  (maybe/do
                   (a <- (Just 5))
                   (! nothing)
                   (Just (+ a a))))

    (check-equal? (Just 20)
                  (maybe/do
                   (a <- (Just 10))
                   (when (= a 10)
                     (break (Just 20)))
                   a))))
