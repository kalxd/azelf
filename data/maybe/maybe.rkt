#lang racket/base

(require racket/contract
         racket/match
         "./type.rkt"

         "../../syntax/curry.rkt"
         "../../internal/error.rkt"
         "../json.rkt"

         (only-in "../../internal/keyword.rkt"
                  break-wrap
                  break)

         (only-in racket/list
                  empty?)

         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide maybe-map
         maybe-then
         maybe->
         ->maybe
         maybe-unwrap
         maybe-catch

         (all-from-out "./type.rkt"))

(curry/contract (maybe-map f a)
  (-> (-> any/c any/c)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (Just (f a))]
    [else a]))

(module+ test
  (require rackunit)

  (test-case "<maybe>: maybe-map"
    (check-equal? (Just 2) (maybe-map add1 (Just 1)))
    (check-equal? nothing ((maybe-map add1) nothing))))

(define/curry (maybe-> b a)
  (match a
    [(Just a) a]
    [else b]))

(module+ test
  (test-case "<maybe>: maybe-unwrap"
    (check-equal? 1 (maybe-> 2 (Just 1)))
    (check-equal? 2 (maybe-> 2 nothing))))

(curry/contract (maybe-then f a)
  (-> (-> any/c (Maybe/c any/c))
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (f a)]
    [else a]))

(module+ test
  (test-case "<maybe>: maybe-then"
    (define (f x)
      (Just (add1 x)))
    (check-equal? (Just 2) (maybe-then f (Just 1)))
    (check-equal? nothing (maybe-then f nothing))))

(define/contract (->maybe a)
  (-> any/c (Maybe/c any/c))
  (if a (Just a) nothing))

(module+ test
  (test-case "<maybe>: ->maybe"
    (check-equal? (Just 1) (->maybe 1))
    (check-equal? nothing (->maybe #f))))

(define (maybe-unwrap a)
  (match a
    [(Just a) a]
    [_ (raise-unwrap-error "maybe-unwrap: 试图解包nothing！")]))

(module+ test
  (test-case "<maybe>: maybe-unwrap"
    (check-equal? 1 (maybe-unwrap (Just 1)))
    (check-equal? "ab" (maybe-unwrap (Just "ab")))
    (check-exn exn:fail?
               (λ ()
                 (maybe-unwrap nothing)))))

(define-syntax-rule (maybe-catch action)
  (with-handlers
    ([exn:fail? (λ (_) nothing)])
    (Just action)))

(module+ test
  (test-case "<maybe>: maybe-catch"
    (check-equal? (Just 1) (maybe-catch 1))
    (check-equal? (Just 10) (maybe-catch (* 1 10)))
    (check-equal? nothing (maybe-catch (/ 1 0)))))

(define-syntax maybe-do-notation
  (syntax-parser
    ; 绑定语法。
    [(_ (var:id (~and (~literal <-)) e:expr) es:expr ...+)
     #'(maybe-then (λ (var)
                     (maybe/do es ...))
                   e)]
    ; 赋值语法。
    [(_ (let [var:id e:expr] ...+) es:expr ...+)
     #'(let ([var e] ...)
         (maybe/do es ...))]

    ; 副作用。
    [(_ ((~literal !) es:expr ...+) rs:expr ...+)
     #'(begin es ...
              (maybe/do rs ...))]

    ; 多条语句。
    [(_ e1:expr e2:expr ...+)
     #'(let ([a e1])
         (match a
           [(Nothing) a]
           [_ (maybe/do e2 ...)]))]

    ; 最后一条语句
    [(_ e:expr) #'e]))

(define-syntax (maybe/do stx)
  (syntax-case stx (break)
    [(_ body ...)
     #'(break-wrap
        (maybe-do-notation body ...))]))

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
