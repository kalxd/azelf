#lang racket/base

(require racket/contract
         racket/match

         "../syntax/curry.rkt"
         "./error.rkt"

         (only-in racket/list
                  empty?)

         (for-syntax racket/base
                     racket/syntax

                     "../internal/macro-util.rkt"))

(provide Nothing
         Just
         Nothing?
         Just?
         nothing
         Maybe/c

         maybe-map
         maybe-then
         maybe->
         ->maybe
         maybe-unwrap
         maybe-catch
         maybe-wrap)

(struct Nothing []
  #:transparent)

(struct Just [value]
  #:transparent)

(define/contract (Maybe/c a)
  (-> any/c contract?)
  (or/c Nothing? (struct/c Just a)))

(define/contract nothing
  (Maybe/c any/c)
  (Nothing))

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

(define-syntax (maybe-wrap stx)
  (syntax-case stx ()
    [(_ f n)
     (let ([ns (gen-n-args #'n)])
       (with-syntax ([name (format-id #f "maybe-wrap:~a" #'f)]
                     [(args ...) ns])
         #'(let ([name (λ (args ...)
                         (->maybe (f args ...)))])
             name)))]))

(module+ test
  (test-case "<maybe>: maybe-wrap"

    (define (testing a b)
      (and (> a b) (- a b)))

    (define f (maybe-wrap testing 2))
    (check-equal? (Just 1) (f 4 3))
    (check-equal? nothing (f 3 4))))
