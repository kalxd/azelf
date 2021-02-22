#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract
         racket/match

         "../maybe/maybe.rkt")

(export-from "./either.rkt")

(provide either/catch
         either->maybe
         maybe->either)

(define/contract (either/catch action)
  (-> (-> any/c) (Either/c exn:fail? any/c))
  (with-handlers ([exn:fail? (λ (e) (Left e))])
    (let ([x (action)])
      (Right x))))

(module+ test
  (require rackunit)
  (test-case "<Either>:either/catch-do"
    (check-pred Left?
                (either/catch
                 (λ ()
                   (/ 1 0))))
    (check-pred Right?
                (either/catch
                 (λ ()
                   (/ 1 1))))))

(define/contract (either->maybe x)
  (-> (Either/c any/c any/c) (Maybe/c any/c))
  (match x
    [(Right x) (Just x)]
    [else nothing]))

(module+ test
  (test-case "<Either>:either->maybe"
    (check-equal? (Just 1) (either->maybe (Right 1)))
    (check-equal? nothing (either->maybe (Left 1)))))

(define/contract (maybe->either e ma)
  (-> any/c (Maybe/c any/c) (Either/c any/c any/c))
  (match ma
    [(Just x) (Right x)]
    [_ (Left e)]))

(module+ test
  (test-case "<Either>:maybe->either"
    (check-equal? (Right 1) (maybe->either 2 (Just 1)))
    (check-equal? (Left 2) (maybe->either 2 nothing))))
