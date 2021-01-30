#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract)

(export-from "./either.rkt")

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
