#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         (only-in racket/function
                  const)
         racket/contract)

(export-from "./functor.rkt")

(provide <$>
         <$
         $>)

(define <$> fmap)

(define/contract (<$ a fa)
  (-> any/c Functor? Functor?)
  (fmap (const a) fa))

(define/contract ($> fa a)
  (-> Functor? any/c Functor?)
  (fmap (const a) fa))

(module+ test
  (require rackunit)
  (test-case "<Functor>:<$"
    (check-equal? (list 1 1 1)
                  (<$ 1 (list 4 5 6))))

  (test-case "<Functor>:$>"
    (check-equal? (list 1 1 1)
                  ($> (list 1 2 3) 1))))
