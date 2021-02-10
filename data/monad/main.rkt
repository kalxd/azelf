#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)

         racket/contract)

(export-from "./monad.rkt")

(provide >>=
         =<<)

(define >>= bind)

(define/contract (=<< f ma)
  (-> (-> any/c Monad?)
      Monad?
      Monad?)
  (bind ma f))

(define/contract ((<=< f g) x)
  (-> (-> any/c Monad?)
      (-> any/c Monad?)
      (-> any/c Monad?))
  (bind (g x) f))
