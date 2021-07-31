#lang racket/base

(require (only-in "./syntax/mod.rkt"
                  export-from))

(export-from racket/base
             racket/contract
             racket/match
             "./syntax/pipeline.rkt"
             "./syntax/spread.rkt"
             "./syntax/function.rkt")
#|
(export-from racket/contract
             racket/match
             "./data/eq/main.rkt"
             "./data/ord/main.rkt"
             "./data/semigroup/main.rkt"
             "./data/monoid/main.rkt"
             "./data/functor/main.rkt"
             "./data/applicative/main.rkt"
             "./data/monad/main.rkt"
             "./data/foldable/main.rkt"
             "./data/json/main.rkt"

             "./data/maybe/main.rkt"
             "./data/either/main.rkt")

(require (except-in racket/base
                    =
                    <
                    >
                    <=
                    >=
                    max
                    min
                    map
                    foldl))

(provide (all-from-out racket/base))
|#
