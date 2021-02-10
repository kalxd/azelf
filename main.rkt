#lang racket/base

(require (only-in "./internal/macro.rkt"
                  export-from))

(export-from racket/contract
             racket/match
             "./base/function.rkt"
             "./base/pipeline.rkt"
             "./base/spread.rkt"

             "./data/eq/main.rkt"
             "./data/ord/main.rkt"
             "./data/semigroup/main.rkt"
             "./data/monoid/main.rkt"
             "./data/functor/main.rkt"
             "./data/applicative/main.rkt"
             "./data/monad/main.rkt"

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
                    map))

(provide (all-from-out racket/base))
