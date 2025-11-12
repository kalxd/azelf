#lang typed/racket/base

(require "./syntax/pipe.rkt"
         (only-in "./syntax/it.rkt" it)
         "./primitive/option.rkt")

(provide it
         (all-from-out typed/racket/base)
         (all-from-out "./syntax/pipe.rkt"
                       "./primitive/option.rkt"))

#|

#lang racket/base

(require (only-in "./internal/mod.rkt"
                  export-from)
         (only-in "./internal/keyword.rkt"
                  it
                  break))

(provide it
         break

         (except-out (all-from-out racket/base)
                     #%app)
         (rename-out [*app* #%app]))

(export-from racket/contract
             racket/match
             "./internal/mod.rkt"
             "./internal/pipeline.rkt"
             "./internal/curry.rkt"
             "./internal/match.rkt"
             "./internal/function.rkt"

             "./type/show.rkt"
             "./type/eq.rkt"
             "./type/ord.rkt"
             "./type/json.rkt"
             "./type/functor.rkt"
             "./type/applicative.rkt"
             "./type/monad.rkt"
             "./type/maybe.rkt"
             "./type/array.rkt"
             "./type/map.rkt"

             "./prelude/maybe.rkt"
             "./prelude/array.rkt"
             "./prelude/map.rkt"
             "./prelude/json.rkt")
|#
