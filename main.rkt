#lang racket/base

(require (only-in "./syntax/mod.rkt"
                  export-from)

         (only-in "./internal/it.rkt"
                  it))

(provide it)

(export-from racket/base
             racket/contract
             racket/match
             "./syntax/mod.rkt"
             "./syntax/pipeline.rkt"
             "./syntax/spread.rkt"
             "./syntax/curry.rkt"
             "./syntax/nil.rkt"

             "./data/json.rkt"
             "./data/function.rkt"
             "./data/list.rkt"
             "./data/maybe/maybe.rkt")
