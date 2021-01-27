#lang racket/base

(require (only-in "./internal/macro.rkt"
                  export-from))

(export-from racket/base
             racket/contract

             "./function/main.rkt")
