#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from))

(export-from "./semigroup.rkt")

(provide <>)

(define <> mappend)
