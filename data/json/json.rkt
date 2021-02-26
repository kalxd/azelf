#lang racket/base

(require racket/generic
         json
         "../../internal/macro.rkt")

(provide gen:ToJSON
         ToJSON?
         ToJSON/c
         ->json)

(define-generics ToJSON
  (->json ToJSON)

  #:defaults
  ([jsexpr? (macro/id ->json)]))
