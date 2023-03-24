#lang racket/base

(require "../type/json.rkt"
         "../type/array.rkt"
         "../type/map.rkt"
         "../type/maybe.rkt"
         "../type/functor.rkt"
         "../internal/pipeline.rkt")

(require racket/contract
         json)

(provide jsexpr->primitive)

(define/contract (jsexpr->primitive j)
  (-> jsexpr? any/c)
  (cond [(list? j)
         (->> (map jsexpr->primitive j)
              list->array)]
        [(hash? j)
         (->> (map jsexpr->primitive j)
              hash->map)]
        [(equal? j (json-null)) nothing]
        [else j]))
