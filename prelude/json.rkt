#lang racket/base

(require "../type/json.rkt"
         "../type/array.rkt"
         "../type/map.rkt"
         "../type/maybe.rkt"
         "../type/functor.rkt"
         "../internal/pipeline.rkt")

(require racket/contract
         json)

(provide json/->primitive
         json/read)

(define/contract (json/->primitive j)
  (-> jsexpr? any/c)
  (cond [(list? j)
         (->> (map json/->primitive j)
              list->array)]
        [(hash? j)
         (->> (map json/->primitive j)
              hash->map)]
        [(equal? j (json-null)) nothing]
        [else j]))

(define/contract json/read
  (-> input-port? any/c)
  (>-> read-json json/->primitive))
