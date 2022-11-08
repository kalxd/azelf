#lang racket/base

(require racket/generic
         racket/contract
         (only-in racket/function
                  identity)
         (prefix-in base:: racket/base)
         json)

(provide gen:ToJSON
         ToJSON?
         ->json
         json->string
         json->byte)

(define (json-number? x)
  (or (exact-integer? x)
      (and (inexact-real? x)
           (rational? x))))

(define (json-nil? x)
  (eq? (json-null) x))

(define-generics ToJSON
  (->json ToJSON)

  #:defaults ([string? (define ->json identity)]
              [boolean? (define ->json identity)]
              [json-number? (define ->json identity)]
              [json-nil? (define ->json identity)]
              [list? (define/generic self/to-json ->json)
                     (define/contract (->json xs)
                       (-> (listof ToJSON?) (listof jsexpr?))
                       (map self/to-json xs))]
              [hash? (define/generic self/to-json ->json)
                     (define/contract (->json o)
                       (-> (hash/c symbol? ToJSON?) (hash/c symbol? jsexpr?))
                       (for/hash ([(k v) o])
                         (values k (self/to-json v))))]))

(define/contract json->string
  (-> ToJSON? string?)
  (compose jsexpr->string
           ->json))

(define/contract json->byte
  (-> ToJSON? bytes?)
  (compose jsexpr->bytes
           ->json))
