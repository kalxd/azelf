#lang racket/base

(require racket/generic
         (only-in racket/function
                  identity)
         (only-in racket/list
                  flatten)
         json)

(provide (except-out (all-defined-out)
                     json-number?
                     json-nil?))

(define (json-number? x)
  (or (exact-integer? x)
      (and (inexact-real? x)
           (rational? x))))

(define (json-nil? x)
  (cond
    [(symbol? x) (eq? 'null x)]
    [else #f]))

(define-generics ToJSON
  (->json ToJSON)
  (json->string ToJSON)
  (json->byte ToJSON)

  #:defaults ([string? (define ->json identity)]
              [boolean? (define ->json identity)]
              [json-number? (define ->json identity)]
              [json-nil? (define ->json identity)]
              [list? (define/generic self/to-json ->json)
                     (define (->json xs)
                       (map self/to-json xs))]
              [hash? (define/generic self/to-json ->json)
                     (define (->json o)
                       (define (f key value)
                         (cons key (self/to-json value)))
                       (let* ([ps (hash-map o f)]
                              [args (flatten ps)])
                         (apply hash args)))])

  #:fallbacks [(define ->json identity)
               (define json->string
                 (compose jsexpr->string
                          ->json))
               (define json->byte
                 (compose jsexpr->bytes
                          ->json))])
