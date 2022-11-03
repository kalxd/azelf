#lang racket/base

(require (prefix-in base:: racket/base)
         racket/contract
         racket/match)

(require "../internal/curry.rkt"
         "../internal/match.rkt"
         "../internal/pipeline.rkt"
         "../type/json.rkt"
         "../data/maybe.rkt")

(struct Hash [ref]
  #:transparent

  #:property prop:sequence
  (match-lambda
    [(Hash h) h])

  #:methods gen:ToJSON
  [(define/match1 ->json
     [(Hash h) h])])

(define/contract (Hash/c key value)
  (-> any/c any/c contract?)
  (struct/c Hash (hash/c key value)))

(define-syntax-rule (make-hash el ...)
  (Hash (base::hash el ...)))

(define/contract hash-empty
  (Hash/c any/c any/c)
  (Hash (base::hash)))

(define/contract (hash-singleton k a)
  (-> any/c any/c (Hash/c any/c any/c))
  (Hash (base::hash k a)))

(define/curry/contract (hash-insert key value h)
  (-> any/c any/c (Hash/c any/c any/c) (Hash/c any/c any/c))
  (match-define (Hash h-ref) h)
  (->> (base::hash-set h-ref key value)
       Hash))

(define/curry/contract (hash-delete key h)
  (-> any/c (Hash/c any/c any/c) (Hash/c any/c any/c))
  (match-define (Hash h-ref) h)
  (->> (base::hash-remove h-ref key)
       Hash))
