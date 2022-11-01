#lang racket/base

(require (prefix-in base:: racket/base)
         racket/contract
         racket/match)

(require "../syntax/curry.rkt"
         "../syntax/match.rkt"
         "../type/json.rkt"
         "../data/maybe.rkt")

(provide %:
         Hash/c)

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

(define-syntax-rule (%: el ...)
  (Hash (base::hash el ...)))
