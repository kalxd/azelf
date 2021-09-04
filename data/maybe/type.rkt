#lang racket/base

(require racket/generic
         racket/match
         racket/contract
         "../json.rkt")

(provide Nothing
         Just
         Nothing?
         Just?
         maybe?
         nothing
         Maybe/c)

(struct Nothing []
  #:transparent

  #:methods gen:ToJSON
  [(define (->json self)
     'nil)])

(struct Just [value]
  #:transparent

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define (->json self)
     (match self
       [(Just a) (self/->json a)]))])

(define/contract (Maybe/c a)
  (-> any/c contract?)
  (or/c Nothing? (struct/c Just a)))

(define/contract nothing
  (Maybe/c any/c)
  (Nothing))

(define/contract (maybe? a)
  (-> any/c boolean?)
  (or (Just? a)
      (Nothing? a)))
