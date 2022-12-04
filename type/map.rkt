#lang racket/base

(require racket/match
         racket/contract
         racket/generic
         racket/struct)

(require "./json.rkt"
         "./eq.rkt"
         "./ord.rkt"
         "./functor.rkt"
         "../internal/match.rkt")

(provide Map?
         list->map
         ->map
         map->list)

(struct InnerMap [ref]
  #:transparent

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'Map)
      (match-lambda
        [(InnerMap h) (hash->list h)])))]

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match1 ->json
     [(InnerMap h) (self/->json h)])]

  #:methods gen:Eq
  [(define/generic self/eq:= eq:=)
   (define/match (eq:= a b)
     [((InnerMap ha) (InnerMap hb))
      (self/eq:= ha hb)])]

  #:methods gen:Ord
  [(define/generic self/ord:compare ord:compare)
   (define/match (ord:compare a b)
     [((InnerMap ha) (InnerMap hb))
      (self/ord:compare ha hb)])]

  #:methods gen:Functor
  [(define/generic self/functor:map functor:map)
   (define/match (functor:map f ma)
     [(_ (InnerMap ha))
      (InnerMap (self/functor:map f ha))])]

  #:property prop:sequence
  (match-lambda
    [(InnerMap h) h]))

(define Map? InnerMap?)

(define/contract (list->map xs)
  (-> (listof (cons/c any/c any/c))
      Map?)
  (InnerMap (make-hash xs)))

(define (->map . xs)
  (InnerMap (apply hash xs)))

(define/match1/contract map->list
  (-> Map? (listof (cons/c any/c any/c)))
  [(InnerMap h) (hash->list h)])