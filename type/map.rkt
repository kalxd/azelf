#lang racket/base

(require racket/match
         racket/contract
         racket/generic
         racket/struct

         (for-syntax racket/base
                     syntax/parse))

(require "./json.rkt"
         "./eq.rkt"
         "./ord.rkt"
         "./functor.rkt"
         "../internal/match.rkt")

(provide Map?
         Map/c
         Map
         hash->map
         list->map
         hashmap
         map->list
         map->hash
         *app*)

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

(define (Map/c k v)
  (struct/c InnerMap (hash/c k v)))

(define-match-expander Map
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(InnerMap (hash-table pat ...))])))

(define (hashmap . xs)
  (InnerMap (apply hash xs)))

(define hash->map InnerMap)

(define/contract (list->map xs)
  (-> (listof (cons/c any/c any/c))
      Map?)
  (InnerMap (make-hash xs)))

(define/match1/contract map->list
  (-> Map? (listof (cons/c any/c any/c)))
  [(InnerMap h) (hash->list h)])

(define/match1/contract map->hash
  (-> Map? hash?)
  [(InnerMap h) h])

(define-syntax (*app* stx)
  (syntax-parse stx
    ; { key val }字面量
    [(_ x:expr ...)
     #:when (eq? (syntax-property stx 'paren-shape) #\{)
     #'(hashmap x ...)]

    ; 普通语法
    [(_ f x ...)
     #'(#%app f x ...)]))
