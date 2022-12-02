#lang racket/base

(require racket/generic
         racket/contract
         racket/match
         (only-in json
                  json-null))

(require "../internal/curry.rkt"
         "../internal/error.rkt"
         "./eq.rkt"
         "./ord.rkt"
         "./functor.rkt"
         "./applicative.rkt"
         "./monad.rkt"
         "./json.rkt")

(provide Nothing
         Nothing?
         nothing
         Just
         Just?
         Maybe/c
         maybe?
         maybe->
         ->maybe
         maybe-unwrap)

(struct Nothing []
  #:transparent

  #:methods gen:Eq
  [(define/match (eq:= a b)
     [((Nothing) (Nothing)) #t]
     [(_ _) #f])]

  #:methods gen:Ord
  [(define/match (ord:compare a b)
     [((Nothing) (Nothing)) 'eq]
     [((Nothing) _) 'lt])]

  #:methods gen:ToJSON
  [(define (->json self)
     (json-null))]

  #:methods gen:Functor
  [(define (functor:map f a)
     a)]

  #:methods gen:Applicative
  [(define (applicative:ap f ma)
     ma)]

  #:methods gen:Monad
  [(define (monad:bind ma f)
     ma)]

  #:property prop:sequence
  (λ (self)
    (in-list '())))

(struct Just [value]
  #:transparent

  #:methods gen:Eq
  [(define/generic self/= eq:=)
   (define/match (eq:= a b)
     [((Just a) (Just b)) (self/= a b)]
     [(_ _) #f])]

  #:methods gen:Ord
  [(define/generic self/compare ord:compare)
   (define/match (ord:compare a b)
     [((Just a) (Just b)) (self/compare a b)]
     [(_ _) 'gt])]

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match (->json self)
     [((Just a)) (self/->json a)])]

  #:methods gen:Functor
  [(define/match (functor:map f a)
     [(_ (Just a)) (Just (f a))])]

  #:methods gen:Applicative
  [(define/match (applicative:ap f ma)
     [((Just f) (Just a)) (Just (f a))]
     [((Nothing) _) nothing])]

  #:methods gen:Monad
  [(define/match (monad:bind ma f)
     [((Just a) _) (f a)])]

  #:property prop:sequence
  (match-lambda
    [(Just a) a]))

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

(define/contract (->maybe a)
  (-> any/c (Maybe/c any/c))
  (if a (Just a) nothing))

(define/curry (maybe-> b a)
  (match a
    [(Just a) a]
    [_ b]))

(define/match (maybe-unwrap a)
  [((Just a)) a]
  [(_) (raise-unwrap-error "maybe-unwrap: 试图解包nothing！")])
