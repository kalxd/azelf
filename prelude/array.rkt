#lang racket/base

(require racket/contract
         racket/generic
         racket/match

         (prefix-in base:: racket/base))

(require "../internal/match.rkt")

(require "./json.rkt"
         "./eq.rkt"
         "./ord.rkt"
         "./functor.rkt"
         "./applicative.rkt"
         "./monad.rkt")

(provide Array
         Array?
         Array/c

         list->array
         array->list)

(struct Array [ref]
  #:transparent

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match1 ->json
     [(Array xs) (self/->json xs)])]

  #:methods gen:Eq
  [(define/generic self/= eq:=)
   (define/match (eq:= a b)
     [((Array xs) (Array ys)) (self/= xs ys)])]

  #:methods gen:Ord
  [(define/generic self/compare ord:compare)
   (define/match (ord:compare a b)
     [((Array xs) (Array ys)) (self/compare xs ys)])]

  #:methods gen:Functor
  [(define/generic self/map functor:map)
   (define/match (functor:map f b)
     [(_ (Array ys)) (Array (self/map f ys))])]

  #:methods gen:Applicative
  [(define/generic self/ap applicative:ap)
   (define/match (applicative:ap a b)
     [((Array xs) (Array ys)) (Array (self/ap xs ys))])]

  #:methods gen:Monad
  [(define/generic self/bind monad:bind)
   (define/match (monad:bind a f)
     [((Array xs) _)
      (base::foldl (λ (x acc)
                     (match-define (Array acc-) acc)
                     (match-define (Array ys) (f x))
                     (Array (append acc- ys)))
                   (Array (list))
                   xs)])])

(define (Array/c a)
  (struct/c Array a))

(define/contract list->array
  (-> list? Array?)
  Array)

(define/match1/contract array->list
  (-> Array? list?)
  [(Array xs) xs])

(define/contract singleton
  (-> any/c (Array/c any/c))
  (compose Array
           list))

(define/contract mempty
  (Array/c any/c)
  (Array (list)))
