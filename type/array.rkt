#lang racket/base

(require racket/contract
         racket/generic
         racket/match
         racket/struct

         (prefix-in base:: racket/base)
         (for-syntax racket/base))

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
         array
         list->array
         array->list)

(struct InnerArray [ref]
  #:transparent

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'Array)
      (match-lambda
        [(InnerArray xs) xs])))]

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match1 ->json
     [(InnerArray xs) (self/->json xs)])]

  #:methods gen:Eq
  [(define/generic self/= eq:=)
   (define/match (eq:= a b)
     [((InnerArray xs) (InnerArray ys)) (self/= xs ys)])]

  #:methods gen:Ord
  [(define/generic self/compare ord:compare)
   (define/match (ord:compare a b)
     [((InnerArray xs) (InnerArray ys)) (self/compare xs ys)])]

  #:methods gen:Functor
  [(define/generic self/map functor:map)
   (define/match (functor:map f b)
     [(_ (InnerArray ys)) (InnerArray (self/map f ys))])]

  #:methods gen:Applicative
  [(define/generic self/ap applicative:ap)
   (define/match (applicative:ap a b)
     [((InnerArray xs) (InnerArray ys)) (InnerArray (self/ap xs ys))])]

  #:methods gen:Monad
  [(define/generic self/bind monad:bind)
   (define/match (monad:bind a f)
     [((InnerArray xs) _)
      (base::foldl (λ (x acc)
                     (match-define (InnerArray acc-) acc)
                     (match-define (InnerArray ys) (f x))
                     (InnerArray (base::append acc- ys)))
                   (InnerArray (list))
                   xs)])]

  #:property prop:sequence
  (match-lambda
    [(InnerArray xs) xs]))

(define Array? InnerArray?)

(define (Array/c a)
  (struct/c InnerArray (listof a)))

(define-match-expander Array
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(InnerArray (list pat ...))])))

(define list->array InnerArray)

(define (array . xs)
  (InnerArray (apply list xs)))

(define/match1 array->list
  [(InnerArray xs) xs])
