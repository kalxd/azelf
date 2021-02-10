#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         (only-in "../../base/pipeline.rkt"
                  ->>
                  it)
         "../functor/functor.rkt"

         racket/contract
         (only-in racket/function
                  curry
                  const))

(export-from "./applicative.rkt")

(provide <*>
         *>
         <*
         lift2
         lift3
         lift4
         lift5)

(define <*> ap)

(define/contract (*> fa fb)
  (-> Applicative? Applicative? Applicative?)
  (<*> (map const fb) fa))

(define/contract (<* fa fb)
  (-> Applicative? Applicative? Applicative?)
  (<*> (map const fa) fb))

(define/contract (lift2 f a b)
  (-> (-> any/c any/c any/c)
      Applicative?
      Applicative?
      Applicative?)
  (define g (curry f))
  (<*> (map g a) b))

(define/contract (lift3 f a b c)
  (-> (-> any/c any/c any/c any/c)
      Applicative?
      Applicative?
      Applicative?
      Applicative?)
  (define g (curry f))
  (->> (map g a)
       (<*> it b)
       (<*> it c)))

(define/contract (lift4 f a b c d)
  (-> (-> any/c any/c any/c any/c any/c)
      Applicative?
      Applicative?
      Applicative?
      Applicative?
      Applicative?)
  (define g (curry f))
  (->> (map f a)
       (<*> it b)
       (<*> it c)
       (<*> it d)))

(define/contract (lift5 f a b c d e)
  (-> (-> any/c any/c any/c any/c any/c any/c)
      Applicative?
      Applicative?
      Applicative?
      Applicative?
      Applicative?
      Applicative?)
  (define g (curry f))
  (->> (map f a)
       (<*> it b)
       (<*> it c)
       (<*> it d)
       (<*> it e)))
