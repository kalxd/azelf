#lang racket/base

(require racket/contract
         racket/match)

(require "../type/maybe.rkt"
         "../type/monad.rkt"

         "../internal/curry.rkt"
         "../internal/match.rkt")

(provide (all-defined-out))

(define/curry/contract (maybe-filter f ma)
  (-> (-> any/c boolean?)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (define (g a)
    (if (f a)
        (Just a)
        nothing))
  (=<< g ma))

(define/curry/contract (maybe-and ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) _) nothing]
    [(_ (Nothing)) nothing]
    [(_ _) mb]))

(define/curry/contract (maybe-or ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) _) nothing]
    [(_ (Nothing)) nothing]
    [(_ _) ma]))

(define/curry/contract (maybe-alt ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) (Just _)) mb]
    [((Just _) _) ma]
    [(_ _) nothing]))

(define/curry/contract (maybe-else a f ma)
  (-> any/c
      (-> any/c any/c)
      (Maybe/c any/c)
      any/c)
  (match ma
    [(Just a) (f a)]
    [_ a]))

(define-syntax-rule (maybe-catch action)
  (with-handlers
    ([exn:fail? (λ (_) nothing)])
    (Just action)))
