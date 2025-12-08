#lang typed/racket/base

(require "../internal/option.rkt")

(provide Nullable)

(struct nullable/nil ()
  #:type-name Nullable/Nil)

(struct (A) nullable/some ([value : A])
  #:type-name Nullable/Some)

(define-type (Nullable A)
  (U Nullable/Nil (Nullable/Some A)))

(: ->nullable
   (All (A) (-> A
                (Nullable A))))
(define (->nullable value)
  (if value
      (nullable/nil)
      (nullable/some value)))
