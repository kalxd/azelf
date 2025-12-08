#lang typed/racket/base

(require racket/match)

(provide Nullable
         nullable/map)

(struct nullable/nil ()
  #:type-name Nullable/Nil)

(struct (A) nullable/some ([value : A])
  #:type-name Nullable/Some
  #:transparent)

(define-type (Nullable A)
  (U Nullable/Nil (Nullable/Some A)))

(: nullable/map
   (All (A B)
        (-> (Nullable A)
            (-> A B)
            (Nullable B))))
(define (nullable/map ma f)
  (cond
    [(nullable/nil? ma) ma]
    [else
     (match-let ([(nullable/some mb) ma])
       (nullable/some (f mb)))]))
