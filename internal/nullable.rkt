#lang typed/racket/base

(require racket/match)

(provide Nullable
         nullable/nil?
         nullable/some?
         match-nullable?
         nullable/map
         nullable/chain)

(struct nullable/nil ()
  #:type-name Nullable/Nil)

(struct (A) nullable/some ([value : A])
  #:type-name Nullable/Some
  #:transparent)

(define-type (Nullable A)
  (U Nullable/Nil (Nullable/Some A)))

(define-syntax-rule (match-nullable? ma [a (body ...)])
  (cond
    [(nullable/nil? ma) ma]
    [else
     (match-let ([(nullable/some a) ma])
       (body ...))]))

(: nullable/map
   (All (A B)
        (-> (Nullable A)
            (-> A B)
            (Nullable B))))
(define (nullable/map ma f)
  (match-nullable?
   ma
   [a (nullable/some (f a))]))

(: nullable/chain
   (All (A B)
        (-> (Nullable A)
            (-> A (Nullable B))
            (Nullable B))))
(define (nullable/chain ma f)
  (match-nullable?
   ma
   [a (f a)]))

(: nullable->option
   (All (A)
        (-> (Nullable A)
            (Option A))))
(define (nullable->option ma)
  (cond
    [(nullable/nil? ma) #f]
    [else
     (match-let ([(nullable/some a) ma])
       a)]))
