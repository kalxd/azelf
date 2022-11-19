#lang racket/base

(require racket/generic
         racket/contract
         (prefix-in base:: racket/base))

(require "./eq.rkt"
         "../internal/match.rkt"
         "../internal/curry.rkt")

(provide Ordering
         gen:Ord
         Ord?
         ord:compare
         compare
         <
         <=
         >
         >=
         min
         max)

(define Ordering
  (or/c 'lt 'gt 'eq))

(define-syntax-rule (generic-compare eq-method gt-method)
  (λ (a b)
    (cond
      [(eq-method a b) 'eq]
      [(gt-method a b) 'gt]
      [else 'lt])))

(define-generics Ord
  (ord:compare Ord a)

  #:defaults
  ([number?
    (define/contract (ord:compare a b)
      (-> number? number? Ordering)
      ((generic-compare base::= base::>) a b))]
   [string?
    (define/contract (ord:compare a b)
      (-> string? string? Ordering)
      ((generic-compare string=? string>?) a b))]
   [char?
    (define/contract (ord:compare a b)
      (-> char? char? Ordering)
      ((generic-compare char=? char>?) a b))]
   [bytes?
    (define/contract (ord:compare a b)
      (-> bytes? bytes? Ordering)
      ((generic-compare bytes=? bytes>?) a b))]
   [pair?
    (define/generic self/compare ord:compare)
    (define/match/contract (ord:compare a b)
      (-> pair? pair? Ordering)
      [((cons a b) (cons x y))
       (let ([ordering (self/compare a x)])
         (if (= 'eq ordering)
             (self/compare b y)
             ordering))])]
   [list?
    (define/generic self/compare ord:compare)
    (define/match/contract (ord:compare xs ys)
      (-> list? list? Ordering)
      [((list) (list)) 'eq]
      [((list) (list _ ...)) 'lt]
      [((list _ ...) (list)) 'gt]
      [((list x xs ...) (list y ys ...))
       (let ([ordering (self/compare x y)])
         (if (= 'eq ordering)
             (self/compare xs ys)
             ordering))])]
   [vector?
    (define/generic self/compare ord:compare)
    (define/match/contract (ord:compare xs ys)
      (-> vector? vector? Ordering)
      [((vector) (vector)) 'eq]
      [((vector) (vector _ ...)) 'lt]
      [((vector _ ...) (vector)) 'gt]
      [((vector x xs ...) (vector y ys ...))
       (let ([ordering (self/compare x y)])
         (if (= 'eq ordering)
             (self/compare xs ys)
             ordering))])]
   [hash?
    (define/generic self/compare ord:compare)
    (define/contract (ord:compare xs ys)
      (-> hash? hash? Ordering)
      (define hxs (hash->list xs))
      (define hys (hash->list ys))
      (self/compare hxs hys))]))

(define compare (curry/n 2 ord:compare))

(define/curry/contract (< a b)
  (-> Ord? Ord? boolean?)
  (= 'lt (compare a b)))

(define/curry/contract (<= a b)
  (-> Ord? Ord? boolean?)
  (/= 'gt (compare a b)))

(define/curry/contract (> a b)
  (-> Ord? Ord? boolean?)
  (= 'gt (compare a b)))

(define/curry/contract (>= a b)
  (-> Ord? Ord? boolean?)
  (/= 'lt (compare a b)))

(define/curry/contract (min a b)
  (->i ([a Ord?]
        [b Ord?])
       [result (a b) (or/c a b)])
  (if (< a b) a b))

(define/curry/contract (max a b)
  (->i ([a Ord?]
        [b Ord?])
       [result (a b) (or/c a b)])
  (if (> a b) a b))
