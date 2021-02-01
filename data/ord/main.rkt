#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract)

(export-from "./ord.rkt")

(provide :<
         :>
         :<=
         :>=
         :max
         :min
         clamp
         between)

(define/contract (:< a b)
  (-> Ord? Ord? boolean?)
  (LT? (compare a b)))

(module+ test
  (require rackunit)
  (test-case "<Ord>::<"
    (check-true (:< 1 2))
    (check-true (:< #\A #\B))))

(define/contract (:> a b)
  (-> Ord? Ord? boolean?)
  (GT? (compare a b)))

(module+ test
  (test-case "<Ord>::>"
    (check-true (:> 2 1))
    (check-true (:> #\B #\A))))

(define/contract (:<= a b)
  (-> Ord? Ord? boolean?)
  (define r (compare a b))
  (or (LT? r) (EQ? r)))

(module+ test
  (test-case "<Ord>:<="
    (check-true (:<= 1 1))
    (check-true (:<= 1 2))
    (check-false (:<= 2 1))))

(define/contract (:>= a b)
  (-> Ord? Ord? boolean?)
  (define r (compare a b))
  (or (GT? r) (EQ? r)))

(module+ test
  (test-case "<Ord>:>="
    (check-true (:>= 2 2))
    (check-true (:>= 2 1))
    (check-false (:>= 1 2))))

(define/contract (:max a b)
  (->i ([a Ord?]
        [b Ord?])
       [result (a b) (or/c a b)])
  (if (:< a b) b a))

(module+ test
  (test-case "<Ord>::max"
    (check-equal? 10 (:max 10 1))
    (check-equal? #\Z (:max #\A #\Z))))

(define/contract (:min a b)
  (->i ([a Ord?]
        [b Ord?])
       [result (a b) (or/c a b)])
  (if (:< a b) a b))

(module+ test
  (test-case "<Ord>::min"
    (check-equal? 1 (:min 10 1))
    (check-equal? #\A (:min #\A #\Z))))

(define/contract (clamp low high value)
  (->i ([low Ord?]
        [high Ord?]
        [value Ord?])
       [result (low high value) (or/c low high value)])
  (:min (:max low value)
        high))

(module+ test
  (test-case "clamp")
  (check-equal? 5 (clamp 1 10 5))
  (check-equal? 10 (clamp 1 10 15))
  (check-equal? 1 (clamp 1 10 0)))

(define/contract (between a b value)
  (-> Ord? Ord? Ord? boolean?)
  (and (:<= a value)
       (:<= value b)))

(module+ test
  (test-case "<Ord>:between"
    (check-true (between 0 10 5))
    (check-true (between 0 10 0))
    (check-true (between 0 10 10))
    (check-false (between 0 10 -1))))
