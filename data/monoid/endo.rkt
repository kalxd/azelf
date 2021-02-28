#lang racket/base

(require racket/contract
         racket/generic
         (only-in racket/function
                  identity)
         "../semigroup/semigroup.rkt"
         "./monoid.rkt")

(provide Endo
         unwrap/endo)

; 自函子，同类型的输入、输出。
; endo :: (-> a a)
(struct Endo [endo]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (let* ([a (Endo-endo self)]
            [msg (format "#<Endo ~s>" a)])
       (display msg port)))]

  #:methods gen:Semigroup
  [(define/generic Semigroup/mappend mappend)
   (define (mappend self other)
     (let ([a (Endo-endo self)]
           [b (Endo-endo other)])
       (Endo (Semigroup/mappend a b))))]

  #:methods gen:Monoid
  [(define (mempty _)
    (Endo identity))])

(define/contract unwrap/endo
  (-> Endo? any/c)
  Endo-endo)
