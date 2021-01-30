#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         racket/contract
         racket/match)

(export-from "./maybe.rkt")

(provide maybe
         maybe->
         ->maybe
         maybe/catch)

;; maybe ;;
(define/contract (maybe def f value)
  (-> any/c
      (-> any/c any/c)
      (Maybe/c any/c)
      any/c)
  (match value
    [(Just x) (f x)]
    [_ def]))

(module+ test
  (require rackunit)
  (define just (Just 1))

  (test-case "<Maybe>:maybe"
    (check-equal? 0 (maybe 0 add1 nothing))
    (check-equal? 2 (maybe 0 add1 (Just 1)))
    (check-equal? "1" (maybe 0 number->string just))))
;; ;;

;; maybe-> ;;
(define/contract (maybe-> x value)
  (->i ([x any/c]
        [f (Maybe/c any/c)])
       [result (x f) (or/c x any/c)])
  (match value
    [(Just a) a]
    [_ x]))

(module+ test
  (test-case "<Maybe>:maybe->"
    (check-equal? 1 (maybe-> 1 nothing))
    (check-equal? 1 (maybe-> 2 (Just 1)))))
;; ;;

;; ->maybe ;;
(define/contract (->maybe x)
  (->i ([x any/c])
       [result (x) (Maybe/c x)])
  (if x
      (Just x)
      nothing))

(module+ test
  (test-case "<Maybe>:->maybe"
    (check-equal? (Just 1) (->maybe 1))
    (check-equal? nothing (->maybe #f))))
;; ;;

(define/contract (maybe/catch action)
  (-> (-> any/c)
      (Maybe/c any/c))
  (with-handlers ([exn:fail?
                   (λ (e) nothing)])
    (let ([x (action)])
      (Just x))))

(module+ test
  (check-pred Nothing?
              (maybe/catch
               (λ ()
                 (/ 1 0))))
  (check-pred Just?
              (maybe/catch
               (λ ()
                 (/ 0 1)))))
