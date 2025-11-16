#lang typed/racket/base

(require typed/json
         (only-in "./option.rkt"
                  option/unwrap-exn)
         "../syntax/pipe.rkt"
         "../syntax/it.rkt")

(provide (all-from-out typed/json)
         exn:fail:json?
         jstring?
         jstring!
         jboolean?
         jboolean!)

(struct exn:fail:json exn:fail ()
  #:type-name JsonError)

(define-type (Nullable a)
  (U 'null a))

(define-type JObject
  (Immutable-HashTable Symbol JSExpr))

(define-syntax-rule (json-err msg)
  (exn:fail:json msg (current-continuation-marks)))

(: nullable->option
   (All (a)
        (-> (Nullable a) (Option a))))
(define (nullable->option ma)
  (cond
    [(eq? 'null ma) #f]
    [else ma]))

(define-syntax-rule (if-let [body result])
  (cond
    [body result]
    [else 'null]))

(: nullable/unwrap-err
   (All (a) (-> (Nullable a) String a)))
(define (nullable/unwrap-err ma msg)
  (cond
    [(eq? 'null ma)
     (raise (exn:fail:json msg (current-continuation-marks)))]
    [else ma]))

(: jstring? (-> JSExpr (Nullable String)))
(define (jstring? value)
  (if-let ([string? value] value)))

(: jstring! (-> JSExpr String))
(define (jstring! json)
  (->> (jstring? json)
       (let ([msg (format "~a无法转化成string" json)])
         (nullable/unwrap-err it msg))))

(: jboolean? (-> JSExpr (Nullable Boolean)))
(define (jboolean? value)
  (if-let ([boolean? value] value)))

(: jboolean! (-> JSExpr Boolean))
(define (jboolean! value)
  (->> (jboolean? value)
       (let ([msg (format "~a无法转化为boolean" value)])
         (nullable/unwrap-err it msg))))

(: jobject? (-> JSExpr (Nullable JObject)))
(define (jobject? value)
  (if-let
   ([hash? value]
    (cast value JObject))))

(: jobject! (-> JSExpr JObject))
(define (jobject! value)
  (->> (jobject? value)
       (let ([msg (format "~a无法转化为object" value)])
         (nullable/unwrap-err it msg))))

(: object
   (All (a)
        (-> (-> JObject a)
            (-> JSExpr a))))
(define ((object f) value)
  (->> (jobject! value)
       f))

(module+ test
  (define value : JSExpr 1)

  (struct user ([name : String])
    #:transparent))
