#lang typed/racket/base

(require typed/json
         "../internal/nullable.rkt")

(provide exn:fail:json?
         jstring?
         jstring!
         jboolean?
         jboolean!)
#|
         jinteger?
         jinteger?
         jfloat?
         jfloat!
         JObject
         JArray
         jobject?
         jobject!
         jarray?
         jarray!
         jfield)
         |#

(struct exn:fail:json exn:fail ()
  #:type-name JsonError)

(define-type JObject
  (Immutable-HashTable Symbol JSExpr))

(define-type JArray
  (Listof JSExpr))

(define-syntax-rule (raise-json-error msg)
  (raise (exn:fail:json msg (current-continuation-marks))))

(define-syntax-rule (require-json-type predicate error-msg)
  (match-nullable predicate
                  [a a]
                  (raise-json-error error-msg)))

(: predicate-simple-type
   (All (a)
        (-> (JSExpr -> Boolean : a)
            (-> JSExpr (Nullable a)))))
(define ((predicate-simple-type predicat) json)
  (if (predicat json)
      (some json)
      nil))

(: jstring? (-> JSExpr (Nullable String)))
(define jstring? (predicate-simple-type string?))

(: jstring! (-> JSExpr String))
(define (jstring! value)
  (require-json-type (jstring? value)
                     (format "~a无法转化成string！" value)))


(: jboolean? (-> JSExpr (Nullable Boolean)))
(define jboolean? (predicate-simple-type boolean?))

(: jboolean! (-> JSExpr Boolean))
(define (jboolean! value)
  (require-json-type (jboolean? value)
                     (format "~a无法转化为boolean" value)))

#|
(: jinteger? (-> JSExpr (Nullable Integer)))
(define (jinteger? value)
  (if-let
   ([integer? value]
    (cast value Integer))))

(: jinteger! (-> JSExpr Integer))
(define (jinteger! value)
  (->> (jinteger? value)
       (let ([msg (format "~a无法转成为integer！" value)])
         (nullable/unwrap-err it msg))))

(: jfloat? (-> JSExpr (Nullable Inexact-Real)))
(define (jfloat? value)
  (if-let
   [(inexact-real? value) value]))

(: jfloat! (-> JSExpr Inexact-Real))
(define (jfloat! value)
  (->> (jfloat? value)
       (let ([msg (format "~a无法转化为float！" value)])
         (nullable/unwrap-err it msg))))

(: jobject?
   (All (a)
        (-> (-> JObject (Nullable a))
            (-> JSExpr (Nullable a)))))
(define ((jobject? f) value)
  (if-let
   ([hash? value]
    (->> (cast value JObject)
         f))))

(: jobject!
   (All (a)
        (-> (-> JObject a)
            (-> JSExpr a))))
(define ((jobject! f) value)
  (cond
    [(eq? 'null value)
     (let ([msg (format "~a无法转化为object！" value)])
       (raise-json-error msg))]
    [else (->> (cast value JObject)
               f)]))

(: jfield
   (All (a)
        (-> JObject
            Symbol
            (-> JSExpr a)
            a)))
(define (jfield obj name trans)
  (cond
    [(hash-has-key? obj name)
     (->> (hash-ref obj name)
          (with-handlers ([exn:fail:json?
                           (λ ([e : JsonError])
                             (let ([msg (format "~a字段解析出错：~a" name (exn-message e))])
                               (raise-json-error msg)))])
            (trans it)))]
    [else (let ([msg (format "~a没有~a字段！" obj name)])
            (raise-json-error msg))]))

(: jarray:tranverse
   (All (a b)
        (-> (Listof a)
            (-> a (Nullable b))
            (Nullable (Listof b)))))
(define (jarray:tranverse xs f)
  (define-values (acc is-break)
    (for/fold ([acc : (Listof b) (list)]
               [is-break : Boolean #f])
              ([x xs]
               #:break is-break)
      (let ([v (f x)])
        (if (eq? v 'null)
            (values acc #t)
            (values (append acc (list v)) #f)))))
  (if is-break 'null acc))

(: jarray?
   (All (a)
        (-> (-> JSExpr (Nullable a))
            (-> JSExpr (Nullable (Listof a))))))
(define ((jarray? f) value)
  (if-let
   [(list? value)
    (->> (cast value JArray)
         (jarray:tranverse it f))]))

(: jarray!
   (All (a)
        (-> (-> JSExpr a)
            (-> JSExpr (Listof a)))))
(define ((jarray! f) value)
  (cond
    [(list? value)
     (->> (cast value JArray)
          (map f it))]
    [else
     (let ([msg (format "~a无法转化成jarray！" value)])
       (raise-json-error msg))]))
|#
