#lang typed/racket/base

(require typed/json
         "../internal/nullable.rkt")

(provide exn:fail:json?
         JObject
         JArray
         jstring?
         jstring!
         jboolean?
         jboolean!
         jinteger?
         jinteger?
         jfloat?
         jfloat!
         jobject?
         jobject!
         jarray?
         jarray!)

(struct exn:fail:json exn:fail ()
  #:type-name JsonError)

(define-type JObject
  (HashTable Symbol JSExpr))

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


(: jinteger? (-> JSExpr (Nullable Integer)))
(define jinteger? (predicate-simple-type exact-integer?))

(: jinteger! (-> JSExpr Integer))
(define (jinteger! value)
  (require-json-type (jinteger? value)
(format "~a无法转成为integer！" value)))

(: jfloat? (-> JSExpr (Nullable Inexact-Real)))
(define jfloat? (predicate-simple-type inexact-real?))

(: jfloat! (-> JSExpr Inexact-Real))
(define (jfloat! value)
  (require-json-type (jfloat? value)
                     (format "~a无法转化为float！" value)))


(: jobject? (-> JSExpr (Nullable JObject)))
(define (jobject? value)
  (if (hash? value)
      (some (cast value JObject))
      nil))

(: jobject! (-> JSExpr JObject))
(define (jobject! value)
  (require-json-type (jobject? value)
                     (format "~a无法转化成object！" value)))

(: jarray? (-> JSExpr (Nullable JArray)))
(define (jarray? value)
  (if (list? value)
      (some (cast value JArray))
      nil))

(: jarray! (-> JSExpr JArray))
(define (jarray! value)
  (require-json-type (jarray? value)
                     (format "~a无法转化成array！" value)))
