#lang typed/racket/base

(require typed/json
         "../syntax/pipe.rkt"
         "../syntax/it.rkt")

(provide (all-from-out typed/json)
         exn:fail:json?
         jstring?
         jstring!
         jboolean?
         jboolean!
         JObject
         nullable->option
         nullable/unwrap-or
         nullable/unwrap
         jobject?
         jobject!
         jarray?
         jarray!
         jfield)

(struct exn:fail:json exn:fail ()
  #:type-name JsonError)

(define-type (Nullable a)
  (U 'null a))

(define-type JObject
  (Immutable-HashTable Symbol JSExpr))

(define-type JArray
  (Listof JSExpr))

(define-syntax-rule (raise-json-error msg)
  (raise (exn:fail:json msg (current-continuation-marks))))

(: nullable->option
   (All (a)
        (-> (Nullable a) (Option a))))
(define (nullable->option ma)
  (cond
    [(eq? 'null ma) #f]
    [else ma]))

(: nullable/unwrap-or
   (All (a)
        (-> (Nullable a)
            a
            a)))
(define (nullable/unwrap-or ma a)
  (if (eq? 'null ma)
      a
      ma))

(: nullable/unwrap
   (All (a) (-> (Nullable a) a)))
(define (nullable/unwrap ma)
  (when (eq? 'null ma)
    (raise-user-error "尝试从null中转成为普通数据！"))
  ma)

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
