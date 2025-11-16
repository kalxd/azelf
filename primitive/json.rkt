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
         object
         jfield)

(struct exn:fail:json exn:fail ()
  #:type-name JsonError)

(define-type (Nullable a)
  (U 'null a))

(define-type JObject
  (Immutable-HashTable Symbol JSExpr))

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
(define (object f)
  (λ (value)
    (->> (jobject! value)
         f)))

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

(module+ test
  (define value : JSExpr
    #hash((name . 10)))

  (struct user ([name : (Nullable String)])
    #:transparent
    #:type-name User)

  (define json->user
    (object (λ ([o : JObject])
              (let ([name (jfield o 'name jstring?)])
                (user name)))))

  (json->user value))
