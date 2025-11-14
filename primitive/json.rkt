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

(struct exn:fail:json exn:fail ())

(define-syntax-rule (if-let [body result])
  (cond
    [body result]
    [else #f]))

(: json/unwrap-option
   (All (a) (-> (Option a) String a)))
(define (json/unwrap-option ma msg)
  (option/unwrap-exn ma
                     (exn:fail:json msg (current-continuation-marks))))

(: jstring? (-> JSExpr (Option String)))
(define (jstring? value)
  (if-let ([string? value] value)))

(: jstring! (-> JSExpr String))
(define (jstring! json)
  (->> (jstring? json)
       (json/unwrap-option it
                           (format "无法将~a转化成string" json))))

(: jboolean? (-> JSExpr (Option Boolean)))
(define (jboolean? value)
  (if-let ([boolean? value] value)))

(: jboolean! (-> JSExpr Boolean))
(define (jboolean! value)
  (->> (jboolean? value)
       (let ([msg (format "~a无法转化为boolean" value)])
         (json/unwrap-option it msg))))

(: jobject? (-> JSExpr (Option (Immutable-HashTable Symbol JSExpr))))
(define (jobject? value)
  (if-let
   ([hash? value]
    (cast value (Immutable-HashTable Symbol JSExpr)))))

(: jobject! (-> JSExpr (Immutable-HashTable Symbol JSExpr)))
(define (jobject! value)
  (->> (jobject? value)
       (let ([msg (format "~a无法转化为object" value)])
         (json/unwrap-option it msg))))
