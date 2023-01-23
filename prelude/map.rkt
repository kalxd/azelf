#lang racket/base

(require racket/contract
         racket/match
         (prefix-in hash:: racket/hash))

(require "../type/map.rkt"
         "../type/ord.rkt"
         "../type/maybe.rkt"
         "../type/array.rkt"
         "../type/monad.rkt"
         "../internal/curry.rkt"
         "../internal/pipeline.rkt"
         "../internal/keyword.rkt"
         "../internal/function.rkt")

(provide (all-defined-out))

;;; 创建 ;;;
(define/contract map-empty
  Map?
  (hashmap))

(define/curry/contract (map-singleton k v)
  (-> any/c any/c Map?)
  (hashmap k v))
;;; end ;;

;;; 更新 ;;;
(define/curry/contract (map-insert k v m)
  (-> any/c any/c Map? Map?)
  (->> (map->hash m)
       (hash-set it k v)
       hash->map))

(define/curry/contract (map-remove k m)
  (-> Ord? Map? Map?)
  (->> (map->hash m)
       (hash-remove it k)
       hash->map))

(define/curry/contract (map-adjust f k m)
  (-> (-> any/c any/c)
      Ord?
      Map?
      Map?)
  (->> (map-get k m)
       (match it
         [(Just v) (map-insert k (f v) m)]
         [_ m])))

(define/curry/contract (map-update f k m)
  (-> (-> any/c (Maybe/c any/c))
      Ord?
      Map?
      Map?)
  (->> (map-get k m)
       (=<< f)
       (match it
         [(Just v) (map-insert k v m)]
         [_ (map-remove k m)])))

(define/curry/contract (map-alter f k m)
  (-> (-> (Maybe/c any/c) (Maybe/c any/c))
      Ord?
      Map?
      Map?)
  (->> (map-get k m)
       f
       (match it
         [(Just v) (map-insert k v m)]
         [_ (map-remove k m)])))
;;; end ;;;

;;; 取值 ;;;
(define/curry/contract (map-key? k m)
  (-> Ord? (Map/c Ord? any/c) boolean?)
  (->> (map->hash m)
       (hash-has-key? it k)))

(define/curry/contract (map-get k m)
  (-> Ord? (Map/c Ord? any/c) (Maybe/c any/c))
  (->> (map->hash m)
       (if (hash-has-key? it k)
           (Just (hash-ref it k))
           nothing)))

(define/contract (map-keys m)
  (-> Map? (Array/c any/c))
  (->> (map->hash m)
       hash-keys
       list->array))

(define/contract (map-values m)
  (-> Map? (Array/c any/c))
  (->> (map->hash m)
       hash-values
       list->array))

(define/contract (map-size m)
  (-> Map? exact-nonnegative-integer?)
  (->> (map->hash m)
       hash-count))
;;; end ;;;

;;; 多个map交互 ;;;
(define/contract (map-union a b)
  (-> Map? Map? Map?)
  (let ([ha (map->hash a)]
        [hb (map->hash b)])
    (->> (hash::hash-union hb
                           ha
                           #:combine const)
         hash->map)))
;;; end ;;;
