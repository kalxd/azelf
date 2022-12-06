#lang racket/base

(require racket/contract)

(require "../type/map.rkt"
         "../type/eq.rkt"
         "../type/maybe.rkt"
         "../type/array.rkt"
         "../internal/curry.rkt"
         "../internal/pipeline.rkt"
         "../internal/keyword.rkt")

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
  (-> Eq? Map? Map?)
  (->> (map->hash m)
       (hash-remove it k)
       hash->map))
;;; end ;;;

;;; 取值 ;;;
(define/curry/contract (map-key? k m)
  (-> Eq? (Map/c Eq? any/c) boolean?)
  (->> (map->hash m)
       (hash-has-key? it k)))

(define/curry/contract (map-get k m)
  (-> Eq? (Map/c Eq? any/c) (Maybe/c any/c))
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
