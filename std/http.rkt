#lang racket/base

(require "../internal/pipeline.rkt"
         "../internal/match.rkt"
         "../internal/keyword.rkt"

         "../type/functor.rkt"
         "../type/array.rkt"
         "../prelude/array.rkt")

(require racket/contract
         net/url
         racket/path)

(provide (all-from-out net/url))

(provide url/clear-pathname
         url/rename-path-with)

(define/contract url/clear-pathname
  (-> url? (Array/c string?))
  (>-> url-path
       list->array
       (map path/param-path)))

(define/contract (url/rename-path-with f url-link)
  (-> (-> string? string?)
      url?
      url?)
  (define path-segments
    (->> (url-path url-link)
         list->array))
  (define last-index
    (->> (length path-segments)
         sub1))
  (define/match1 adjust
    [(path/param path param)
     (path/param (f path) param)])
  (->> (array-adjust adjust last-index path-segments)
       (struct-copy url url-link [path (array->list it)])))
