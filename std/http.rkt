#lang racket/base

(require "../internal/pipeline.rkt"
         "../internal/match.rkt"
         "../internal/keyword.rkt"

         "../type/functor.rkt"
         "../type/maybe.rkt"
         "../type/array.rkt"
         "../prelude/array.rkt")

(require racket/contract
         net/url
         racket/path)

(provide (all-from-out net/url))

(provide url/clear-pathname
         url/rename-path-with)

(module inner racket/base
  (require "../internal/pipeline.rkt"
           "../internal/keyword.rkt"
           "../type/array.rkt"
           "../type/functor.rkt"
           "../prelude/array.rkt")

  (require net/url)

  (provide (all-defined-out))

  (define url-raw-path-segments
    (>-> url-path list->array))

  (define (play/url-pathname f url-link)
    (->> (url-path url-link)
         list->array
         (map path/param-path)
         f)))

(require (prefix-in inner:: 'inner))

;;; 获取 ;;;

(define/contract url/clear-pathname
  (-> url? (Array/c string?))
  (>-> url-path
       list->array
       (map path/param-path)))

(define/contract url/filename
  (-> url? (Maybe/c string?))
  (>-> inner::url-raw-path-segments
       tail
       (map path/param-path)))

;;; end ;;;

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
