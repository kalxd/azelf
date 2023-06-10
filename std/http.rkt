#lang racket/base

(require "../internal/pipeline.rkt"
         "../internal/match.rkt"
         "../internal/keyword.rkt"
         "../internal/function.rkt"
         "../internal/curry.rkt"
         "../internal/mod.rkt"

         "../type/functor.rkt"
         "../type/monad.rkt"
         "../type/maybe.rkt"
         "../type/array.rkt"
         "../type/ord.rkt"
         "../type/map.rkt"
         "../prelude/map.rkt"
         "../prelude/array.rkt")

(require racket/contract
         racket/match
         net/url
         racket/path)

(provide (all-from-out net/url))
(export-from "./internal/http.rkt")

(provide url/clear-pathname
         url/filename
         url/filename-without-ext
         url/rename-with
         url/rename-to)

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

(define/contract (url/filename-without-ext url-link)
  (-> url? (Maybe/c string?))
  (monad/do
   (filename <- (url/filename url-link))
   (Just (->> (path-replace-extension filename #"")
              path->string))))

;;; end ;;;

(define/contract (url/rename-with f url-link)
  (-> (-> string? string?)
      url?
      url?)
  (define path-segments
    (->> (url-path url-link)
         list->array))
  (define last-index
    (->> (length path-segments)
         sub1
         (max 0)))
  (define/match1 adjust
    [(path/param path param)
     (path/param (f path) param)])
  (->> (array-adjust adjust last-index path-segments)
       (struct-copy url url-link [path (array->list it)])))

(define/contract (url/rename-to filename url-link)
  (-> string? url? url?)
  (url/rename-with (const filename) url-link))
