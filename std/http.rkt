#lang racket/base

(require "../internal/pipeline.rkt"
         "../internal/match.rkt"
         "../internal/keyword.rkt"
         "../internal/function.rkt"
         "../internal/curry.rkt"

         "../type/functor.rkt"
         "../type/monad.rkt"
         "../type/maybe.rkt"
         "../type/array.rkt"
         "../type/ord.rkt"
         "../prelude/array.rkt")

(require racket/contract
         racket/match
         net/url
         racket/path)

(provide (all-from-out net/url))

(provide (except-out (all-defined-out)
                     priv/send))

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

(struct RequestOption [redirect]
  #:transparent)

(struct Request [url header body option]
  #:transparent)

(define/curry (priv/send f data)
  (match-define [Request url header body option] data)
  (f url header #:redirecttions (maybe-> 0 option)))

(define/contract (http/get request)
  (-> Request? input-port?)
  (priv/send get-pure-port request))

(define http/get/url (curry/n 1 get-pure-port))
(define http/head/url (curry/n 1 head-pure-port))
(define http/delete/url (curry/n 1 delete-pure-port))
(define http/options/url (curry/n 1 options-pure-port))
