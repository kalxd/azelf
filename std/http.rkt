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
         "../type/map.rkt"
         "../prelude/map.rkt"
         "../prelude/array.rkt")

(require racket/contract
         racket/match
         net/url
         racket/path
         racket/struct
         (only-in json
                  read-json)
         (prefix-in list:: racket/list))

(provide (all-from-out net/url))

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

;;; HTTP Client ;;;

(struct RequestBase [url query redirect]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'Url)
      (match-lambda
        [(RequestBase url query redirect)
         (cond
           [(= (map-size query) 0)
            (list (url->string url))]
           [else (->> (format "~A&~A"
                              (url->string url)
                              query)
                      list)])])))])

(define/contract (priv/req-base-url input)
  (-> (or/c url? string?) RequestBase?)
  (define link-url
    (if (url? input) input (string->url input)))
  (define-values (plain-url query)
    (let ([query (->> (url-query link-url) list->map)]
          [plain-url (struct-copy url link-url [query '()])])
      (values plain-url query)))
  (RequestBase plain-url query 0))

(define http/get-url (curry/n 1 get-pure-port))
(define http/head-url (curry/n 1 head-pure-port))
(define http/delete-url (curry/n 1 delete-pure-port))
(define http/options-url (curry/n 1 options-pure-port))
