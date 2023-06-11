#lang racket/base

(require racket/generic
         net/url)

(require racket/contract
         racket/match
         (only-in "../../internal/pipeline.rkt"
                  ->>
                  >->)
         (only-in "../../internal/keyword.rkt"
                  it)
         (only-in "../../internal/curry.rkt"
                  define/curry/contract)
         (only-in "../../internal/function.rkt"
                  identity)
         (only-in "../../internal/match.rkt"
                  define/match1)
         "../../type/maybe.rkt"
         "../../type/json.rkt"
         "../../type/show.rkt"
         "../../type/map.rkt"
         (only-in "../../prelude/maybe.rkt"
                  maybe-else)
         (only-in "../../prelude/json.rkt"
                  json/read)
         (only-in "../../prelude/map.rkt"
                  map-insert
                  map-empty
                  map-empty?
                  map-fold))

(provide http/url
         http/set-query
         http/set-header
         http/set-body
         http/get
         http/get/json
         http/head
         http/head/json
         http/delete
         http/delete/json
         http/options
         http/options/json)

(struct BaseRequest [url query header]
  #:transparent)

(define/contract (http/url input)
  (-> (or/c string? url?) BaseRequest?)
  (define url-link
    (if (url? input) input (string->url input)))
  (define-values (raw-url query)
    (let ([query (->> (url-query url-link)
                      list->map)]
          [raw-url (struct-copy url url-link
                                [query '()])])
      (values raw-url query)))
  (BaseRequest raw-url query map-empty))

(define/curry/contract (http/set-query key value base)
  (-> symbol? Show? BaseRequest? BaseRequest?)
  (->> (BaseRequest-query base)
       (map-insert key (show value))
       (struct-copy BaseRequest base [query it])))

(define/curry/contract (http/set-header key value base)
  (-> Show? Show? BaseRequest? BaseRequest?)
  (->> (BaseRequest-header base)
       (map-insert (show key) (show value))
       (struct-copy BaseRequest base [header it])))

(struct PlainRequest BaseRequest [redirect]
  #:transparent)

(struct BodyRequest BaseRequest [body]
  #:transparent)

(define-generics ToPlainRequest
  (->plain-request ToPlainRequest)

  #:defaults
  ([string?
    (define/generic self/->plain-request ->plain-request)
    (define ->plain-request
      (>-> http/url self/->plain-request))]
   [url?
    (define/generic self/->plain-request ->plain-request)
    (define ->plain-request
      (>-> http/url self/->plain-request))]
   [BaseRequest?
    (define/match1 ->plain-request
      [(BaseRequest url query header)
       (PlainRequest url query header 0)])]
   [PlainRequest?
    (define ->plain-request identity)]
   [BodyRequest?
    (define/match1 ->plain-request
      [(BodyRequest url query header _)
       (PlainRequest url query header 0)])]))

(define-generics ToBodyRequest
  (->body-request ToBodyRequest)

  #:defaults
  ([string?
    (define/generic self/->body-request ->body-request)
    (define ->body-request
      (>-> http/url self/->body-request))]
   [url?
    (define/generic self/->body-request ->body-request)
    (define ->body-request
      (>-> http/url self/->body-request))]
   [BaseRequest?
    (define/match1 ->body-request
      [(BaseRequest url query header)
       (BodyRequest url query header nothing)])]
   [PlainRequest?
    (define/match1 ->body-request
      [(PlainRequest url query header _)
       (BodyRequest url query header nothing)])]
   [BodyRequest? (define ->body-request identity)]))

(define/curry/contract (http/set-redirect redirect base)
  (-> exact-nonnegative-integer? ToPlainRequest? PlainRequest?)
  (->> (->plain-request base)
       (struct-copy PlainRequest it [redirect redirect])))

(define/curry/contract (http/set-body body base)
  (-> ToJSON? ToBodyRequest? BodyRequest?)
  (->> (->body-request base)
       (struct-copy BodyRequest it [body (Just body)])))

(define/curry/contract (make-correct-url base-option)
  (-> BaseRequest? (values url? (listof string?)))
  (match-define (BaseRequest base-url query header) base-option)
  (define final-url
    (->> (map->list query)
         (struct-copy url base-url [query it])))
  (define (make-header-pair acc k v)
    (->> (format "~a: ~a" k v)
         (cons it acc)))
  (define header-list
    (map-fold make-header-pair '() header))
  (values final-url header-list))

(define/curry/contract (make-plain-request f base-option)
  (-> procedure? ToPlainRequest? input-port?)
  (define option (->plain-request base-option))
  (match-define (PlainRequest req-url query header redirect) option)
  (define-values (final-url header-list)
    (make-correct-url option))
  (f final-url header-list))

(define/curry/contract (make-body-request f base-option)
  (-> procedure? ToBodyRequest? input-port?)
  (define option (->plain-request base-option))
  (match-define (BodyRequest req-url query header body) option)
  (define-values (final-url header-list)
    (make-correct-url option))
  (define body-byte
    (maybe-else #"" json->byte))
  (f final-url body-byte header-list))

(define/contract http/get
  (-> ToPlainRequest? input-port?)
  (make-plain-request get-pure-port))

(define/contract http/get/json
  (-> ToPlainRequest? any/c)
  (>-> http/get json/read))

(define/contract http/head
  (-> ToPlainRequest? input-port?)
  (make-plain-request head-pure-port))

(define/contract http/head/json
  (-> ToPlainRequest? any/c)
  (>-> http/head json/read))

(define/contract http/delete
  (-> ToPlainRequest? input-port?)
  (make-plain-request delete-pure-port))

(define/contract http/delete/json
  (-> ToPlainRequest? any/c)
  (>-> http/delete json/read))

(define/contract http/options
  (-> ToPlainRequest? input-port?)
  (make-plain-request options-pure-port))

(define/contract http/options/json
  (-> ToPlainRequest? any/c)
  (>-> http/options json/read))
