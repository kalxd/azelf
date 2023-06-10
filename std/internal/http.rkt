#lang racket/base

(require racket/generic
         net/url)

(require racket/contract
         racket/match
         (only-in "../../internal/pipeline.rkt"
                  ->>)
         (only-in "../../internal/keyword.rkt"
                  it)
         (only-in "../../internal/curry.rkt"
                  define/curry/contract)
         (only-in "../../internal/function.rkt"
                  identity)
         (only-in "../../internal/match.rkt"
                  define/match1)
         (only-in "../../type/maybe.rkt"
                  Just
                  nothing)
         "../../type/json.rkt"
         (only-in "../../type/show.rkt"
                  Show?
                  show)
         (only-in "../../type/map.rkt"
                  list->map
                  map->list)
         "../../prelude/map.rkt")

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
  (-> Show? Show? BaseRequest? BaseRequest?)
  (->> (BaseRequest-query base)
       (map-insert (show key) (show value))
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
  ([BaseRequest?
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
  ([BaseRequest?
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

(define/curry/contract (make-plain-request f base-option)
  (-> procedure? ToPlainRequest? input-port?)
  (match-define (PlainRequest req-url query header redirect)
    (->plain-request base-option))
  (define final-url
    (->> (map->list query)
         (struct-copy url req-url [query it])))
  (define (make-header acc key value)
    (cons (format "~a: ~a" key value) acc))
  (f final-url
     (map-fold make-header '() header)
     #:redirections redirect))

(define/contract http/get
  (-> ToPlainRequest? input-port?)
  (make-plain-request get-pure-port))

(define/contract http/head
  (-> ToPlainRequest? input-port?)
  (make-plain-request head-pure-port))

(define/contract http/delete
  (-> ToPlainRequest? input-port?)
  (make-plain-request delete-pure-port))

(define/contract http/options
  (-> ToPlainRequest? input-port?)
  (make-plain-request options-pure-port))
