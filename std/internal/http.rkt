#lang racket/base

(require racket/generic
         net/url
         (for-syntax racket/base
                     racket/syntax))

(require racket/contract
         racket/match
         racket/port
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
         ; http/set-query
         ; http/set-header
         http/set-body
         http/get
         http/get/json
         http/get/html
         http/head
         http/head/json
         http/head/html
         http/delete
         http/delete/json
         http/head/html
         http/options
         http/options/json
         http/options/html
         http/post
         http/post/json
         http/post/html
         http/put
         http/put/json
         http/put/html
         http/download-to)

(struct BaseRequest [url query header]
  #:transparent)

#|
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
|#

(struct PlainRequest BaseRequest [redirect]
  #:transparent)

(struct BodyRequest BaseRequest [body]
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

(define-generics Requestable
  (request/->base-request Requestable)
  (request/set-header k v Requestable)
  (request/set-query k v Requestable)

  #:defaults
  ([string?
    (define/generic self/->base-request request/->base-request)
    (define/generic self/set-query request/set-query)
    (define/generic self/set-header request/set-header)
    (define/contract request/->base-request
      (-> string? BaseRequest?)
      (>-> string->url self/->base-request))
    (define/contract (request/set-header k v url-link)
      (-> symbol? Show? string? BaseRequest?)
      (->> (request/->base-request url-link)
           (self/set-header k v it)))
    (define/contract (request/set-query k v url-link)
      (-> Show? Show? string? BaseRequest?)
      (->> (request/->base-request url-link)
           (self/set-query k v it)))]
   [url?
    (define/generic self/set-header request/set-header)
    (define/contract (request/->base-request self)
      (-> url? BaseRequest?)
      (define-values (raw-url query)
        (let ([query (->> (url-query self)
                          list->map)]
              [raw-url (struct-copy url self
                                    [query '()])])
          (values raw-url query)))
      (BaseRequest raw-url query map-empty))
    (define/contract (request/set-header k v self)
      (-> symbol? Show? url? BaseRequest?)
      (->> (request/->base-request self)
           (self/set-header k v it)))]
   [BaseRequest?
    (define/contract request/->base-request
      (-> BaseRequest? BaseRequest?)
      identity)
    (define/contract (request/set-query k v self)
      (-> symbol? Show? BaseRequest? BaseRequest?)
      (->> (BaseRequest-query self)
           (map-insert k (show v))
           (struct-copy BaseRequest self [query it])))
    (define/contract (request/set-header k v self)
      (-> Show? Show? BaseRequest? BaseRequest?)
      (->> (BaseRequest-header self)
           (map-insert (show k) (show v))
           (struct-copy BaseRequest self [header it])))]))

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

(define-syntax (make-plain-function stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([method-name (format-id #'name "http/~a" #'name)]
                   [method-name/json (format-id #'name "http/~a/json" #'name)]
                   [method-name/html (format-id #'name "http/~a/html" #'name)]
                   [function-name (format-id #'name "~a-pure-port" #'name)])
       #'(begin
           (define/contract method-name
             (-> ToPlainRequest? input-port?)
             (make-plain-request function-name))
           (define/contract method-name/json
             (-> ToPlainRequest? any/c)
             (>-> method-name
                  json/read))
           (define/contract method-name/html
             (-> ToPlainRequest? string?)
             (>-> method-name port->string))))]))

(define-syntax (make-body-function stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([method-name (format-id #'name "http/~a" #'name)]
                   [method-name/json (format-id #'name "http/~a/json" #'name)]
                   [method-name/html (format-id #'name "http/~a/html" #'name)]
                   [function-name (format-id #'name "~a-pure-port" #'name)])
       #'(begin
           (define/contract method-name
             (-> ToBodyRequest? input-port?)
             (make-body-request function-name))
           (define/contract method-name/json
             (-> ToBodyRequest? any/c)
             (>-> method-name
                  json/read))
           (define/contract method-name/html
             (-> ToBodyRequest? string?)
             (>-> method-name port->string))))]))

(make-plain-function get)
(make-plain-function head)
(make-plain-function delete)
(make-plain-function options)

(make-body-function post)
(make-body-function put)

(define/contract (http/download-to url-link save-path)
  (-> ToPlainRequest? path-string? void?)
  (->> (http/get url-link)
       port->bytes
       (call-with-output-file save-path
         (λ (port)
           (write-bytes it port))
         #:exists 'replace)
       void))
