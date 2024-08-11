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
                  define/match1
                  define/match1/contract)
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

(provide http/set-query
         http/set-header
         http/set-body
         http/set-redirect
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
         http/download)

(struct RequestOption [uri
                       query
                       header
                       body
                       redirect]
  #:transparent)

(define/curry/contract (make-correct-url base-option)
  (-> RequestOption? (values url? (listof string?)))
  (match-define (RequestOption base-url query header _ _) base-option)
  (define final-url
    (->> (map->list query)
         (struct-copy url base-url [query it])))
  (define (make-header-pair acc k v)
    (->> (format "~a: ~a" k v)
         (cons it acc)))
  (define header-list
    (map-fold make-header-pair '() header))
  (values final-url header-list))

(define/curry/contract (make-plain-request f option)
  (-> procedure? RequestOption? input-port?)
  (match-define (RequestOption _ _ _ _ redirect) option)
  (define-values (final-url header-list)
    (make-correct-url option))
  (f final-url header-list redirect))

(define/curry/contract (make-body-request f option)
  (-> (-> url? bytes? (listof string?) input-port?)
      RequestOption?
      input-port?)
  (match-define (RequestOption req-url query header body _) option)
  (define-values (final-url header-list)
    (make-correct-url option))
  (define body-byte
    (maybe-else #"" json->byte body))
  (displayln header-list)
  (displayln (length header-list))
  (displayln body-byte)
  (f final-url body-byte header-list))

(define-generics Requestable
  (request/make-url Requestable)
  (request/set-header k v Requestable)
  (request/set-query k v Requestable)
  (request/set-redirect n Requestable)
  (request/set-body n Requestable)
  (request/get Requestable)
  (request/head Requestable)
  (request/delete Requestable)
  (request/options Requestable)
  (request/post Requestable)
  (request/put Requestable)

  #:defaults
  ([string?
    (define/generic self/make-url request/make-url)
    (define/generic self/set-query request/set-query)
    (define/generic self/set-header request/set-header)
    (define/generic self/set-redirect request/set-redirect)
    (define/generic self/set-body request/set-body)
    (define/generic self/get request/get)
    (define/generic self/head request/head)
    (define/generic self/delete request/delete)
    (define/generic self/options request/options)
    (define/generic self/post request/post)
    (define/generic self/put request/put)
    (define/contract request/make-url
      (-> string? RequestOption?)
      (>-> string->url self/make-url))
    (define/contract (request/set-query k v self)
      (-> symbol? Show? string? RequestOption?)
      (->> (request/make-url self)
           (self/set-query k v it)))
    (define/contract (request/set-header k v self)
      (-> Show? Show? string? RequestOption?)
      (->> (request/make-url self)
           (self/set-header k v it)))
    (define/contract (request/set-redirect n self)
      (-> exact-nonnegative-integer? string? RequestOption?)
      (->> (request/make-url self)
           (self/set-redirect n it)))
    (define/contract (request/set-body body self)
      (-> ToJSON? string? RequestOption?)
      (->> (request/make-url self)
           (self/set-body body it)))
    (define/contract request/get
      (-> string? input-port?)
      (>-> request/make-url self/get))
    (define/contract request/head
      (-> string? input-port?)
      (>-> request/make-url self/head))
    (define/contract request/delete
      (-> string? input-port?)
      (>-> request/make-url self/delete))
    (define/contract request/options
      (-> string? input-port?)
      (>-> request/make-url self/options))
    (define/contract request/post
      (-> string? input-port?)
      (>-> request/make-url self/post))
    (define/contract request/put
      (-> string? input-port?)
      (>-> request/make-url self/put))]

   [url?
    (define/generic self/set-query request/set-query)
    (define/generic self/set-header request/set-header)
    (define/generic self/set-redirect request/set-redirect)
    (define/generic self/set-body request/set-body)
    (define/generic self/get request/get)
    (define/generic self/head request/head)
    (define/generic self/delete request/delete)
    (define/generic self/options request/options)
    (define/generic self/post request/post)
    (define/generic self/put request/put)
    (define/contract (request/make-url self)
      (-> url? RequestOption?)
      (define-values (raw-url query)
        (let ([query (->> (url-query self)
                          list->map)]
              [raw-url (struct-copy url self
                                    [query '()])])
          (values raw-url query)))
      (RequestOption raw-url query map-empty nothing 0))
    (define/contract (request/set-header k v self)
      (-> symbol? Show? url? RequestOption?)
      (->> (request/make-url self)
           (self/set-header k v it)))
    (define/contract (request/set-query k v self)
      (-> Show? Show? url? RequestOption?)
      (->> (request/make-url self)
           (self/set-query k v it)))
    (define/contract (request/set-body body self)
      (-> ToJSON? url? RequestOption?)
      (->> (request/make-url self)
           (self/set-body body it)))
    (define/contract request/get
      (-> url? input-port?)
      (>-> request/make-url self/get))
    (define/contract request/head
      (-> url? input-port?)
      (>-> request/make-url self/head))
    (define/contract request/delete
      (-> url? input-port?)
      (>-> request/make-url self/delete))
    (define/contract request/options
      (-> url? input-port?)
      (>-> request/make-url self/options))
    (define/contract request/post
      (-> url? input-port?)
      (>-> request/make-url self/post))
    (define/contract request/put
      (-> url? input-port?)
      (>-> request/make-url self/put))]

   [RequestOption?
    (define/generic self/set-redirect request/set-redirect)
    (define/generic self/set-body request/set-body)
    (define/contract request/make-url
      (-> RequestOption? RequestOption?)
      identity)
    (define/contract (request/set-query key value base)
      (-> symbol? Show? RequestOption? RequestOption?)
      (->> (RequestOption-query base)
           (map-insert key (show value))
           (struct-copy RequestOption base [query it])))
    (define/contract (request/set-header key value base)
      (-> Show? Show? RequestOption? RequestOption?)
      (->> (RequestOption-header base)
           (map-insert (show key) (show value))
           (struct-copy RequestOption base [header it])))
    (define/contract (request/set-redirect n self)
      (-> exact-nonnegative-integer? RequestOption? RequestOption?)
      (struct-copy RequestOption self [redirect n]))
    (define/contract (request/set-body body self)
      (-> ToJSON? RequestOption? RequestOption?)
      (struct-copy RequestOption self [body (Just body)]))
    (define/contract request/get
      (-> RequestOption? input-port?)
      (make-plain-request get-pure-port))
    (define/contract request/head
      (-> RequestOption? input-port?)
      (make-plain-request head-pure-port))
    (define/contract request/delete
      (-> RequestOption? input-port?)
      (make-plain-request delete-pure-port))
    (define/contract request/options
      (-> RequestOption? input-port?)
      (make-plain-request options-pure-port))
    (define/contract request/post
      (-> RequestOption? input-port?)
      (make-body-request post-pure-port))
    (define/contract request/put
      (-> RequestOption? input-port?)
      (make-body-request put-pure-port))]))

(define-syntax (make-all-function stx)
  #`(begin
      #,@(for/list ([x '(get head delete options post put)])
           (with-syntax ([origin (format-id stx "request/~a" x)]
                         [http-name (format-id stx "http/~a" x)]
                         [http-json (format-id stx "http/~a/json" x)]
                         [http-html (format-id stx "http/~a/html" x)])
             #`(begin
                 (define/contract http-name
                   (-> Requestable? input-port?)
                   origin)
                 (define/contract http-json
                   (-> Requestable? any/c)
                   (>-> (http/set-header "Content-Type" "application/json")
                        http-name
                        json/read))
                 (define/contract http-html
                   (-> Requestable? string)
                   (>-> http-name port->string)))))))

(define/curry/contract (http/set-query k v source)
  (-> symbol? Show? Requestable? RequestOption?)
  (request/set-query k v source))

(define/curry/contract (http/set-header k v source)
  (-> Show? Show? Requestable? RequestOption?)
  (request/set-header k v source))

(define/curry/contract (http/set-body body source)
  (-> ToJSON? Requestable? RequestOption?)
  (request/set-body body source))

(define/curry/contract (http/set-redirect n source)
  (-> exact-nonnegative-integer? Requestable? RequestOption?)
  (request/set-redirect n source))

(make-all-function)

(define/curry/contract (http/download save-path source)
  (-> path-string? Requestable? void?)
  (->> (http/get source)
       port->bytes
       (call-with-output-file save-path
         (λ (port)
           (write-bytes it port))
         #:exists 'replace)
       void))

(module+ test
  (require net/base64)

  (define password "UYAzwDJ84BidDe4X")
  (define username "null")

  (define bear
    (->> (string-append password username)
         string->bytes/utf-8
         base64-encode))

  (define rsp
    (->> "http://localhost:3000/json"
         (http/set-header "Content-Type" "application/x-www-form-urlencoded")
         (http/set-header "Authorization" (format "Basic ~a" bear))
         (http/set-body "a=b")
         (! (displayln it))
         request/post)))
