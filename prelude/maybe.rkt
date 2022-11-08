#lang racket/base

(require racket/generic
         racket/contract
         racket/match
         (only-in json
                  json-null)
         syntax/parse/define

         (for-syntax racket/base))

(require "../internal/curry.rkt"
         "../internal/error.rkt"
         "./eq.rkt"
         "./json.rkt")

(provide Nothing
         Nothing?
         nothing
         Just
         Just?
         Maybe/c
         maybe?
         maybe-map
         maybe->
         maybe-then
         maybe-filter
         maybe-replace
         maybe-and
         maybe-alt
         maybe-or
         maybe-else
         ->maybe
         maybe-unwrap
         maybe-catch)

(struct Nothing []
  #:transparent

  #:methods gen:Eq
  [(define/match (eq:= a b)
     [((Nothing) (Nothing)) #t]
     [(_ _) #f])]

  #:methods gen:ToJSON
  [(define (->json self)
     (json-null))]

  #:property prop:sequence
  (λ (self)
    (in-list '())))

(struct Just [value]
  #:transparent

  #:methods gen:Eq
  [(define/generic self/= eq:=)
   (define/match (eq:= a b)
     [((Just a) (Just b)) (self/= a b)]
     [(_ _) #f])]

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match (->json self)
     [((Just a)) (self/->json a)])]

  #:property prop:sequence
  (match-lambda
    [(Just a) a]))

(define/contract (Maybe/c a)
  (-> any/c contract?)
  (or/c Nothing? (struct/c Just a)))

(define/contract nothing
  (Maybe/c any/c)
  (Nothing))

(define/contract (maybe? a)
  (-> any/c boolean?)
  (or (Just? a)
      (Nothing? a)))

(define/curry/contract (maybe-map f a)
  (-> (-> any/c any/c)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (Just (f a))]
    [_ a]))

(define/curry (maybe-> b a)
  (match a
    [(Just a) a]
    [_ b]))

(define/curry/contract (maybe-then f a)
  (-> (-> any/c (Maybe/c any/c))
      (Maybe/c any/c)
      (Maybe/c any/c))
  (match a
    [(Just a) (f a)]
    [_ a]))

(define/curry/contract (maybe-filter f ma)
  (-> (-> any/c boolean?)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (define (g a)
    (if (f a)
        (Just a)
        nothing))
  (maybe-then g ma))

(define/curry/contract (maybe-replace a ma)
  (-> any/c (Maybe/c any/c) (Maybe/c any/c))
  (match ma
    [(Just _) (Just a)]
    [_ ma]))

(define/curry/contract (maybe-and ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) _) nothing]
    [(_ (Nothing)) nothing]
    [(_ _) mb]))

(define/curry/contract (maybe-or ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) _) nothing]
    [(_ (Nothing)) nothing]
    [(_ _) ma]))

(define/curry/contract (maybe-alt ma mb)
  (-> (Maybe/c any/c) (Maybe/c any/c) (Maybe/c any/c))
  (match* (ma mb)
    [((Nothing) (Just _)) mb]
    [((Just _) _) ma]
    [(_ _) nothing]))

(define/curry/contract (maybe-else a f ma)
  (-> any/c
      (-> any/c any/c)
      (Maybe/c any/c)
      any/c)
  (match ma
    [(Just a) (f a)]
    [_ a]))

(define/contract (->maybe a)
  (-> any/c (Maybe/c any/c))
  (if a (Just a) nothing))

(define/match (maybe-unwrap a)
  [((Just a)) a]
  [(_) (raise-unwrap-error "maybe-unwrap: 试图解包nothing！")])

(define-syntax-rule (maybe-catch action)
  (with-handlers
    ([exn:fail? (λ (_) nothing)])
    (Just action)))


;;; 将a包装成Maybe，已经是了就不用再包装一层。
(define (*->maybe* a)
  (cond [(maybe? a) a]
        [else (->maybe a)]))

(define-syntax-parser *maybe/do*
  ; a <- ma 绑定语法
  [(_ (var:id (~literal <-) e:expr) es:expr ...)
   #'(match (*->maybe* e)
       [(Nothing) nothing]
       [(Just var) (*maybe/do* es ...)])]

  ; let a = b 普通赋值语法
  [(_ ((~literal let) var:id (~literal =) e:expr) es:expr ...)
   #'(let ([var e])
       (*maybe/do* es ...))]

  ; 副作用
  [(_ ((~literal !) es:expr ...+) rs:expr ... +)
   #'(begin
       es ...
       (*maybe/do* rs ...))]

  ; 多条语句。
  [(_ e1:expr e2:expr ...+)
   #'(begin
       e1
       (*maybe/do* e2 ...))]

  ; 最后一条语句
  [(_ e:expr) #'(*->maybe* e)])

(module+ test
  (*maybe/do*
   (a <- nothing)
   (let b = a)
   (displayln a)
   (displayln b)))
