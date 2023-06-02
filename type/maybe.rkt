#lang racket/base

(require racket/generic
         racket/contract
         racket/match
         (only-in json
                  json-null)
         racket/stxparam
         syntax/parse/define
         (for-syntax racket/base))

(require "../internal/curry.rkt"
         "../internal/error.rkt"
         "../internal/keyword.rkt"
         "./show.rkt"
         "./eq.rkt"
         "./ord.rkt"
         "./functor.rkt"
         "./applicative.rkt"
         "./monad.rkt"
         "./json.rkt")

(provide Nothing
         Nothing?
         nothing
         Just
         Just?
         Maybe/c
         maybe?
         maybe->
         ->maybe
         maybe-just
         maybe-unwrap
         maybe/do)

(struct Nothing []
  #:transparent

  #:methods gen:Show
  [(define show:show generic-fmt-show)]

  #:methods gen:Eq
  [(define/match (eq:= a b)
     [((Nothing) (Nothing)) #t]
     [(_ _) #f])]

  #:methods gen:Ord
  [(define/match (ord:compare a b)
     [((Nothing) (Nothing)) 'eq]
     [((Nothing) _) 'lt])]

  #:methods gen:ToJSON
  [(define (->json self)
     (json-null))]

  #:methods gen:Functor
  [(define (functor:map f a)
     a)]

  #:methods gen:Applicative
  [(define (applicative:ap f ma)
     ma)]

  #:methods gen:Monad
  [(define (monad:bind ma f)
     ma)]

  #:property prop:sequence
  (λ (self)
    (in-list '())))

(struct Just [value]
  #:transparent

  #:methods gen:Show
  [(define show:show generic-fmt-show)]

  #:methods gen:Eq
  [(define/generic self/= eq:=)
   (define/match (eq:= a b)
     [((Just a) (Just b)) (self/= a b)]
     [(_ _) #f])]

  #:methods gen:Ord
  [(define/generic self/compare ord:compare)
   (define/match (ord:compare a b)
     [((Just a) (Just b)) (self/compare a b)]
     [(_ _) 'gt])]

  #:methods gen:ToJSON
  [(define/generic self/->json ->json)
   (define/match (->json self)
     [((Just a)) (self/->json a)])]

  #:methods gen:Functor
  [(define/match (functor:map f a)
     [(_ (Just a)) (Just (f a))])]

  #:methods gen:Applicative
  [(define/match (applicative:ap f ma)
     [((Just f) (Just a)) (Just (f a))]
     [((Nothing) _) nothing])]

  #:methods gen:Monad
  [(define/match (monad:bind ma f)
     [((Just a) _) (f a)])]

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

(define/contract (->maybe a)
  (-> any/c (Maybe/c any/c))
  (if a (Just a) nothing))

(define/curry (maybe-> b a)
  (match a
    [(Just a) a]
    [_ b]))

(define/curry/contract (maybe-just msg ma)
  (-> string? (Maybe/c any/c) any/c)
  (match ma
    [(Just a) a]
    [_ (raise-unwrap-error msg)]))

(define/contract maybe-unwrap
  (-> (Maybe/c any/c) any/c)
  (maybe-just "maybe-unwrap: 试图解包nothing！"))

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
  [(_ ((~datum let) var:id (~datum =) e:expr) es:expr ...)
   #'(let ([var e])
       (*maybe/do* es ...))]

  ; 副作用
  [(_ ((~datum !) es:expr ...) rs:expr ...+)
   #'(begin
       es ...
       (*maybe/do* rs ...))]

  ; 多条语句。
  [(_ e1:expr e2:expr ...+)
   #'(match (*->maybe* e1)
       [(Nothing) nothing]
       [_ (*maybe/do* e2 ...)])]

  ; 最后一条语句
  [(_ e:expr) #'(*->maybe* e)])

(define-syntax-rule (maybe/do body ...)
  (call/cc (λ (k)
             (define (f . xs)
               (define value
                 (match xs
                   [(list) nothing]
                   [(list a) (if (maybe? a) a (Just a))]
                   [_ (raise-syntax-error #f "break最多接受一个值！")]))
               (k value))
             (syntax-parameterize
                 ([break (make-rename-transformer #'f)])
               (*maybe/do* body ...)))))
