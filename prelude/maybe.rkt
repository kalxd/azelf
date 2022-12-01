#lang racket/base

(require racket/contract
         racket/match
         syntax/parse/define
         racket/stxparam
         (for-syntax racket/base))

(require "../type/maybe.rkt"
         "../type/monad.rkt"

         "../internal/keyword.rkt"
         "../internal/curry.rkt"
         "../internal/match.rkt")

(provide (except-out (all-defined-out)
                     *->maybe*
                     *maybe/do*))

(define/curry/contract (maybe-filter f ma)
  (-> (-> any/c boolean?)
      (Maybe/c any/c)
      (Maybe/c any/c))
  (define (g a)
    (if (f a)
        (Just a)
        nothing))
  (=<< g ma))

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
  [(_ ((~literal !) es:expr ...) rs:expr ...+)
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
