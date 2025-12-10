#lang typed/racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(provide Nullable
         (rename-out [nullable/nil? nullable/has-nil]
                     [nullable/some nullable/has-some])
         match-nullable?
         nullable/map
         nullable/chain
         nullable/unwrap-exn
         nullable/unwrap-error
         nullable/unwrap
         nullable/unwrap-or
         nullable->option
         option->nullable
         do/nullable?)

(struct nullable/nil ()
  #:type-name Nullable/Nil)

(struct (A) nullable/some ([value : A])
  #:type-name Nullable/Some
  #:transparent)

(define-type (Nullable A)
  (U Nullable/Nil (Nullable/Some A)))

(define-syntax-rule (match-nullable? ma [a (body ...)])
  (cond
    [(nullable/nil? ma) ma]
    [else
     (match-let ([(nullable/some a) ma])
       (body ...))]))

(: nullable/map
   (All (A B)
        (-> (Nullable A)
            (-> A B)
            (Nullable B))))
(define (nullable/map ma f)
  (match-nullable?
   ma
   [a (nullable/some (f a))]))

(: nullable/chain
   (All (A B)
        (-> (Nullable A)
            (-> A (Nullable B))
            (Nullable B))))
(define (nullable/chain ma f)
  (match-nullable?
   ma
   [a (f a)]))

(define-syntax-rule (define-nil-function (name args ...) body)
  (define (name ma args ...)
    (cond
      [(nullable/nil? ma) body]
      [else
       (match-let ([(nullable/some a) ma])
         a)])))

(: nullable/unwrap-exn
   (All (a)
        (-> (Nullable a)
            exn
            a)))
(define-nil-function (nullable/unwrap-exn e)
  (raise e))

(: nullable/unwrap-error
   (All (a)
        (-> (Nullable a)
            String
            a)))
(define-nil-function (nullable/unwrap-error msg)
  (raise-user-error msg))

(: nullable/unwrap
   (All (a)
        (-> (Nullable a)
            a)))
(define-nil-function (nullable/unwrap)
  (raise-user-error "无法从nil中取值！"))

(: nullable/unwrap-or
   (All (a)
        (-> (Nullable a)
            a
            a)))
(define-nil-function (nullable/unwrap-or a)
  a)

(: nullable->option
   (All (A)
        (-> (Nullable A)
            (Option A))))
(define (nullable->option ma)
  (cond
    [(nullable/nil? ma) #f]
    [else
     (match-let ([(nullable/some a) ma])
       a)]))

(: option->nullable
   (All (a) (-> (Option a)
                (Nullable a))))
(define (option->nullable a)
  (if a
      (nullable/some a)
      (nullable/nil)))

(define-syntax (do/nullable? stx)
  (define-syntax-class define-bind
    #:description "define绑定"
    #:literals (define :)
    (pattern (define key:id e:expr)
             #:with expr #'(define key e))
    (pattern (define key:id : ty:expr e:expr)
             #:with expr #'(define key : ty e)))

  (syntax-parse stx
    ; 绑定
    [(_ (val:id (~literal <-) e:expr) es:expr ...)
     #'(match-nullable?
        e
        [a (let [(val a)]
             (do/nullable? es ...))])]
    ; 变量绑定
    [(_ e:define-bind es:expr ...+)
     #'(begin e.expr (do/nullable? es ...))]

    ; 多条语句
    [(_ e:expr es:expr ...+)
     #'(match-nullable?
        e
        [_ (do/nullable? es ...)])]

    ; 最后一句
    [(_ e:expr) #'e]))
