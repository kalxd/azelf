#lang racket/base

(require racket/match
         racket/contract

         (prefix-in base:: racket/base)
         (prefix-in list:: racket/list)

         (for-syntax racket/base
                     syntax/for-body))

(require "../type/array.rkt"
         "../type/maybe.rkt"
         "../type/eq.rkt"
         "../type/ord.rkt"
         "../internal/curry.rkt"
         "../internal/match.rkt"
         "../internal/pipeline.rkt"
         "../internal/keyword.rkt")

(provide (all-defined-out))

(module inner racket/base
  (require racket/match
           "../type/array.rkt"
           "../type/maybe.rkt"
           (prefix-in base:: racket/base)
           (prefix-in list:: racket/list))
  (provide (all-defined-out))

  (define (++ xs ys)
    (match-define (Array as ...) xs)
    (match-define (Array bs ...) ys)
    (list->array (base::append as bs)))

  (define (: x xs)
    (++ (array x) xs))

  (define (<:> x xs)
    (++ xs (array x)))

  (define (span f xs acc)
    (match xs
      [(Array) (cons acc (array))]
      [(Array y ys ...)
       (if (f y)
           (span f (list->array ys) (<:> y acc))
           (cons acc xs))]))

  (define (partition f xs)
    (match xs
      [(Array xs ...)
       (let-values ([(as bs) (list::partition f xs)])
         (cons (list->array as)
               (list->array bs)))]))

  (define/match (group-by f xs acc)
    [(_ (Array) _) acc]
    [(_ (Array a as ...) _)
     (match-define (cons ys zs)
       (span (λ (y) (f a y))
             xs
             (array)))
     (group-by f
               zs
               (<:> ys acc))])

  (define/match (filter-map f xs acc)
    [(_ (list) _) acc]
    [(_ (list a ys ...) _)
     (match (f a)
       [(Just a) (filter-map f ys (append acc (list a)))]
       [_ (filter-map f ys acc)])]))

(require (prefix-in inner:: 'inner))

;;; 合并 ;;;
(define/curry/contract (++ xs ys)
  (-> Array? Array? Array?)
  (inner::++ xs ys))

(define/curry/contract (: x xs)
  (-> any/c Array? Array?)
  (inner::: x xs))

(define/curry/contract (<:> x xs)
  (-> any/c Array? Array?)
  (inner::<:> x xs))

(define/curry/contract (zip-with f xs ys)
  (-> (-> any/c any/c any/c)
      Array?
      Array?
      Array?)
  (match-define (Array as ...) xs)
  (match-define (Array bs ...) ys)
  (list->array
   (for/list ([x as]
              [y bs])
     (f x y))))

(define/curry/contract (zip xs ys)
  (-> Array? Array? Array?)
  (zip-with cons xs ys))
;;; end ;;;

;;; 构造 ;;;
(define/curry/contract (repeat n a)
  (-> (or/c zero? positive?)
      any/c
      Array?)
  (list->array
   (for/list ([_ (in-range n)]) a)))

(define/curry/contract (range start end)
  (-> number? number? Array?)
  (list->array
   (for/list ([x (in-range start end)]) x)))

(define/contract empty
  (Array/c any/c)
  (array))

(define/contract (singleton x)
  (-> any/c (Array/c any/c))
  (array x))
;;; end ;;;

;;; 基本属性 ;;;
(define/match1/contract length
  (-> Array? exact-nonnegative-integer?)
  [(Array xs ...) (base::length xs)])

(define/match1/contract empty?
  (-> Array? boolean?)
  [(Array xs ...) (list::empty? xs)])
;;; end ;;;

;;; 子列表 ;;;
(define/match1/contract head
  (-> Array? (Maybe/c any/c))
  [(Array) nothing]
  [(Array a xs ...) (Just a)])

(define/match1/contract tail
  (-> Array? (Maybe/c any/c))
  [(Array) nothing]
  [_ (->> (array->list it)
          list::last
          Just)])

(define/curry/contract (take-while f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (match xs
    [(Array xs ...)
     (list->array (list::takef xs f))]))

(define/curry/contract (take n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::take xs m)))))

(define/curry/contract (take-right n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::take-right xs m)))))

(define/curry/contract (drop-while f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (match xs
    [(Array xs ...)
     (list->array (list::dropf xs f))]))

(define/curry/contract (drop n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::drop xs m)))))

(define/curry/contract (drop-right n xs)
  (-> exact-integer?
      Array?
      Array?)
  (if (>= 0 n)
      empty
      (match-let ([(Array xs ...) xs]
                  [m (min (length xs) n)])
        (list->array (list::drop-right xs m)))))

(define/curry/contract (filter f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (match xs
    [(Array xs ...)
     (list->array (base::filter f xs))]))

(define/curry/contract (reject f xs)
  (-> (-> any/c boolean?)
      Array?
      Array?)
  (->> (array->list xs)
       (list::filter-not f it)
       list->array))
;;; end ;;;

;;; 查找 ;;;
(define/curry/contract (find f xs)
  (-> (-> any/c boolean?)
      Array?
      (Maybe/c any/c))
  (match xs
    [(Array xs ...)
     (->maybe (base::findf f xs))]))

(define/curry/contract (member? x xs)
  (-> Eq? (Array/c Eq?) boolean?)
  (->> (find (= x) xs)
       Just?))
;;; end ;;;

;;; 转换 ;;;
(define/match1/contract reverse
  (-> Array? Array?)
  [(Array xs ...)
   (->> (base::reverse xs)
        list->array)])

(define/curry/contract (sort-by f xs)
  (-> (-> any/c any/c boolean?)
      Array?
      Array?)
  (match-define (Array as ...) xs)
  (->> (base::sort as f)
       list->array))

(define/contract sort
  (-> Array? Array?)
  (sort-by <))

(define/curry/contract (filter-map f xs)
  (-> (-> any/c (Maybe/c any/c))
      Array?
      Array?)
  (->> (array->list xs)
       (inner::filter-map f it (list))
       list->array))

(define/curry/contract (partition f xs)
  (-> (-> any/c boolean?)
      Array?
      (cons/c Array? Array?))
  (inner::partition f xs))

(define/curry/contract (split-at n xs)
  (-> exact-nonnegative-integer?
      Array?
      (cons/c Array? Array?))
  (define m (min (length xs) n))
  (match-let ([(Array ys ...) xs])
    (define-values (as bs) (list::split-at ys m))
    (cons (list->array as)
          (list->array bs))))

(define/curry/contract (span f xs)
  (-> (-> any/c boolean?)
      Array?
      (cons/c Array? Array?))
  (inner::span f xs (array)))

(define/curry/contract (group-by f xs)
  (-> (-> any/c any/c boolean?)
      Array?
      (Array/c Array?))
  (inner::group-by f xs empty))

(define/curry/contract (group xs)
  (-> Array?
      (Array/c Array?))
  (group-by = xs))

(define/curry/contract (array-adjust f i xs)
  (-> (-> any/c any/c)
      exact-nonnegative-integer?
      (Array/c any/c)
      (Array/c any/c))
  (match-define (Array ys ...) xs)
  (define l (base::length ys))
  (if (>= i l)
      xs
      (->> (base::list-ref ys i)
           f
           (list::list-set ys i it)
           list->array)))

(define/curry/contract (array-update i x xs)
  (-> exact-nonnegative-integer?
      any/c
      (Array/c any/c)
      Array?)
  (define len (length xs))
  (if (>= i len)
      xs
      (match xs
        [(Array xs ...)
         (->> (list::list-set xs i x)
              list->array)])))
;;; end ;;;

;;; 解构 ;;;
(define/curry/contract (array-get i xs)
  (-> exact-nonnegative-integer?
      Array?
      (Maybe/c any/c))
  (match xs
    [(Array) nothing]
    [(Array xs ...)
     (let ([xs-len (base::length xs)])
       (if (>= i xs-len)
           nothing
           (->> (base::list-ref xs i)
                ->maybe)))]))

(define/curry/contract (array-index x xs)
  (-> Eq?
      (Array/c Eq?)
      (Maybe/c exact-nonnegative-integer?))
  (match xs
    [(Array xs ...)
     (->> (list::index-of xs x)
          ->maybe)]))

(define/curry/contract (foldl f acc xs)
  (-> (-> any/c any/c any/c)
      any/c
      (Array/c any/c)
      any/c)
  (match xs
    [(Array xs ...)
     (base::foldl f acc xs)]))

(define/curry/contract (foldr f acc xs)
  (-> (-> any/c any/c any/c)
      any/c
      (Array/c any/c)
      any/c)
  (match xs
    [(Array xs ...)
     (base::foldr f acc xs)]))
;;; end ;;;

;;; 一些宏、语法 ;;;
(define-syntax (for/array stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([acc empty])
           clauses
           pre-body ...
           (let ([x post-body ...])
             (<:> x acc))))]))

(define-syntax (for*/array stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(let-values ([(acc k)
                       (for*/fold/derived original
                         ([acc empty] [k empty])
                         clauses
                         pre-body ...
                         (let ([xs post-body ...])
                           (values (++ acc xs) xs)))])
           acc))]))
;;; end ;;;
