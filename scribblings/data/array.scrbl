#lang scribble/manual
@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "array"]{数组}

@racketmodname[azelf]内置的数组，取代了racket的@racket[list]。
整体接口偏向Haskell的Data.List。

@section[#:tag "array-type"]{数组定义}

@defproc[(Array? [a any/c]) boolean?]{
是不是数组。
}

@defproc[(Array/c [a contract?]) contract?]{
@racket[contract]构造器。
}

@defform[(Array pattern ...)]{
数组模式匹配关键字。

@examples[
#:eval sb

(define/match1 f
  [(Array) 'empty]
  [(Array _) 'one]
  [(Array _ _) 'two]
  [(Array a b _ ...) (+ a b)]
  [_ 'fucked])

(f (array))
(f (array 1))
(f (array 1 2))
(f (array 1 2 3 4))
]

这里需要注意，@racket[...]匹配出来的数组结构是@racket[list]，不是@racket[Array?]。

@examples[
#:eval sb

(match (array 1 2 3 4 5)
  [(Array xs ...) xs])
]
}

@section[#:tag "array-contractor"]{构造新数组}

@defproc[(array [x a] ...) (Array/c a)]{
类似@racket[list]，构造出新的@racket[Array?]。

@examples[
#:eval sb
(array)
(array 1 2 3)
]
}

@defproc[(list->array [xs (listof a)]) (Array/c a)]{
@racket[list]转化成@racket[array]。

@examples[
#:eval sb
(list->array (list 1 2 3))
]
}

@defproc[(repeat [n (or/c zero? positive?)] [a a]) (Array/c a)]{
@examples[
#:eval sb
(repeat 10 "hello")
]
}

@defproc[(range [start number?] [end number?]) (Array/c number?)]{
@examples[
#:eval sb
(range 1 10)
]
}

@defthing[empty (Array/c a)]{
空数组。

@codeblock{
(= (array) empty)
}
}

@defproc[(singleton [x a]) (Array/c a)]{
生成一个单元素的数组。

@codeblock{
(= (array x) (singleton x))
}
}

@section[#:tag "array-concat"]{数组合并}

@defproc[(++ [xs (Array/c a)] [ys (Array/c a)]) (Array/c a)]{
数组拼接。

@examples[
#:eval sb
(++ (array 1 2 3) (array 3 2 1))
]
}

@defproc[(: [x a] [xs (Array/c a)]) (Array/c a)]{
相当于@racket[cons]。

@examples[
#:eval sb

(: 1 (array))
(: 1 (array 2 3))
]
}

@defproc[(<:> [x a] [xs (Array/c a)]) (Array/c a)]{
往数组后面追加元素，相当于其他语言的push。

@examples[
#:eval sb
(<:> 1 (array))
(<:> 1 (array 2 3))
]
}

@defproc[(zip-with [f (-> a b c)] [xs (Array/c a)] [ys (Array/c b)]) (Array/c c)]{
合并两条数组。
}

@defproc[(zip [xs (Array/c a)] [ys (Array/c b)]) (Array/c (cons/c a b))]{
合并两条数组。
}

@section[#:tag "array-property"]{列表属性}

@defproc[(length [xs (Array/c any/c)]) exact-nonnegative-integer?]{
数组长度。

@examples[
#:eval sb
(length (array))
(length (array 1 2 3))
]
}

@defproc[(empty? [xs (Array/c any/c)]) boolean?]{
是否是空数组。

@examples[
#:eval sb
(empty? (array))
(empty? (array 1))
]
}

@section[#:tag "array-subarray"]{子列表操作}

@defproc*[([(head [xs (Array/c a)]) (Maybe/c a)]
           [(tail [xs (Array/c a)]) (Maybe/c a)])]{
@examples[
#:eval sb
(head (array))
(head (range 10 20))

(tail (array))
(tail (range 10 20))
]
}

@defproc*[([(take-while [f (-> a boolean?)] [xs (Array/c a)]) (Array/c a)]
           [(drop-while [f (-> a boolean?)] [xs (Array/c a)]) (Array/c a)])]{
@examples[
#:eval sb
(take-while even? (array 2 4 7 8 10))
(drop-while even? (array 2 4 7 8 10))
]
}

@defproc*[([(take [n exact-integer?] [xs (Array/c a)]) (Array/c a)]
           [(take-right [n exact-integer?] [xs (Array/c a)]) (Array/c a)]
           [(drop [n exact-integer?] [xs (Array/c a)]) (Array/c a)]
           [(drop-right [n exact-integer?] [xs (Array/c a)]) (Array/c a)])]{
@examples[
#:eval sb
(take 10 (array 1 2 3 4))
(drop -1 (array 1 2 3 4))
]
}

@defproc*[([(filter [f (-> a boolean?)] [xs (Array/c a)]) (Array/c a)]
           [(reject [f (-> a boolean?)] [xs (Array/c a)]) (Array/c a)])]{
过滤列表。

@examples[
#:eval sb
(filter even? (array 1 2 3 4))
(reject even? (array 1 2 3 4))
]
}

@section[#:tag "array-search"]{数组查找}

@defproc[(find [f (-> a boolean?)] [xs (Array/c a)]) (Maybe/c a)]{
@examples[
#:eval sb
(find (= 1) (array 3 1 2))
(find (= (Just 1)) (array nothing nothing (Just 2)))
(find (= nothing) (array nothing nothing (Just 2)))
]
}

@defproc[(member? [a Eq?] [xs (Array/c Eq?)]) (Maybe/c Eq?)]{
@examples[
#:eval sb
(member? 1 (array))
(member? 2 (array 1 2 3))
]
}

@section[#:tag "array-transform"]{数组转换}

@defproc[(reverse [xs (Array/c a)]) (Array/c a)]{
反转数组。
}

@defproc*[([(sort-by [f (-> a a boolean?)] [xs (Array/c a)]) (Array/c a)]
           [(sort [xs (Array/c a)]) (Array/c a)])]{
@racket[sort]默认升序排序。

@codeblock{
(define sort (sort-by <))
}
}

@defproc[(filter-map [f (-> a (Maybe/c b))] [xs (Array/c a)]) (Array/c b)]{
即要@racket[filter]又要@racket[map]。

@examples[
#:eval sb
(define (f x)
  (if (< x 0)
      nothing
      (Just (add1 x))))

(filter-map f (array -1 1 -2 2))
]
}

@defproc[(partition [f (-> a boolean?)] [xs (Array/c a)]) (cons (Array/c a) (Array/c a))]{
挑出两堆数组，左组满足@racket[f]条件，右组不足。
}

@defproc[(span [f (-> a boolean?)] [xs (Array/c a)]) (cons/c (array a) (array a))]{
从头寻找满足@racket[f]的元素，直到未满足为止；返回一组满足元素的集合，及剩下元素集合。

@examples[
#:eval sb
(span (λ (x) (< x 3)) (array 1 2 3 4 1 2 3 4))
(span (λ (x) (< x 9)) (array 1 2 3))
]
}

@defproc*[([(group-by [f (-> a a boolean?)] [xs (Array/c)]) (Array/c (Array/c a))]
           [(group [xs (Array/c a)]) (Array/c (Array/c a))])]{
按条件@racket[f]分组。
}

@defproc[(update-at [i exact-nonnegative-integer?] [x a] [xs (Array/c a)]) (Array/c a)]{
更新@racket[i]元素。
}

@section[#:tag "array-destruction"]{数组解构}

@defproc[(at [i exact-nonnegative-integer?] [xs (Array/c a)]) (Maybe/c a)]{
获取第@racket[i] - 1个元素。
@examples[
#:eval sb
(at 0 empty)
(at 10 (array 1 2 3))
(at 1 (array 1 2 3))
]
}

@defproc[(index [a Eq?] [xs (Array/c Eq?)]) (Maybe/c exact-nonnegative-integer?)]{
查看元素在哪个位置。
@examples[
#:eval sb
(index 'a empty)
(index (Just "a") (array nothing (Just "b") (Just "a")))
]
}

@defproc*[([(foldl [f (-> a b a)] [acc a] [xs (Array/c b)]) a]
           [(foldr [f (-> b a a)] [acc a] [xs (Array/c b)]) a])]{
}

@section[#:tag "array-syntax"]{数组特有语法}

@defform*[((for/array (条件 ...) (代码体 ...))
           (for*/array (条件 ...) (代码体 ...)))]{
类似@racket[for/list]、@racket[for*/list]，结果产生的是@racket[array]。

@examples[
#:eval sb
(for/array ([n (in-range 1 10)]) (+ n n))
(for*/array ([n (in-range 1 10)] [m (array "hello" "world")]) (array n m))
]
}
