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
