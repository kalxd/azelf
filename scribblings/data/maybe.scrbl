#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "maybe"]{Maybe}

@bold{Nil}的安全类型，在类型上避免空指针错误。

@section{Maybe类型定义}

@defproc[(Maybe/c [a any/c])
				  contract?]{
“泛型”maybe容器，检测@racket[Just]是否为a。
}

@defproc*[([(Just [a any/c]) (Maybe/c any/c)]
		   [(Nothing) (Maybe/c any/c)])]{
Maybe构造器。
同时可用于@racket[match]。
}

@defthing[nothing (Maybe/c any/c)]{
已确定的Maybe的值。无须再调用生成。
}

@section{操作}

@defproc[(maybe? [x any/c]) boolean?]{
检测是否Maybe。
}

@defproc[(->maybe [x any/c])
		 (Maybe/c x)]{
将任意一个值转化成Maybe。Racket不像其他语言有个特殊的@code{Nil}，它用@racket[#f]表示“空”，
因此该函数仅会将@racket[#f]转化为@racket[nothing]，其他一切都为@racket[Just]。

这引来一个问题，那么我们该如何得到@code{Just #f}呢？我的答案是直接调用@code{(Just #f)}。

@examples[
#:eval sb
(->maybe 1)
(->maybe 0)
(->maybe #f)
(Just #f)
(->maybe #t)
]
}

@defproc[(maybe-filter [f (-> a boolean?)] [ma (Maybe/c a)]) (Maybe/c a)]{
类似于@racket[filter]，@racket[f]返回@racket[#f]，整个结果就成了@racket[nothing]。

@examples[
#:eval sb

(maybe-filter even? (Just 2))

(define (gt xs)
	(> (length xs) 5))
(maybe-filter gt (Just (array 1 2 3)))
(maybe-filter gt (Just (array 1 2 3 4 5 6)))
]
}

@defproc[(maybe-and [ma (Maybe/c any/c)] [mb (Maybe/c)]) (Maybe/c)]{
类似于@racket[and]。@racket[ma]、@racket[mb]其中之一是@racket[nothing]，结果就是@racket[nothing]，反之返回@racket[mb]。

@examples[
#:eval sb

(maybe-and (Just 1) nothing)
(maybe-and (Nothing) (Just 2))
(maybe-and (Just 1) (Just 2))
]
}

@defproc[(maybe-or [ma (Maybe/c any/c)] [mb (Maybe/c any/c)]) (Maybe/c any/c)]{
类似于@racket[or]。见@racket[maybe-and]。

@examples[
#:eval sb

(maybe-or (Just 1) nothing)
(maybe-or (Nothing) (Just 2))
(maybe-or (Just 1) (Just 2))
]
}

@defproc[(maybe-alt [ma (Maybe/c any/c)] [mb (Maybe/c any/c)]) (Maybe/c any/c)]{
只有当@racket[ma]、@racket[mb]都为@racket[nothing]，结果才为@racket[nothing]。

@examples[
#:eval sb

(maybe-alt (Just 1) (Just 2))
(maybe-alt (Just 1) (Nothing))
(maybe-alt nothing (Just 2))
]
}

@defproc[(maybe-else [a any/c] [f (-> any/c any/c)] [ma (Maybe/c any/c)]) any/c]{
根据条件选择对应的回调函数：@racket[Just]调用@racket[f]；@racket[nothing]则返回@racket[a]。

@examples[
#:eval sb

(maybe-else 10 add1 (Just 1))
(maybe-else 10 add1 nothing)
]
}

@defproc[(maybe-> [x any/c]
				  [f (Maybe/c any/c)])
				  (or/c x any/c)]{
同@racket[maybe]，接受一个默认值@code{x}，如果@code{f}为@racket[Just]，则直接获取@racket[Just]内容，反之以@code{x}代之。

@examples[
#:eval sb
(maybe-> 1 nothing) ;; 1
(maybe-> 1 (Just 2)) ;; 2
]

}

@defproc[(maybe-unwrap [x (Maybe/c y)]) y]{
解包@racket[Maybe]，遇到@racket[nothing]直接抛异常。

@examples[
#:eval sb

(maybe-unwrap (Just 1))
(maybe-unwrap (Just #f))

(maybe-unwrap nothing)
]
}

@defform[#:kind "语法" (maybe-catch expr)
		#:grammar
		[(expr 任何表达式)]]{
自动捕获@racket[expr]错误，如果@racket[expr]无任何错误，返回@racket{(Just expr)}，反之返回@racket[nothing]。

@examples[
#:eval sb

(maybe-catch (* 1 0))

(maybe-catch (/ 1 0))
]
}

@section[#:tag "maybe-do"]{Maybe的do记法}

@defform[(maybe/do expr ...+)
		 #:grammar
		 [(expr 普通表达式
				赋值表达式
				中断表达式
				副作用表过式)
		  (赋值表达式 (code:line)
		  			(let id = expr)
					(id <- expr))
		  (中断表达式 (code:line)
		  			(break)
		  			(break 值))
		  (副作用表过式 (code:line)
		  			  (! 任意表达式 ...+))]]{
自动处理空值语法，无论何种方式，经过@racket[maybe/do]的结果必然是@racket[maybe?]。

在赋值表达式中，(id <- expr)这种形式，会对expr结果进行封装，如果是普通值（非Maybe），会以@racket[->maybe]包装后再进行相关流程。(let)表达式就是纯粹赋值，没有多余处理流程。

@examples[
#:eval sb

(define (f n)
  (and (< n 5) n))

(maybe/do
  (a <- (f 1))
  (b <- (f 2))
  (+ a b))

(maybe/do
  (a <- (f 1))
  (b <- (f 10))
  (+ a b))

(maybe/do
  (let a = (f 1))
  (let b = (f 10))
  (+ a b))
]

@racket[break]可以中断操作，提前跳转整个代码块。它可以接受一个参数或无参数：接受一个参数，该参数自动封装成@racket[maybe?]（除非已经是Maybe）；无参数返回@racket[nothing]。

@examples[
#:eval sb

(maybe/do
  (a <- 1)
  (when (= a 1) (break 10))
  (! (displayln a))
  (add1 a))

(maybe/do
  (a <- 1)
  (unless (= a 10) (break))
  a)
]

@racket[!]仅仅用来处理副作用的代码。需要IO处理（如打印、读写文件），又不想中断整个操作，那么可以使用该形式表过式。不管它最终结果是什么，都不会中断后面代码！

@examples[
#:eval sb

(define (fmt x)
  (displayln x)
  #f)

(maybe/do
  (a <- "hello")
  (b <- "world")
  (! (fmt a)
     (fmt b))
  (void))

(maybe/do
  (a <- "hello")
  (b <- "world")
  (fmt a)
  (fmt b)
  (void))
]

}
