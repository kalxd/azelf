#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf))

@title{Maybe}

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

@defproc[(maybe-map [f (-> any/c any/c)] [a (Maybe/c any/c)]) (Maybe/c any/c)]{
Functor映射。

@examples[
#:eval sb

(maybe-map add1 (Just 1))
(maybe-map add1 nothing)
]
}

@defproc[(maybe-then [f (-> any/c (Maybe/c any/c))] [a (Maybe/c any/c)]) (Maybe/c any/c)]{
Monad的binding。

@examples[
#:eval sb

(define (f x)
  (Just (add1 x)))

(maybe-then f (Just 1))
(maybe-then f Nothing)
]
}

@defproc[(maybe-unwrap [x any/c]
					   [f (Maybe/c any/c)])
					   (or/c x any/c)]{
同@racket[maybe]，接受一个默认值@code{x}，如果@code{f}为@racket[Just]，则直接获取@racket[Just]内容，反之以@code{x}代之。

@examples[
#:eval sb
(maybe-unwrap 1 nothing) ;; 1
(maybe-unwrap 1 (Just 2)) ;; 2
]

}
