#lang scribble/manual
@require[@for-label[azelf
                    racket]]

@title{Maybe}

@bold{Nil}的安全类型，在类型上避免空指针错误。

@section{Maybe类型定义}

@defproc*[([(Just? [a any/c]) boolean?]
		   [(Nothing? [a any/c]) boolean?])]{
检测是否Just及Nothing。
}

@defproc[(Maybe/c [a any/c])
				  contract?]{
“泛型”maybe容器，检测@racket[Just]是否为a。
}

@defproc*[([(Just [a any/c]) (Maybe/c a)]
		   [(Nothing) (Maybe/c any/c)])]{
Maybe构造器。同时适用于@racket[match]。

@codeblock{
(define/match (my/add1 x)
  [(Just x) (add1 x)]
  [(Nothing) 0])

(my/add1 (Just 1))
;; 2
(my/add1 nothing)
;; 0
}

}

@defthing[nothing (Maybe/c any/c) #:value (Nothing)]{
@racket[Nothing]本身就是特定值，不需要额外再构造一次。
}

@section{操作}

@defproc[(maybe [def any/c]
				[f (-> any/c any/c)]
				[value (Maybe/c any/c)])
		  any/c]{
同Haskell的maybe，展开整个Maybe。
}

@defproc[(->maybe [x any/c])
		 (Maybe/c)]{
将任意一个值转化成Maybe。Racket不像其他语言有个特殊的@code{Nil}，它用@racket[#f]表示“空”，
因此该函数仅会将@racket[#f]转化为@racket[nothing]，其他一切都为@racket[Just]。

这引来一个问题，那么我们该如何得到@code{Just #f}呢？我的答案是直接调用@code{(Just #f)}。

@codeblock{
(->maybe 1) ;; (Just 1)
(->maybe 0) ;; (Just 0)
(->maybe #f) ;; nothing
(Just #f) ;; (Just #f)
(->maybe #t) ;; (Just #t)
}

}

@defproc[(maybe-> [x any/c]
				  [f (Maybe/c any/c)])
		 (or/c x any/c)]{
同@racket[maybe]，接受一个默认值@code{x}，如果@code{f}为@racket[Just]，则直接获取@racket[Just]内容，反之以@code{x}代之。

@codeblock{
(maybe-> 1 nothing) ;; 1
(maybe-> 1 (Just 2)) ;; 2
}

}

@defproc[(maybe/catch [action (-> any/c)])
		 (Maybe/c any/c)]{
自动捕获异常。如果@code{action}出现异常，将返回@racket[nothing]，反之，得到@code{action}的执行结果，@racket[Just]包装之后返回。

@codeblock{
(maybe/catch (lambda () (/ 1 0)))
;; 抛出“除零”异常，返回Nothing。

(maybe/catch (lambda () (/ 1 1)))
;; 没有异常，得到(Just 1)
}
}
