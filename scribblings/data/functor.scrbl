#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf json))

@title[#:tag "functor"]{Functor（函子）}

@section{类型类定义}

@defidform[#:kind "接口" gen:Functor]{
Functor接口。
}

@defproc[(functor:map [f (-> a b)] [ma Functor?]) Functor?]{
Functor的最小实现。
}

@defproc[(Functor? [a any/c]) boolean?]{
是不是gen:Functor实例。
}

@section{函子函数}

@defproc[(map [f (-> a b)] [ma Functor?]) Functor?]{
@racket[functor:map]柯里化版本。

@examples[
#:eval sb

(map add1 (list 1 2 3))
(map add1 (hash 1 2 3 4))
]
}

@defproc[(<$> [f (-> a b)] [ma Functor?]) Functor?]{
@racket[map]别名。
}

@defproc[(<#> [ma Functor?] [f (-> a b)]) Functor?]{
@racket[map]参数反转版本。

@codeblock{
(define <#> (flip map))
}
}

@defproc[(<$ [a a] [ma Functor?]) Functor?]{
替换为常量@racket[a]。

@examples[
#:eval sb

(<$ 1 (list 1 2 3 4 4 5))
(<$ 2 (hash 11 12 13 14))
]
}

@defproc[($> [ma Functor?] [a any/c]) Functor?]{
@racket[<$]反转参数版本。
}
