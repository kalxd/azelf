#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Functor}

@defidform[#:kind "接口"
				  gen:Funtor]{
最小实现@racket[map]。
}

@defproc[(Functor? [a any/c]) boolean?]

@defthing[Functor/c contract?]

@defproc*[([(map [f (-> any/c any/c)] [fa Functor?]) Functor?]
		   [(<$> [f (-> any/c any/c)] [fa Functor?]) Functor?])]{
映射函数。

@examples[
#:eval sb
(map add1 '(1 2 3))
(map add1 (Just 1))
(map add1 (Right 1))
]

}

@defproc*[([(<$ [a any/c] [fa Functor?]) Functor?]
		   [($> [fa Functor?] [a any/c]) Functor?])]{
忽略@code{fa}状态值，并将它的状态替换为@code{a}。

@examples[
#:eval sb
($> (Just 1) 2)
(<$ 3 (Just -1))
($> nothing 3)
]
}
