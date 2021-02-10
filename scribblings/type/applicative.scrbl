#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Applicative}

@defidform[#:kind "接口"
		   gen:Applicative]{
最小实现@racket[ap]。

不取名@code{apply}，在于@racket[apply]已定义，并且与@racket[ap]含义并不相同。
}

@defproc[(Applicative? [a any/c]) boolean?]

@defthing[Applicative/c contract?]

@defproc[(ap [fa Applicative?] [a any/c]) Applicative?]{

@examples[
#:eval sb
(ap (Just add1) (Just 1))
]
}

@defproc*[([(<* [fa Applicative?] [fb Applicative?]) Applicative?]
		   [(*> [fa Applicative?] [fb Applicative?]) Applicative?])]{
返回对应的状态值。

@examples[
#:eval sb
(<* (Just 1) (Just 2))
(<* nothing (Just 2))

(*> (Just 1) (Just 2))
(*> (Just 1) nothing)
]
}

@defproc*[([(lift2 [f (-> any/c any/c any/c)]
				   [a Applicative?]
				   [b Applicative?])
				   Applicative?]
		   [(lift3 [f (-> any/c any/c any/c any/c)]
		  		   [a Applicative?]
				   [b Applicative?]
				   [c Applicative?])
				   Applicative?]
		   [(lift4 [f (-> any/c any/c any/c any/c any/c)]
		   	   [a Applicative?]
				   [b Applicative?]
				   [c Applicative?]
				   [d Applicative?])
				   Applicative?]
		   [(lift5 [f (-> any/c any/c any/c any/c any/c any/c)]
		   	   [a Applicative?]
				   [b Applicative?]
				   [c Applicative?]
				   [d Applicative?]
				   [e Applicative?])
				   Applicative?])]{
提升普通方法@code{f}。

@examples[
#:eval sb
(lift2 + (Just 1) (Just 2))
(lift2 + (Right 1) (Right 1))
]

}
