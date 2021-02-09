#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Apply}

@defidform[#:kind "接口"
		   gen:Apply]{
最小实现@racket[ap]。

不取名@code{apply}，在于@racket[apply]已定义，并且与@racket[ap]含义并不相同。
}

@defproc[(Apply? [a any/c]) boolean?]

@defthing[Apply/c contract?]

@defproc[(ap [fa Apply?] [a any/c]) Apply?]{

@examples[
#:eval sb
(ap (Just add1) (Just 1))
]
}

@defproc*[([(<* [fa Apply?] [fb Apply?]) Apply?]
		   [(*> [fa Apply?] [fb Apply?]) Apply?])]{
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
				   [a Apply?]
				   [b Apply?])
				   Apply?]
		   [(lift3 [f (-> any/c any/c any/c any/c)]
		  		   [a Apply?]
				   [b Apply?]
				   [c Apply?])
				   Apply?]
		   [(lift4 [f (-> any/c any/c any/c any/c any/c)]
		   	   [a Apply?]
				   [b Apply?]
				   [c Apply?]
				   [d Apply?])
				   Apply?]
		   [(lift5 [f (-> any/c any/c any/c any/c any/c any/c)]
		   	   [a Apply?]
				   [b Apply?]
				   [c Apply?]
				   [d Apply?]
				   [e Apply?])
				   Apply?])]{
提升普通方法@code{f}。

@examples[
#:eval sb
(lift2 + (Just 1) (Just 2))
(lift2 + (Right 1) (Right 1))
]

}
