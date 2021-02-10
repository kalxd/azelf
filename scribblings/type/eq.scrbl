#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Eq}

相等比较类型类。

@defidform[#:kind "接口"
			gen:Eq]{
最小实现@racket[=]。
}

@defproc[(Eq? [a any/c]) boolean?]

@defthing[Eq/c contract?]

@defproc[(= [a Eq?] [b Eq?]) boolean?]{
比较两数是否相等。

@examples[
#:eval sb
(= 1 1)
(= "hello" "hello")
]
}

@defproc[(/= [a Eq?] [b Eq?]) boolean?]{
@racket[=]的反面。
}

@defproc[(=* [a Eq?] [b Eq?] ...) boolean?]{
比较多个值是否相等。

@examples[
#:eval sb
(=* 2 2 2 2)
(=* #\A #\A #\A)
]
}
