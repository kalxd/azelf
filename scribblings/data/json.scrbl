#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf json))

@title{JSON}

提供稍微便利的json操作写法。

@defidform[#:kind "接口" gen:ToJSON]{
转成json的接口，需最小实现@racket[->json]。
}

@defproc[(->json [a gen:ToJSON]) jsexpr?]{
将可转的数据，转成@racket[jsexpr?]。
}

@defproc[(json->string [a gen:ToJSON]) string?]{
将@racket[a]转化成@racket[string?]。

@examples[
#:eval sb

(json->string "hello")
(json->string 1)
(json->string (list 1 (list "hello") "world"))
]
}

@defproc[(json->byte [a gen:ToJSON]) bytes?]{
将@racket[a]转化成@racket[bytes?]。

@examples[
#:eval sb

(json->byte "hello")
(json->byte (list 1 (list "hello") "world"))
]

}
