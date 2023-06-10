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

@section[#:tag "json-ext"]{JSON扩展方法}

@defproc[(json/->primitive [value jsexpr?]) any/c]{
将JSON转化为原始类型数据。

@examples[
#:eval sb

(json/->primitive (list 1 2 3))
(json/->primitive 'null)
(json/->primitive (hash 'a (list 1 2 3) 'b (hash 'name 2 'age 4)))
]
}

@defproc[(json/read [port input-port?]) any/c]{
从@racket[input-port?]中读取出json。
}
