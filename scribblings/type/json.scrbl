#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf
		  			 json))

@title{ToJSON}

将一切转化成@racketmodname[json]的能力。

@defidform[#:kind "接口" gen:ToJSON]{
最小实现@racket[->json]。
}

@defproc[(ToJSON? [a any/c]) boolean?]

@defthing[ToJSON/c contract?]

@defproc[(->json [a ToJSON?]) jsexpr?]{
将一个可转化的数据结构，转化成@racket[jsexpr?]。

@examples[
#:eval sb
(->json 1)
(->json (Just 1))
(->json (Just (Just 1)))
(->json nothing)
(->json (Just nothing))
]
}

@defproc[(json->string [a ToJSON?]) string?]{
将一个JSON对象。转化成@racket[string?]。

@examples[
#:eval sb
(json->string 1)
(json->string (Just 1))
(json->string nothing)
]
}

@defproc[(json->output [port output-port?] [a ToJSON?]) void?]{
将@racket[ToJSON?]写入@racket[output-port?]。

@examples[
#:eval sb

(require racket/port)
(call-with-output-string
  (lambda (port) (json->output port (Just (Just "hello")))))
]
}
