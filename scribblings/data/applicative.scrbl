#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "applicative"]{Applicative（应用函子）}

@section[#:tag "applicative-typeclass"]{类型类定义}

@defidform[#:kind "接口" gen:Applicative]{
Applicative接口。
}

@defproc[(applicative:ap [fa Applicative?] [fb Applicative?]) Applicative?]{
Applicative最小实现。

@margin-note*{因为racket没有泛型，另一个最小实现pure不需要提供，只是在使用时，需要明确指定。}
}

@defproc[(Applicative? [a any/c]) boolean?]{
是不是gen:Applicative实例。
}

@section[#:tag "applicative-f"]{Applicative操作函数}

@defproc[(<*> [fa Applicative?] [fb Applicative?]) Applicative?]{

@examples[
#:eval sb
(<*> (list (const 1)) (list 1 2 3 4))
]
}

@defproc*[([(<* [fa Applicative?] [fb Applicative?]) Applicative?]
           [(*> [fb Applicative?] [fa Applicative?]) Applicative?])]{
@examples[
#:eval sb
(<* (list 1 2) (list 1 2 3 4))
(*> (list 1 2) (list 1 2 3 4))
]
}
