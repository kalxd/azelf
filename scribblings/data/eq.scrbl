#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf json))

@title[#:tag "eq"]{Eq（相等比较）}

@section[#:tag "eq-typeclass"]{Eq定义}

@defidform[#:kind "接口" gen:Eq]{
@bold{相等}接口。
}

@defproc[(eq:= [a Eq?] [b Eq?]) boolean?]{
@racket[gen:Eq]最小实现。
两者是否相等。
}

@defproc[(Eq? [a any/c]) boolean?]{
是否是@racket[gen:Eq]实例。
}

@section[#:tag "eq-f"]{Eq操作}

@defproc[(= [a Eq?] [b Eq?]) boolean?]{
判断相等。
}

@defproc[(/= [a Eq?] [b Eq?]) boolean?]{
判断不相等。
}
