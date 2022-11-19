#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "ord"]{Ord（大小比较）}

@section[#:tag "ord-typeclass"]{Ord定义}

@defidform[#:kind "接口" gen:Ord]{
@bold{大小}接口。

最小实现@racket[ord:compare]。
}

@defthing[Ordering (or/c 'eq 'lt 'gt)]{
比较结果。
}

@defproc[(ord:compare [a Ord?] [b Ord?]) Ordering]{}

@defproc[(Ord? [a any/c]) boolean?]{
是否@racket[gen:Ord]实例。
}

@section[#:tag "ord-f"]{Ord操作}

@defproc[(compare [a Ord?] [b Ord?]) Ordering]{
@examples[
#:eval sb

(compare 1 1)
(compare "hello" "Hello")
(compare (list 1 2) (list 1 2 3))
]
}

@defproc*[([(> [a Ord?] [b Ord?]) boolean?]
           [(>= [a Ord?] [b Ord?]) boolean?]
           [(< [a Ord?] [b Ord?]) boolean?]
           [(<= [a Ord?] [b Ord?]) boolean?])]{
@examples[
#:eval sb
(> 1 2)
(>= 2 1)
(< (list 1 2) (list 2 1))
(<= (hash) (hash))
]
}

@defproc*[([(min [a Ord?] [b Ord?]) (or/c a b)]
           [(max [a Ord?] [b Ord?]) (or/c a b)])]{
@examples[
#:eval sb

(max 1 1)
(min #\A #\B)
]
}
