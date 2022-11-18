#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "monad"]{Monad（单子）}

@section[#:tag "monad-typeclass"]{Monad定义}

@defidform[#:kind "接口" gen:Monad]{
Monad接口。

最小实现@racket[monad:bind]。
}

@defproc[(monad:bind [f (-> a Monad?)] [ma Monad?]) Monad?]{
即@racket[>>=]。
}

@defproc[(Monad? [a any/c]) boolean?]{
是否@racket[gen:Monad]实例。
}

@section[#:tag "monad-f"]{Monad操作}

@defproc*[([(>>= [f (-> a Monad?)] [ma Monad?]) Monad?]
           [(=<< [ma Monad?] [f (-> a Monad?)]) Monad?])]{
@examples[
#:eval sb

(>>= (list 1 2 3) list)
(>> (list 1 2 3) (list 10 20))
(=<< (lambda (x) (list x x x )) (list 11 12 13 14))
]
}

@defproc*[([(>> [ma Monad?] [mb Monad?]) Monad?]
           [(<< [mb Monad?] [ma Monad?]) Monad?])]{

@examples[
#:eval sb

(<< (list 1 2) (list 11 12 13 14))
(>> (list 1 2) (list 11 12 13 14))
]
}
