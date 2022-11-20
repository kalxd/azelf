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

@section[#:tag "monad-do"]{Monad的do记法}

@defform[(monad/do 表达式 ...+)
                   #:grammar
                   [(表达式 普通表达式
                          赋值表达式
                          中断表达式
                          副作用表达式)
                    (赋值表达式 (code:line)
                             (let id = expr)
                             (id <- expr))
                    (中断表达式 (code:line)
                             (break 任意值))
                    (副作用表达式 (code:line)
                               (! 表达式 ...+))]]{

@examples[
#:eval sb

(monad/do
  (n <- (list 1 2))
  (list n n))

(monad/do
  (n <- (list 1 2))
  (! (when (= n 2) (break (list 20 20))))
  (list n n))
]
}
