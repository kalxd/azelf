#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Either}

@bold{异常}的安全类型。

@section{Either类型定义}

@defproc*[([(Left? (a any/c)) boolean?]
		   [(Right? (a any/c)) boolean?])]

@defproc[(Either/c (a any/c) (b any/c))
		  contract?]

@defproc*[([(Left (a any/c)) (Either/c a any/c)]
		   [(Right (a any/c)) (Either/c any/c a)])]{
Either值构造器，同样适用于@racket[match]。

@examples[
#:eval sb
(define (right-add1 x)
  (match x
    [(Right x) (add1 x)]
    [(Left _) 0]))

(right-add1 (Right 1))
(right-add1 (Left 1))
]
}

@section{Either操作函数}

@defproc[(either/catch [action (-> any/c)])
		 (Either/c exn:fail? any/c)]{
自动捕获@code{action}异常，出现异常时，返回@racket[(Left exn:fail)]，反之，得到@code{action}执行结果，以@racket[Right]包装后返回。

@examples[
#:eval sb
(either/catch (lambda () (/ 1 0)))
(either/catch (lambda () (/ 1 1)))
]
}

@defproc[(either->maybe [either (Either/c any/c any/c)]) (Maybe/c any/c)]{
Either转换为Maybe：@racket[Right]转成@racket[Just]；@racket[Left]转成@racket[nothing]。

@examples[
#:eval sb
(either->maybe (Right 1))
(either->maybe (Left 1))
]
}
