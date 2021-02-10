#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Monad}

@defidform[#:kind "接口"
			gen:Monad]{
最小实现@racket[bind]。
}

@defproc[(Monad? [a any/c]) boolean?]

@defthing[Monad/c contract?]

@defproc[(bind [ma Monad?] [f (-> any/c Monad?)]) Monad?]{
@examples[
#:eval sb
(define inc
  (compose Just add1))

(bind (Just 1) inc)
]
}

@defproc*[([(>>= [ma Monad?] [f (-> any/c Monad?)]) Monad?]
		   [(=<< [f (-> any/c Monad?)]) Monad?])]{
@racket[>>=]是@racket[bind]别名，@racket[=<<]是@racket[bind]参数翻转版。

@examples[
#:eval sb
(define inc
  (compose Just add1))
(>>= (Just 1) inc)
(=<< inc (Just 1))
]
}

@defproc*[([(<=< [g (-> any/c Monad?)] [f (-> any/c Monad?)]) (-> any/c Monad?)]
		   [(>=> [f (-> any/c Monad?)] [g (-> any/c Monad?)]) (-> any/c Monad?)])]{
Monad版的函数组合。先应用@code{f}，最后应用@code{g}。

@examples[
#:eval sb
(define inc
  (compose Just add1))

(define ->just-string
  (compose Just number->string))

((<=< ->just-string inc) 1)
((>=> inc ->just-string) 1)
]
}

@defproc[(join [ma Monad?]) Monad?]{
解包出两层嵌套的Monad。

@examples[
#:eval sb
(join (Just (Just 1)))
(join (list (list 1) (list 2 3)))
]
}
