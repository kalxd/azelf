#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{Monad}

@section{Monad接口}

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

@section{do记法}

垂涎多时的功能，有了它才更像个命令式。

@defidform[#:kind "关键字"
			<-]{
monad的@racket[bind]操作，可以拿到只关心的状态。

@examples[
#:eval sb
(>>= (Just 1) (lambda (x) (Just (add1 x))))

(monad-do
  (x <- (Just 1))
  (Just (add1 x)))
]

以上两者写法等价，明显可以看到@racket[monad-do]更加简洁、直观。
}

@defform[#:literals [<- let]
		 (monad-do clause ...+)
		 #:grammar
		 ([clause monad-expr (code:line do-clause clause)]
		  [do-clause [pattern <- monad-expr]
		  			 clause
					 (let ([id expr] ...))])]{
Haskell的do记法。

@examples[
#:eval sb
(monad-do
  (x <- (Just 1))
  (y <- (Just 2))
  (Just (+ x y)))
]

@racket[monad-do]允许自定义新变量，Haskell的@code{let}可能对应Racket的@racket[let*]，此处使用的是@racket[let]，需要调用前面的变量，可以写多个@code{let}。

@examples[
#:eval sb
(monad-do
  (let ([x 1]
        [y 2]))
  (let ([x (+ x y)]))
  (Just x))
]

@racket[monad-do]只关心我们关心的状态，遇到不关心的状态时，会自断中断后面所有操作。

@examples[
#:eval sb
(monad-do
  (x <- (Just 1))
  (displayln x)
  (y <- nothing)
  (displayln y)
  (Just (+ x y)))
]
}
