#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "nil"]{优雅地处理空值}

@section{do记法}

很多场合都会产生空值，racket中的空值即为@racket[#f]。虽然已经有了@racket[and]和@racket[or]，但依然感觉不便，于是借鉴Haskell的do记法，自定义了处理空值的@racket[nil/do]语法。

@defform*[((nil/do (let [id expr] ...+) body ...+)
			(nil/do (id <- expr) body ...+)
			(nil/do (break-when expr) body ...+)
			(nil/do (break-unless expr) body ...+)
			(nil/do (! expr) body ...+)
			(nil/do expr ...))]{
如果了解过Haskell的Maybe及Monad的do-notation，那么对这些语法一定不陌生。

绑定表达式：

@examples[
#:eval sb

(nil/do
  (a <- 1)
  (displayln a)
  (b <- #f)
  (displayln b)) ; 这一条是不会打印出来的。
]

定义新变量

@examples[
#:eval sb

(nil/do
  (let ([a 1] [b #f]))
  (displayln b)
  (displayln a)
  a)
]

提交中断：

@examples[
#:eval sb

(nil/do
  (a <- 1)
  (break-when (= a 1) 10)
  a)
]

无论如何都不会中断：

@examples[
#:eval sb

(nil/do
  (a <- 1)
  (! (displayln a))
  (! #f)
  a)
]

}

@section{自动处理空值管道}

@defform[(nil/->> a f ...)]{
类似于@racket[->>]，不同在于@racket[nil/->>]自动处理nil，其中任一函数返回nil，自动结束并返回。
同样的，它支持@racket[it]关键字。

@examples[
#:eval sb

(nil/->> 1
         (+ it it)
         add1)

(nil/->> 1
         (+ it it)
         (lambda (x) #f)
         (+ it it))
]
}
