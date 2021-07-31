#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "nil/do"]{空值的do记法}

很多场合都会产生空值，racket中的空值即为@racket[#f]。虽然已经有了@racket[and]和@racket[or]，但依然感觉不便，于是借鉴Haskell的do记法，自定义了处理空值的@racket[nil/do]语法。

@defform*[((nil/do (let [id expr] ...+) body ...+)
			(nil/do (id <- expr) body ...+)
			(nil/do (break expr) body ...+)
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
  (break 10)
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
