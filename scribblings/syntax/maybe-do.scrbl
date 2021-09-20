#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{maybe/do}

@defform[(maybe/do expr ...+)
		 #:grammar
		 [(expr 普通表达式
				赋值表达式
				中断表达式
				副作用表过式)
		  (赋值表达式 (code:line)
		  			(let id = expr)
					(id <- expr))
		  (中断表达式 (code:line)
		  			(break)
		  			(break 值))
		  (副作用表过式 (code:line)
		  			  (! 任意表达式 ...+))]]{
自动处理空值语法，无论何种方式，经过@racket[maybe/do]的结果必然是@racket[maybe?]。

在赋值表达式中，(id <- expr)这种形式，会对expr结果进行封装，如果是普通值（非Maybe），会以@racket[->maybe]包装后再进行相关流程。(let)表达式就是纯粹赋值，没有多余处理流程。

@examples[
#:eval sb

(define (f n)
  (and (< n 5) n))

(maybe/do
  (a <- (f 1))
  (b <- (f 2))
  (+ a b))

(maybe/do
  (a <- (f 1))
  (b <- (f 10))
  (+ a b))

(maybe/do
  (let a = (f 1))
  (let b = (f 10))
  (+ a b))
]

@racket[break]可以中断操作，提前跳转整个代码块。它可以接受一个参数或无参数：接受一个参数，该参数自动封装成@racket[maybe?]（除非已经是Maybe）；无参数返回@racket[nothing]。

@examples[
#:eval sb

(maybe/do
  (a <- 1)
  (when (= a 1) (break 10))
  (! (displayln a))
  (add1 a))

(maybe/do
  (a <- 1)
  (unless (= a 10) (break))
  a)
]

@racket[!]仅仅用来处理副作用的代码。需要IO处理（如打印、读写文件），又不想中断整个操作，那么可以使用该形式表过式。不管它最终结果是什么，都不会中断后面代码！

@examples[
#:eval sb

(define (fmt x)
  (displayln x)
  #f)

(maybe/do
  (a <- "hello")
  (b <- "world")
  (! (fmt a)
     (fmt b))
  (void))

(maybe/do
  (a <- "hello")
  (b <- "world")
  (fmt a)
  (fmt b)
  (void))
]

}
