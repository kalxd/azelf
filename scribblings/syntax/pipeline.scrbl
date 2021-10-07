#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "pipeline"]{管道与匿名函数}

@section{匿名函数与it}

在一些小众语言中可能会见过@code{it}关键字，它指代该条语句为匿名函数：

@codeblock|{
(+ 1 it)
(lambda (it) (+ 1 it))
}|

以上两条代码可视为等价，换言之，第一条可展开为第二条语句。需要注意的是，@code{it}仅能用在特定的语法中，不能随处使用。

@examples[
#:eval sb
(+ 1 it)
]

@section{管道语法}

@defform[(->> 值 管道 ...)
		 #:grammar
		 [(值 任何表达式)
		  (管道 it表达式
		  	   副作用表达式
			   中断表达式)
		  (it表过式 普通表达式
		  		   带有it关键词表达式)
		  (副作用表达式 (! 表达式))
		  (中断表达式 (break 任何值))]]{
创建一个管道，@racket[值]依次流经每个@racket[管道]。

@examples[
#:eval sb
(->> 1
     (+ it 1) ;; 1 + 1 = 2
     (+ it it)) ;; 2 + 2 = 4
]

@racket[->>]还可以接受普通的单参数函数。

@examples[
#:eval sb
(->> 1
     add1 ;; 2
     (+ it 10)) ;; 2 + 10 = 12
]

同时它还能提前中断。

@examples[
#:eval sb
(->> 1
	 (when (= it 1) (break 20))
	 add1)
]
}

@section{组合函数}

@defform[(>-> 管道 ...+)
		 #:grammar
		 [(管道 普通函数
		 	   带有it关键字函数
			   副作用区块)
		  (副作用区域 (! 普通函数或带有it关键字函数))]]{
类似@racket[->>]，不同在于它只接受函数，并返回一个新的组合函数，相当于@racket[compose]高级版。

@examples[
#:eval sb
(define f (>-> add1 (+ it it)))

(f 1)
(f 2)
]

@racket[副作用区域]可以让你做些小动作，而不改变数值。

@examples[
#:eval sb
(define f
  (>-> add1
   (! (displayln it)
      (+ it 10))
   add1))

(f 1)
(f 2)
]
}

@defform[(<-< 管道 ...+)]{
@racket[>->]参数反向版本，使用方式完全一致。
}
