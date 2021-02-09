#lang scribble/manual

@(require "./run.rkt"
		  (for-label azelf))

@title{友好的语法}

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

@defidform[#:kind "关键字"
			it]{
仅能在几种语法中使用的关键字。
}

@defform[(->> value pipe ...)
		 #:grammar
		 [(value any/c)
		  (pipe procedure?)]]{
创建一个管道，@code{value}依次流经每个管道（@code{pipe}）。

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

}

@defform[(<-< f ...+)
		 #:grammar
		 [(f procedure?)]]{
@racket[compose]增强版，可以像@racket[compose]一样使用它，同时也允许使用@racket[it]关键字。

@examples[
#:eval sb
(define f (<-< add1 (+ it it)))

(f 1)
(f 2)
]

}

@section{展开列表}

javascript中有@code{const [a, ...as] = xs}写法，用于快速展开列表。

@defform[(define/values (attr ...+) expr)
		 #:contracts ([expr (listof any/c)])]{
有点类似于@racket[define-values]，该函数用于展开一个列表（目前仅对@racket[list]有效）。
同样使用@bold{...}可表示“剩余”部分，不同于javascript，该语法可以使用任何位置，没有任何限制。

@examples[
#:eval sb
(define/values (a b) (list 1 2 3))
a
b
]

@examples[
#:eval sb
(define/values (a ...b) (list 1 2 3))
a
b
]

@examples[
#:eval sb
(define/values (...a b) (list 1 2 3 4))
a
b
]

@examples[
#:eval sb
(define/values (a ...b c) (list 1 2 3 4))
a
b
c
]

}
