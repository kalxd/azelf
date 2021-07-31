#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))


@title{列表展开语法}

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
