#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title{关键字}

@racket[azelf]提供了新的关键字，进一步拓展了racket语法。

@defidform[#:kind "关键字" it]{
@racket[it]提代被隐藏的参数，在一些宏中，为了书写方便，特意隐藏参数，如果需要使用它，可以用@racket[it]代替，具体例子见@racket[->>]。
}

@defidform[#:kind "关键字" break]{
跳了当前逻辑，实现提前中断。详见@racket[maybe/do]。
}
