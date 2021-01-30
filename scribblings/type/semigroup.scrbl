#lang scribble/manual

@require[@for-label[azelf]]

@title{Semigroup}

@defthing[gen:Semigroup any/c]{
Semigroup接口，该接口必须实现@racket[mappend]。
}

@defproc[(Semigroup? [any any/c]) boolean?]

@defthing[Semigroup/c contract?]

@defproc*[([(mappend [a Semigroup?] [b Semigroup?]) Semigroup?]
		   [(<> [a Semigroup?] [b Semigroup?]) Semigroup?])]{
“拼接”@code{a}和@code{b}。

@codeblock{
(<> "hello" "world")
; "helloworld"
(<> (list 1 2) (list 3 4))
; (list 1 2 3 4)
}

目前而言，Racket原始数字类型还不是半群（有两种实现方式，目前未实现）。
已实现类型如下：

@itemlist[
@item{@racket[list?]}
@item{@racket[string?]}
@item{@racket[vector?]}
@item{@racket[procedure?]}
]

}
