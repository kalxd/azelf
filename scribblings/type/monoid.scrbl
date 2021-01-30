#lang scribble/manual

@require[@for-label[azelf]]

@title{Monoid}

@defthing[gen:Monoid any/c]{
必须实现@racket[mempty]。
}

@defproc[(Monoid? [any any/c]) boolean?]

@defthing[Monoid/c contract?]

@defproc[(mempty [a Monoid?]) Monoid?]{

Racket没有真正的泛型，故需要填入一个实例（并不是类型）才行，所以@italic{a}可以是对应类型的任意值。

@codeblock{
(mempty "sb")
; ""

(mempty (list 1 2))
; '()
}

已实现类型：

@itemlist[
@item{ @racket[string?] }
@item{ @racket[list?] }
@item{ @racket[vector?] }
@item{ @racket[procedure?] }
]
}
