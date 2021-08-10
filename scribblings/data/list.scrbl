#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf json))

@title[#:tag "list"]{列表}

@section{合并其他列表}

@defproc[(zip [xs (listof any/c)] [ys (listof any/c)]) (listof pair?)]{
以@racket[cons]合并。

@examples[
#:eval sb

(zip '(1 2 3 4 5) '(a b c))
]
}
