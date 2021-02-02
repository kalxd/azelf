#lang scribble/manual

@require[@for-label[azelf]]

@title{Eq}

相等比较类型类。

@defidform[gen:Eq]{
最小实现@racket[=]。
}

@defproc[(Eq? [a any/c]) boolean?]

@defthing[Eq/c contract?]

@defproc[(= [a Eq?] [b Eq?]) boolean?]{
比较两数是否相等。之所以取名@racket[=]，在于@racket[=]、@racket[==]皆被占用，@racket[=]与@racket[/=]字数上还能统一。

@codeblock{
(= 1 1) ;; #t
(= "hello" "hello") #t
}
}

@defproc[(/= [a Eq?] [b Eq?]) boolean?]{
@racket[=]的反值。
}

@defproc[(=* [a Eq?] [b Eq?] ...) boolean?]{
比较多个值是否相等。

@codeblock{
(=* 2 2 2 2) ; #t
(=* #\A #\A #\A) ; #t
}
}
