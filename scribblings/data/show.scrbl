#lang scribble/manual

@(require "../run.rkt"
          (for-label azelf))

@title[#:tag "show"]{Show(生成可读字符串)}

@section[#:tag "show-typeclass"]{Show定义}

@defidform[#:kind "接口" gen:Show]{
基本类型定义。最小实现为@racket[show:show]。
}

@defproc[(show:show [a Show?]) string?]{
@racket[gen:Show]最小实现。

@codeblock{
(struct User [name]
  #:methods gen:Show
  [(define/match (show:show user)
    [((User name)) (string-append "User: " name)])])
}
}

@defproc[(Show? [a any/c]) boolean?]{
}

@section[#:tag "show-function"]{Show一般操作}

@defproc[(show [a Show?]) string?]{
@racket[show:show]别名。
}
