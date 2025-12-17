#lang scribble/manual

@require[(for-label azelf)]

@title{Nullable}

不同于@racket[Option]的扁平的union，@racket[Nullable]允许多层嵌套。

@codeblock{
           (Nullable (Nullable String))
           }

@defform[#:kind "类型"
         (Nullable a)]{
类似于Haskell的Maybe。
}

@defthing[nil (Nullable a)]{
 类似于其他语言的nil。
}

@defproc[(some [value a]) (Nullable a)]{
类似Just。
}
