#lang scribble/manual

@require[(for-label azelf)]

@title{Nullable}

不同于@racket[Option]的扁平的union，@racket[Nullable]允许多层嵌套。

该数据结构常用于对接第三方接口，如果第三方接口包含nil等字样空值，便可以用该数据结构包装。

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

@defproc*[([(some? [ma (Nullable a)]) Boolean]
           [(nil? [ma (Nullable a)]) Boolean])]{
判断是否@racket[nil]或@racket[some]。
}

@defproc[(nullable/map [ma (Nullable a)] [f (-> a b)]) (Nullable b)]{
Functor::fmap。
}

@defproc[(nullable/chain [ma (Nullable a)] [f (-> a (Nullable b))]) (Nullable b)]{
Monad::bind
}

@defproc*[([(nullable/unwrap-exn [ma (Nullable a)] [e exn]) a]
           [(nullable/unwrap-error [ma (Nullable a)] [msg String]) a]
           [(nullable/unwrap [ma (Nullable a)]) a])]{
强制取值。
}

@defproc[(nullable/filter-map [xs (Listof a)] [f (-> a (Nullable b))]) (Listof b)]{
过滤数组。
}

@defproc[(nullable/cat-somes [xs (Listof (Nullable a))]) (Listof a)]{
只取出some值。
}

@defproc*[([(nullable->option [ma (Nullable a)]) (Option a)]
           [(option->nullable [ma (Option a)]) (Nullable a)])]{
@racket[Nullable]和@racket[Option]相对转化。
}

@defform[(do/nullable? do语句 ...)
         #:grammar
         [(do语句 (code:line)
                  绑定语句
                  赋值语句)
          (赋值语句 (define datum ...))
          (绑定语句 (identifier <- datum))]]{
类似于@racket[do?]。
}
