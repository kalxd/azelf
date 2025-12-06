#lang scribble/manual

@require[(for-label azelf)]

@title{option}

@racket[Option]代表为空的类型。

跟Rust等支持@bold{和类型}不一样，@racket[Option]仅仅是一个联合别名：@racket[(define-type (a) (U #f a))]。

也就是说，它不支持嵌套：

@racketblock[
  (:type (Option (Option Real)))
]

结果：

@racketresult[
(U False Real)

[can expand further: Real]
]

@defproc[(option/map
    [ma (Option a)]
    [f (-> a b)])
    (Option b)]{
类似Result::map和Result::chain，因为@racket[Option]不可嵌套，
所以@racket[a]既可以是非空任意值，也可以是任意的@racket[(Option a)]。
}
