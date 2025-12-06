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

@section{常用函数}

@defproc[(option/map
    [ma (Option a)]
    [f (-> a b)])
    (Option b)]{
类似Result::map和Result::chain，因为@racket[Option]不可嵌套，
所以@racket[a]既可以是非空任意值，也可以是任意的@racket[(Option a)]。
}

@defproc[(option/unwrap-exn
          [ma (Option a)]
          [err exn])
         a]{
强行解构@racket[(Option a)]到@racket[a]。

如果@racket[ma]为@racket[#f]，会抛出用户指定的@racket[exn]——也就是@racket[err]参数。
}

@defproc[(option/unwrap-error
          [ma (Option a)]
          [err-msg String])
         a]{
类似@racket[option/unwrap-exn]，自定义错误从@racket[exn]变成@racket[String]。

@racket[#f]抛出@racket[exn:fail:user]。
}

@defproc[(option/unwrap
          [ma (Option a)])
          a]{
类似于Rust的Option::unwrap，遇到@racket[#f]直接抛出@racket[exn:fail:user]。
}

@section{do?语法糖}

类似于Haskell的do记法。

@defform[(do? do语句 ...)
         #:grammar
         [(do语句 (code:line)
                  绑定语句
                  赋值语句)
          (赋值语句 (define datum ...))
          (绑定语句 (identifer <- datum))]]{
@racket[do?]会对每条@racket[do语句]检查，如果该语句返回@racket[#f]，
那么整条语句就会退出，同样只返回@racket[#f]。
可以把@racket[do?]看成一个巨大的@racket[and]，并且它允许对中间值进行绑定，即@racket[绑定语句]。

@racketblock[
 (do? (a <- #f)
      (define b : Real 2)
      (+ b a))
]
}
