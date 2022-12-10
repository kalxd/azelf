#lang scribble/manual

@(require "./run.rkt"
          (for-label azelf))

@title{简介}

这是一款个人风格十分浓烈的lisp方言，一款向Haskell看齐的动态语言。

@section{安装与使用}

使用@exec{raco pkg install azelf}命令安装，需要升级时使用@exec{raco pkg update azelf}命令。

azelf同racket一样，也有两种使用方式：使用@italic{#lang}与@code|{(require azelf)}|。前者直接使用azelf整个环境，后者仅仅作为第三方依赖。

@codeblock{
#lang azelf
(->> 1 add1) ;; 2
}

@codeblock{
#lang racket/base
(require azelf)
(->> 1 add1) ;; 2
}

@section{快速入门}

@racketmodname[azelf]基本延续了@racketmodname[racket/base]大部分语言，并补充了部分新语法，
其中比较常用有@racket[->>]和@racket[maybe/do]。

@subsection{新增语法}

如上所述，新增管道语法@racket[->>]。

@codeblock{
(->> 1 add1 add1 add1)
; 与下面写法等价
(add1 (add1 (add1 1)))
}

@racket[->>]更加简洁可读。@racket[->>]还提供了新关键字——@racket[it]——指代上个管道传下的参数，省下写lambda函数步骤。

@codeblock{
(->> 1 add1 (if (< it 10) 0 it) add1)

; (if (< it 10) 0 it) 这一段包含了it，之后会自动展开成：

(->> 1
     add1
     (lambda (it)
       (if (< it 10) 0 it))
    add1)
}

除此之外，@racket[maybe/do]也是十分有用的语法，它连接了外部racket与@racketmodname[azelf]，
一般的racket代码，用@racket[#f]表示空值，@racketmodname[azelf]的空值是@racket[nothing]，模拟了Haskell的Maybe类型。
所以在@racket[maybe/do]中，一切@racket[#f]会自动转化成@racket[nothing]。

@examples[
#:eval sb

(maybe/do
  (a <- 1)
  (b <- #f)
  (+ a b))
]

因为b为空，所以整条语句的结果就是@racket[nothing]，当然了，@racket[maybe/do]自然也能正确处理@racket[Maybe?]类型。

与@racket[Maybe/do]相对应的，还有@racket[monad/do]，这个语法比较纯粹，不会做过多处理，一切Monad实例都可以在里面运行。

@subsection{代替语法}

@racketmodname[azelf]重新实现了@racket[Array]和@racket[Map]，分别代替了@racket[list]、@racket[hash]，
目的在于接口的一致性。
重写了@racket[filter]、@racket[sort]等默认函数，使用时需要稍加注意。

创建新数据：

@examples[
#:eval sb
(array 1 2 3 4)
]

创建@racket[Map]，提供了两种创建方式：一种是@racket[hashmap]函数，另一种就是{}语法。

@codeblock{
(hashmap 1 2 3 4)
{ 1 2 3 4 }
}

@section{小结}

个人精力有限，只能简单介绍这些，想要深入了解，请阅读各个模块、语法的文档。
