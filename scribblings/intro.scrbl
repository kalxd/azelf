#lang scribble/manual

@require[@for-label[azelf racket/generic json]]

@title{简介}

Racket自身功能簿弱，不同类型功能上重复，部分设计不明确：@racket[foldl]和@racket[for/fold]，前者仅适用于@racket[list]，后者可应对任意@racket[sequence?]；Racket用@racket[#f]代替Nil，导致在数据转换时语义不明，例如转化成@racketmodname[json]的时候。
为此，azelf致力打造规范、使用简便的“标准库”，类似于Purescript、Haskell的@bold{Prelude}模块设计。与此同时提供更多方便使用的语法与数据结构。

目的只有一个：赏心悦目！

总体设计上向Purescript的Prelude看齐，部分函数不可避免与@racketmodname[racket/base]重名，重名的函数不仅涵盖原有功能，还增强了不少新特性，例如@racket[=]，在racket中仅仅比较@racket[number?]，在@racketmodname[azelf]中可以比较任意@racket[Eq?]，自然也包括@racket[number?]！

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
