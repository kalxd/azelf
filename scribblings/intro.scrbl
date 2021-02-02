#lang scribble/manual

@require[@for-label[azelf racket/generic json]]

@title{简介}

Racket自身功能簿弱，不同数据结构功能经常重复，例如@racket[foldl]和@racket[for/fold]，前者仅适用于@racket[list]，后者可应对任意@racket[sequence?]。Racket本身也有混乱的地方，@racket[#f]代替Nil，部分地方导致混乱，例如转化为@racketmodname[json]的时候。

所以该库提供类型于Haskell一类的解决方案，利用@racketmodname[racket/generic]提供接口，实现统一行为，减少不必要的函数定义，增加记忆负担。同时增强原有Racket标准库，增加更多好用的函数与语法。

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
