#lang scribble/manual

@require[@for-label[azelf]]

@title{简介}

Racket是一门动态语言，Racket是一门创建语言的语言。自当是用其能、尽其才，以她为基础，构建个人偏好极强的运行时，，更是个人语言。

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

@section{全景图}

@racketmodname[azelf]可以认为是JS函数式的延续：

@itemlist[@item{喜欢柯里化？@secref["curry"]了解一下。}
		  @item{不喜欢空指针？@secref["maybe"]了解一下。}
		  @item{想要js的pipeline？@secref["pipeline"]了解一下。}]
