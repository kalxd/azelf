#lang scribble/manual

@(require scribble/eval
          (for-label azelf))

@(define azelf-eval (make-base-eval))
@interaction-eval[#:eval azelf-eval
                         (require azelf)]

@title{azelf}
@author{XGLey}

@defmodule[azelf]

超能力工具箱，提供一切便于书写的语法及函数集合，让人爱上编程的感觉。

曾经年少轻狂，想在动态语言中寻找Haskell的味道。不堪回首的往事，兴许能让人想法更加成熟，思索再三，决定去掉在动态语言中难以使用的类型类，辅之以语法糖形式，书写华章。

@table-of-contents[]

@include-section["intro.scrbl"]
@include-section["syntax/intro.scrbl"]
@include-section["data/intro.scrbl"]
@include-section["std/intro.scrbl"]
