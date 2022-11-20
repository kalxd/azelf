#lang scribble/manual

@(require scribble/eval
          (for-label azelf))

@(define azelf-eval (make-base-eval))
@interaction-eval[#:eval azelf-eval
                         (require azelf)]

@title{azelf}
@author{荀涧林}

@defmodule[azelf]

超能力工具箱，专为Haskeller准备的语言，尽量提供更多柯里化函数及部分类型类。

@table-of-contents[]

@include-section["intro.scrbl"]
@include-section["syntax/intro.scrbl"]
@include-section["data/intro.scrbl"]
@include-section["std/intro.scrbl"]
