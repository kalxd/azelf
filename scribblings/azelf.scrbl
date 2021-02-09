#lang scribble/manual

@(require scribble/eval
		  (for-label azelf))

@(define azelf-eval (make-base-eval))
@interaction-eval[#:eval azelf-eval
				  (require azelf)]

@title{azelf}
@author{XGLey}

@defmodule[azelf]

超能力工具箱，提供Haskell（Purescript）相似类型类、函数，用Racket写Haskell。

@table-of-contents[]

@include-section["intro.scrbl"]
@include-section["syntax.scrbl"]

@include-section["data/intro.scrbl"]
@include-section["type/intro.scrbl"]
