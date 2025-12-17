#lang scribble/manual

@title{azelf}
@author{荀洪道}

@defmodule[azelf]

基于@racketmodname[typed/racket/base]的超能力工具箱。
专注于静态脚本。

写脚本也一定要使用静态类型，不然无法保证脚本的正确性。
一直以来的误区都认为写脚本速度快，实际上忽略了脚本的正确性；写不对脚本没有运行的必要。

@local-table-of-contents[]

@include-section["option.scrbl"]
