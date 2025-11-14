#lang scribble/manual

@title{azelf}
@author{荀洪道}

@defmodule[azelf]

超能力工具箱，基于typed racket的脚本语言。
不要问为什么脚本一定要带类型，问就是为你的代码负责——连简单类型检查都通不过的代码，跟垃圾有什么区别？！

整体设计向原始racket靠拢，尽量不重复造轮子，复用现有生态。
