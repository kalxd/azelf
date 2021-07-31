#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "mod"]{模块快速语法}

@section{同时导入导出其他模块}

@defform[(export-from module-path ...+)]{

平日里，我们常写：

@codeblock|{
(require a)

(provide (all-from-out a))
}|

那么有了@racket[export-from]，就能将它们合并一起：

@codeblock|{
(export-from a b c)
}|

}
