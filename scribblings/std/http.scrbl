#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf azelf/std/http))

@title[#:tag "http"]{网络相关标准库}

@defmodule[azelf/std/http]

该模块自动导出了@racketmodname[net/url]，算是对原有功能的增强。

@section[#:tag "http-f"]{扩展接口}

@defproc[(url/clear-pathname [url-link url?]) (Array/c string?)]{
给出干净的pathname。

@examples[
#:eval sb
(require azelf/std/http)
(url/clear-pathname (string->url "http://baidu.com/a;c/b;d/image.jpg"))
]
}

@defproc*[([(url/filename [url-link url?]) (Maybe/c string?)]
           [(url/filename-without-ext [url-link url?]) (Maybe/c string?)])]{

@examples[
#:eval sb
(require azelf/std/http)
(url/filename (string->url "http://www.baidu.com"))
(url/filename (string->url "http://www.baidu.com/a/b.mp3"))
(url/filename (string->url "http://www.baidu.com/"))
]
}

@defproc[(url/rename-with [f (-> string? string?)] [url-link url?]) url?]{
为链接重命名，此处用的是高阶函数@racket[f]，避免手动再获取一次。

@codeblock{
(define url-link (string->url "http://baidu.com/a/b/c.jpg"))

(url/rename-with string-upcase url-link)
; http://baidu.com/a/b/C.jpg
}
}

@defproc[(url/rename-to [filename string?] [url-link url?]) url?]{

@examples[
#:eval sb
(url/rename-to "404.gif" (string->url "http://www.baidu.com/file.html"))
]
}
