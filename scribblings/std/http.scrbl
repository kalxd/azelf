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

@section[#:tag "http-client"]{HTTP客户端}

创建一个简易的客户端实现，目前只返回响应体，没有响应头。因为发送请求不算是个纯函数，所以下面只列出必要的函数说明。

@defproc*[([(http/get [option Requestable?]) input-port?]
           [(http/head [option Requestable?]) input-port?]
           [(http/delete [option Requestable?]) input-port?]
           [(http/options [option Requestable?]) input-port?]
           [(http/get/json [option Requestable?]) any/c]
           [(http/head/json [option Requestable?]) any/c]
           [(http/delete/json [option Requestable?]) any/c]
           [(http/options/json [option Requestable?]) any/c]
           [(http/get/html [option Requestable?]) string?]
           [(http/head/html [option Requestable?]) string?]
           [(http/delete/html [option Requestable?]) string?]
           [(http/options/html [option Requestable?]) string?]
           [(http/post [option ToBodyRequest?]) input-port?]
           [(http/put [option ToBodyRequest?]) input-port?]
           [(http/post/json [option ToBodyRequest?]) any/c]
           [(http/put/json [option ToBodyRequest?]) any/c]
           [(http/post/html [option ToBodyRequest?]) string?]
           [(http/put/html [option ToBodyRequest?]) string?])]{
发送请求。这些方法之间，仅method和返回数据结构的不同。

@codeblock{
(http/get "www.baidu.com")
(http/post/json "www.baidu.com")
(http/delete (string->url "www.google.com"))
}
}

@defproc*[([(http/set-query [key symbol?] [value Show?] [option Requestable?]) RequestOption?]
           [(http/set-header [key Show?] [value Show?] [option Requestable?]) RequestOption?]
           [(http/set-redirect [redirect exact-nonnegative-integer?] [option Requestable?]) RequestOption?]
           [(http/set-json [body ToJSON?] [option Requestable?]) RequestOption?]
           [(http/set-body [body bytes?] [option Requestable?]) RequestOption?])]{
一些常规的请求设定，字面意思。

@codeblock{
(->> "http://localhost"
     (http/set-header "Auth" "yes")
     (http/set-json (hashmap 'a "b" 'c "d"))
     (http/post))
}
}

@defproc[(http/download [save-path path-string?] [link Requestable?]) void?]{
保存网络资源到本地。
}
