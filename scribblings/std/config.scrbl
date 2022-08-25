#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf azelf/std/config))

@title[#:tag "config"]{应用程序配置管理}

@defmodule[azelf/std/config]

该模块是基于@racket[get-preference]、@racket[put-preferences]的封装。这两个函数都有一个可选参数@italic{pathname}，如果设为@racket[#f]，那么它就会去读取系统的配置，这个配置可以从
@codeblock{
(find-system-path 'pref-file)
}
找到。

说回这个config封装，我们这个config最好是用户的配置，不要再去污染系统，例如每个单独的程序都以自己的配置文件，而且相互独立、隔离，不能都放到系统配置上。基于这个前提，用户就需要自己管理配置文件，一旦确定配置路径（或称为@italic{filepath}），就可以我们这个模块了。

第一步就是将它包装进我们为之准备的房舍。
@codeblock{
(define ref (config-ref filepath))
}
这个@italic{ref}就是我们配置文件的引用，之后就能进行读（@racket[config-get]）写（@racket[config-put]）了。

@defproc[(ConfigRef? [a any/c]) boolean?]{
}

@defproc[(config-ref [ref path-string?]) ConfigRef?]{
配置文件的包装，在此包装上，提供一些额外的方法。
}

@defproc[(config-get [key symbol?] [config ConfigRef?]) (Maybe/c any/c)]{
从配置文件@racket[config]中读取单项键值。

从配置文件中读取，结果就两种：读得到与读不到。不管文件存不存在，这个函数都不会抛出任何异常；如果能正确读取，返回@racket[Just]，除此之外的所有情况都返回@racket[nothing]。

@codeblock{
; 文件不存在
(define ref (config-ref "/a/not/exist"))

(config-get 'name ref) ; Nothing

; 存在的文件
(define ref (config-ref "./my-config.rktd"))
(config-get 'name ref) ; Just 1
(config-get 'unkown ref) ; Nothing
}

}

@defproc[(config-put [key symbol?] [value any/c] [config ConfigRef?]) void?]{
将配置项写回配置文件，不管文件存不存在，都可以调用该函数：如果不存在就会自动创建。

@codeblock{
(define ref (config-ref "my.rktd"))
(config-put 'name "this is my name" ref) ; 完成，并自动创建my.rktd文件
}
}
