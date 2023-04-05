#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf azelf/std/file))

@title[#:tag "file"]{系统文件标准库}

@defmodule[azelf/std/file]

虽说是标准库的一部分，但也仅仅添加少量的接口，更多的部分需要从@racketmodname[racket/file]去获取。

@section[#:tag "file-f"]{扩展接口}

@defproc[(file/save-by [f (-> input-port?)] [file-path path-string?]) void?]{
从@racket[f]生产出的内容，覆盖写入到@racket[file-path]。

@codeblock{
(file/save-by (lambda () (open-input-string "hell world")) "test.txt")

; test.txt的内容：
; hello world
}
}
