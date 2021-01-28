#lang scribble/manual
@require[@for-label[azelf
                    racket]]

@title{Maybe}

@section{类型定义}
@defproc[(Maybe/c [a any/c])
				  contract?]{
“泛型”maybe容器，检测@racket[Just]是否为a。
}

@defproc*[([(Just [a any/c]) (Maybe/c a)]
		   [(Nothing) (Maybe/c any/c)])]{
生成一个maybe实例。
}

@defthing[nothing (Maybe/c any/c) #:value (Nothing)]{
@racket[Nothing]本身就是特定值，不需要额外再构造一次。
}
