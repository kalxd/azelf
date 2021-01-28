#lang scribble/manual

@require[@for-label[azelf racket]]

@title{Either}

@bold{异常}的安全类型。

@section{Either类型定义}

@defproc*[([(Left? (a any/c)) boolean?]
		   [(Right? (a any/c)) boolean?])]

@defproc[(Either/c (a any/c) (b any/c))
		  contract?]

@defproc*[([(Left (a any/c)) (Either/c a any/c)]
		   [(Right (a any/c)) (Either/c any/c a)])]{
Either值构造器，同样适用于@racket[match]。

@codeblock{
(define/match (right-add1 x)
  [(Right x) (add1 x)]
  [(Left _) 0])

(right-add1 (Right 1))
;; 2
(right-add1 (Left 1))
;; 0
}

}

@section{Either操作函数}

@defproc[(either/catch [action (-> any/c)])
		 (Either/c exn:fail? any/c)]{
自动捕获@code{action}异常，出现异常时，返回@racket[(Left exn:fail)]，反之，得到@code{action}执行结果，以@racket[Right]包装后返回。

@codeblock{
(either/catch (lambda () (/ 1 0)))
;; Left exn:fail:contract:divide-by-zero

(either/catch (lambda () (/ 1 1)))
;; Right 1
}
}
