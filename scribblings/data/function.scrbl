#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf json))

@title[#:tag "function"]{函数}

一些辅助函数，该模块已经导出了@racketmodname[racket/function]的@racket[identity]。

@defproc[(const [a any/c] [b any/c]) a]{
重新定义@racket[const]，原因在于@racketmodname[racket/function]的const并没有柯里化处理。这版本已实现全部柯里化。

@examples[
#:eval sb

(const 1 2)
((const 1) 2)
]
}

@defproc[(flip [f (-> a b c)]) (-> b a c)]{
反转函数入参顺序，某些场景还是挺有用。

@examples[
#:eval sb
(define f (flip string))

(string #\A #\B)
(f #\A #\B)
]
}
