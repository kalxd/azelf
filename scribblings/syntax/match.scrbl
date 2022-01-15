#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "match"]{模式匹配}

@racketmodname[azelf]包含整个@racketmodname[racket/match]，该模块是对@racketmodname[racket/match]的一个补充。

@defform[(define/match1 函数名 函数体 ...)]{
类似于@racket[define/match]，@racket[define/match1]只支持一个参数函数，并且该参数用@racket[it]表示。

@examples[
#:eval sb

(define/match1 f
  [1 2]
  [_ 10])

(f 1)
(f 2)

(define/match1 g
  [1 (+ it it it)]
  [_ 10])

(g 1)
(g 2)
]
}

@defform[(define/match1/contract 函数名 contract-body 函数体 ...)]{
像我这么在乎安全的人，怎能少了@racketmodname[racket/contract]。

@examples[
#:eval sb

(define/match1/contract f
  (-> positive? positive?)
  [1 2]
  [_ -1])

(f 1)
(f 10)
]
}

@defform[(define/match/contract (函数名 参数 ...) contract-body 函数体 ....)]{
我们也给@racket[define/match]加个@racketmodname[racket/contract]。

@examples[
#:eval sb

(define/match/contract (f a)
  (-> positive? positive?)
  [(1) 2]
  [(_) -10])

(f 1)
(f 2)
]

}
