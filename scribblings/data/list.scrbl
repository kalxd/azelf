#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "list"]{列表}

@section{查找}

@defproc[(head [xs (listof any/c)]) (Maybe/c any/c)]{
取出列表的头部。

@examples[
#:eval sb

(head (list))
(head (list 1))
(head (list "hello" "world"))
]
}

@section{遍历}

@defproc[(foldl [f (-> any/c any/c any/c)] [acc any/c] [xs list?]) any/c]{
柯里化的@racket[foldl]，固定参数长度。

@examples[
#:eval sb
(foldl + 0 (list 1 2 3 4))
(foldl cons (list) (list 1 2 3 4))
]
}

@defproc[(foldr [f (-> any/c any/c any/c)] [acc any/c] [xs list?]) any/c]{
柯里化的@racket[foldr],固定参数长度。

@examples[
#:eval sb
(foldr + 0 (list 1 2 3 4))
(foldr cons (list) (list 1 2 3 4))
]
}

@section{变换}

@defproc[(map [f (-> any/c any/c)] [xs list?]) list?]{
柯里化@racket[map]，固定参数个数。

@examples[
#:eval sb
(map add1 (list))
(map add1 (list 1 2))
]
}

@defproc[(zip [xs (listof any/c)] [ys (listof any/c)]) (listof pair?)]{
以@racket[cons]合并。

@examples[
#:eval sb

(zip '(1 2 3 4 5) '(a b c))
]
}

@section{过滤}

@defproc*[([(filter [f (-> a boolean?)] [xs (listof a)]) (listof a)]
		   [(reject [f (-> a boolean?)] [xs (listof a)]) (listof a)])]{
柯里化@racket[filter]，@racket[reject]是@racket[filter]的反面。

@examples[
#:eval sb

(filter even? (list 1 2 3 4 5 6))
(reject even? (list 1 2 3 4 5 6))
]
}

@defproc[(traverse [f (-> a (Maybe/c b))] [xs (listof a)]) (Maybe/c (listof b))]{
@examples[
#:eval sb

(define (get a)
  (->maybe (and (> a 5) a)))

(traverse get (list 10 11 12))

(traverse get (list 7 6 5 4 3))

(traverse get (list 1 2 10 12))

(traverse get (list))
]
}
