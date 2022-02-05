#lang scribble/manual
@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "list"]{列表}

@defproc[(head [xs (listof any/c)]) (Maybe/c any/c)]{
取出列表的头部。

@examples[
#:eval sb

(head (list))
(head (list 1))
(head (list "hello" "world"))
]
}

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

@defproc[(zip [xs (listof any/c)] [ys (listof any/c)]) (listof pair?)]{
以@racket[cons]合并。

@examples[
#:eval sb

(zip '(1 2 3 4 5) '(a b c))
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
