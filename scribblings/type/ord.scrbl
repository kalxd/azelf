#lang scribble/manual

@require[@for-label[azelf]]

@title{Ord}

需要特别说明，Racket本身已经定义了@racket[<]、@racket[max]等，为了与之区分，以下实现的重名函数名都以“@bold{:}”修饰。

@defidform[gen:Ord]{
最小实现@racket[compare]。
}

@defproc[(Ord? [a any/c]) boolean?]

@defthing[Ord/c contract?]

@deftogether[(@defproc*[([(LT) LT?]
						 [(EQ) EQ?]
						 [(GT) GT?])]
			  @defthing[lt LT?]
			  @defthing[eq EQ?]
			  @defthing[gt GT?])]{
这些列举的都是@racket[compare]函数的比较结果。每次实现Ord必需的值。

@codeblock{
(struct A [a]
  #:methods gen:Ord
  [(define (compare a b)
     (let ([a (A-a a)]
           [b (A-a b)])
       (if (equal? a b)
           eq
           lt)))])
}

@racket[LT]、@racket[EQ]、@racket[GT]可运用于@racket[match]。
}

@defproc*[([(LT? [a any/c]) boolean?]
		   [(EQ? [a any/c]) boolean?]
		   [(GT? [a any/c]) boolean?])]

@defproc[(compare [a Ord?] [b Ord?]) (or/c lt eq gt)]{
比较@code{a}与@code{b}，比较返回@racket[lt]等结果。

@codeblock{
(compare 1 1)
; #EQ
}
}

@defproc*[([(:< [a Ord?] [b Ord?]) boolean?]
		   [(:> [a Ord?] [b Ord?]) boolean?]
		   [(:<= [a Ord?] [b Ord?]) boolean?]
		   [(:>= [a Ord?] [b Ord?]) boolean?])]{
两数比较。

@codeblock{
(:< 1 2) ;; #t
(:>= #\A #\A) ;; #t
}
}

@defproc*[([(:max [a Ord?] [b Ord?]) (or/c a b)]
		   [(:min [a Ord?] [b Ord?]) (or/c a b)])]{
比较两者大小，返回要求的那个值。

@codeblock{
(:max 1 10) ;; 10
(:min #\B #\H) ;; #\B
}
}

@defproc[(clamp [low Ord?] [high Ord?] [a Ord?]) (or/c low high a)]{
给定一个范围[low, high]，如果@code{a}在该范围内，返回@code{a}；@code{a}在左侧，返回@code{low}，反之返回@code{high}。

@codeblock{
(clamp 1 10 5) ;; 5
(clamp 1 10 11) ;; 10
(clamp 1 10 0) ;; 0
}
}

@defproc[(between [low Ord?] [high Ord?] [a Ord?]) boolean?]{
检查@code{a}在不在[low, high]区间内。

@codeblock{
(between 1 10 15) ;; #f
(between 1 10 6) ;; #t
}
}
