#lang scribble/manual

@(require "../run.rkt"
		  (for-label azelf))

@title[#:tag "curry"]{柯里化}

@section{curry定义}

@defform[(define/curry (id args ...+) body ...+)]{
结合了@racket[define]及@racket[curry]，直接定义柯里化函数。

@examples[
#:eval sb

(define/curry (my/add a b)
  (+ a b))

(my/add)
(my/add 1)
(my/add 1 2)
]

}

@section{定义柯里化、约束型函数}

@defform[(define/curry/contract (id args ...+) body ...+)]{
与@racket[define/curry]类似，同样定义出柯里化函数，唯一不同在于它内部用了@racket[define/contract]，能对函数做出约束。

@examples[
#:eval sb

(define/curry/contract (my/add a b)
  (-> number? number? number?)
  (+ a b))

(my/add)

(my/add 1 2)
((my/add 1) "2")
]

}


@section{柯里化不定参函数}

@defform[(curry/n f number)
		#:grammar
		[(f 已定义的函数名)
		 (number 确定参数个数)]]{
将不定参函数@racket[f]，转化成全新、参数个数确定（@racket[number]个）的柯里化函数。

@examples[
#:eval sb

(define my/add (curry/n + 2))

(my/add)

(my/add 3 4)

(my/add 3 4 5)
]

}
