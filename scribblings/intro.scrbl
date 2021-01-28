#lang scribble/manual

@require[@for-label[azelf]]

@title{简介}

@codeblock{
#lang azelf

(->> 1 (+ it 1))
;; 2
}

@codeblock{
#lang racket/base
(require azelf)

(->> 1 (+ it 1))
}
