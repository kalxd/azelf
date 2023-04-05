#lang scribble/manual

@(require (for-label azelf))

@title[#:style '(toc) #:tag "std"]{标准库}

标准库的方法都排除在@code{#lang azelf}之外，需要对应的功能，要@racket[require]进来。

@include-section["config.scrbl"]
@include-section["http.scrbl"]
@include-section["file.scrbl"]
