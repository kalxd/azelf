#lang info

(define collection "azelf")
(define pkg-desc "超能力工具箱")
(define version "0.7.0")
(define pkg-authors '(XGLey))

(define deps '(["base" #:version "8.16"]))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))

(define scribblings '(("scribblings/azelf.scrbl"
                       (multi-page))))
