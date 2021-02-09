#lang racket/base

(require racket/generic
         (only-in racket/list
                  append-map))

(provide gen:Bind
         Bind?
         Bind/c
         bind)

(define-generics Bind
  (bind f Bind)
  #:defaults
  ([list? (define bind append-map)]))
