#lang racket/base

(require racket/generic
         racket/match
         (only-in racket/function
                  identity))

(provide gen:Show
         Show?
         show:show
         show
         generic-fmt-show)

(define (generic-fmt-show a)
  (format "~v" a))

(define-generics Show
  (show:show Show)

  #:defaults ([string? (define show:show identity)]
              [char? (define show:show string)]
              [number? (define show:show number->string)]
              [symbol? (define show:show symbol->string)]
              [bytes? (define show:show bytes->string/utf-8)]
              [boolean?
               (define/match (show:show a)
                 [(#t) "true"]
                 [(#f) "false"])]
              [list? (define show:show generic-fmt-show)]
              [vector? (define show:show generic-fmt-show)]
              [hash? (define show:show generic-fmt-show)]))

(define show show:show)
