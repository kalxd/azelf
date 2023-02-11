#lang racket/base

(require racket/generic
         racket/match
         (only-in racket/function
                  identity))

(provide show)

(define/match (show-list-inner acc xs)
  [(_ (list)) (string-append acc "]")]
  [(_ (list a as ...))
   (let ([acc- (string-append acc
                              ","
                              (show:show a))])
     (show-list-inner acc- as))])

(define/match (show-list xs)
  [((list)) "[]"]
  [((list a as ...))
   (string-append "["
                  (show:show a)
                  (show-list-inner "" as))])

(define-generics Show
  (show:show Show)

  #:defaults ([string? (define show:show identity)]
              [char? (define show:show string)]
              [number? (define show:show number->string)]
              [list? (define show:show show-list)]))

(define show show:show)
