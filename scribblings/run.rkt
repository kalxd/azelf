#lang racket/base

(require scribble/eval
         azelf)

(provide (all-from-out scribble/eval)
         sb)

; sandbox
(define sb (make-base-eval))

(interaction-eval #:eval sb
                  (require azelf))
