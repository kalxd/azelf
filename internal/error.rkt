#| 内部错误 |#
#lang racket/base

(provide raise-unwrap-error)

; 解空包时抛该错误。
(struct exn:fail:unwrap exn:fail []
  #:transparent)

(define (raise-unwrap-error msg)
  (raise (exn:fail:unwrap msg (current-continuation-marks))))
