#lang racket/base

(provide raise-unwrap-error)

(struct exn:fail:unwrap exn:fail []
  #:transparent)

(define (raise-unwrap-error msg)
  (raise (exn:fail:unwrap msg (current-continuation-marks))))
