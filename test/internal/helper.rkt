#lang racket/base

(require rackunit/text-ui)

(provide (all-defined-out))

(define (run-task task)
  (void (run-tests task 'verbose)))

(define (run-all-task task-list)
  (for ([task task-list])
    (run-task task)))
