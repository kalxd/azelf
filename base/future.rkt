#lang racket/base

(require racket/future
         racket/contract

         (for-syntax racket/base))

(provide await
         define/async)

;; await ;;
(define (await f)
  (cond
    [(future? f) (touch f)]
    [else f]))

(module+ test
  (require rackunit)
  (test-case "<Future>:await"
    (check-equal? 1 (await 1))
    (check-equal? "hello" (await "hello"))
    (check-equal? 1 (await (future (λ () 1))))))
;; ;;

;; define/async ;;
(define-syntax-rule (define/async (f args ...) body ...)
  (define (f args ...)
    (future (λ () body ...))))

(module+ test
  (test-case "<Future>:define/async"
    (define/async (inc x)
      (+ 1 x))

    (check-equal? 2 (await (inc 1)))
    (check-equal? 3 (await (inc (await (inc 1)))))))
;; ;;
