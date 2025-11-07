#lang typed/racket/base

(require (for-syntax racket/base))

(provide Option
         option/map
         option/unwrap-exn
         option/unwrap-error
         option/unwrap)

; 为了更方便处理异常的宏
(define-syntax-rule (fn-error (name arg ...) (handle ...))
  (define (name ma arg ...)
    (cond
      [(eq? ma #f) (handle ...)]
      [else ma])))

(define-type (Option a) (U #f a))

(: option/map
   (All (a b) (-> (Option a)
                  (-> a b)
                  (Option b))))
(define (option/map ma f)
  (cond
    [(eq? ma #f) #f]
    [else (f ma)]))

(: option/unwrap-exn
   (All (a) (-> (Option a)
                exn
                a)))
(fn-error (option/unwrap-exn err)
   (raise err))

(: option/unwrap-error
   (All (a) (-> (Option a)
                String
                a)))
(fn-error (option/unwrap-error err-msg)
  (error err-msg))

(: option/unwrap
   (All (a) (-> (Option a)
                a)))
(fn-error (option/unwrap)
  (error "无法从#f的Option中取值！"))
