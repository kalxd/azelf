#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide option/map
         option/unwrap-exn
         option/unwrap-error
         option/unwrap
         do?)

; 为了更方便处理异常的宏
(define-syntax-rule (fn-error (name arg ...) (handle ...))
  (define (name ma arg ...)
    (cond
      [(eq? ma #f) (handle ...)]
      [else ma])))

(: option/map
   (All (a b) (-> (Option a)
                  (-> a b)
                  (Option b))))
(define (option/map ma f)
  (and ma (f ma)))

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

(define-syntax (do? stx)
  (define-syntax-class define-bind
    #:description "define绑定"
    #:literals (define :)
    (pattern (define key:id e:expr)
             #:with expr #'(define key e))
    (pattern (define key:id : ty:expr e:expr)
             #:with expr #'(define key : ty e)))

  (syntax-parse stx
    ; 绑定
    [(_ (val:id (~literal <-) e:expr) es:expr ...+)
     #'(let ([val e])
         (cond
           [(eq? #f val) #f]
           [else (do? es ...)]))]
    ; 变量绑定
    [(_ e:define-bind es:expr ...+)
     #'(begin
         e.expr
         (do? es ...))]
    ; 多条语句。
    [(_ e:expr es:expr ...+)
     #'(let ([value e])
         (cond
           [(eq? #f value) #f]
           [else (do? es ...)]))]
    ; 只剩下最后一句。
    [(_ e:expr) #'e]))
