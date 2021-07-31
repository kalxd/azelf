#lang racket/base

(require racket/list
         racket/vector
         (for-syntax racket/base
                     racket/string
                     racket/list))

(provide define/values)

(define-for-syntax (info-for-spread var-syntax)
  (define var-list (syntax->datum var-syntax))
  (define var-str-list (map symbol->string var-list))
  (define len (length var-list))
  (define pos (index-where var-str-list
                           (λ (var)
                             (string-prefix? var
                                             "..."))))
  (define name (and pos (string-trim (list-ref var-str-list pos)
                                     "...")))
  (define name-replace (if name
                           (list-set var-list
                                     pos
                                     (string->symbol name))
                           var-list))
  (values len
          pos
          name-replace))

(define-syntax (define/values stx)
  (syntax-case stx ()
    [(_ (a ...) xs)
     (let-values ([(len pos name) (info-for-spread #'(a ...))])
       (with-syntax ([n len]
                     [index pos]
                     [name (datum->syntax stx name)])
         #'(begin
             (define xs-length (length xs))

             ;; 切头部
             (define cut-head
               (or index n))

             ;; 切结尾
             (define cut-tail
               (if index
                   (- n index 1)
                   n))

             (define ys
               (let*-values ([(head xxs) (split-at xs cut-head)]
                             [(middle tail) (if index
                                                (split-at-right xxs cut-tail)
                                                (values empty empty))]
                             [(middle) (if index
                                           (list middle)
                                           empty)])

                 (append head middle tail)))

             (define-values name
               (apply values ys)))))]))
