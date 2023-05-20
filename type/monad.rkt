#lang racket/base

(require racket/generic
         racket/contract

         syntax/parse/define
         (for-syntax racket/base))

(require "../internal/curry.rkt"
         "../internal/function.rkt"
         "../internal/keyword.rkt")

(provide gen:Monad
         Monad?
         monad:bind
         >>=
         =<<
         >>
         <<
         monad/do)

(define-generics Monad
  (monad:bind Monad f)

  #:defaults ([list?
               (define/contract (monad:bind ma f)
                 (-> list?
                     (-> any/c list?)
                     list?)
                 (for/foldr ([acc (list)])
                            ([a ma])
                   (append (f a) acc)))]
              [procedure?
               (define/contract (monad:bind f g)
                 (-> procedure? procedure? procedure?)
                 (λ (r)
                   (define a (f r))
                   ((g a) r)))]))

(define >>= (curry/n 2 monad:bind))
(define =<< (flip >>=))

(define/curry/contract (>> ma mb)
  (-> Monad? Monad? Monad?)
  (>>= ma (const mb)))

(define << (flip >>))

(define-syntax-parser *monad/do*
  ; a <- ma绑定语法
  [(_ (var:id (~datum <-) e:expr) es:expr ...+)
   #'(>>= e
          (λ (var)
            (*monad/do* es ...)))]

  ; let a = b普通赋值
  [(_ ((~datum let) var:id (~datum =) e:expr) es:expr ...+)
   #'(let ([var e])
       (*monad/do* es ...))]

  ; 副作用
  [(_ ((~datum !) es:expr ...+) rs:expr ...+)
   #'(begin
       es ...
       (*monad/do* rs ...))]

  ; 最后几句
  [(_ e1:expr e2:expr es:expr ...)
   #'(*monad/do* (>> e1 e2) es ...)]
  ; 最后一句
  [(_ e:expr) #'e])

(define-syntax-rule (monad/do body ...)
  (break-wrap
   (*monad/do* body ...)))
