#lang racket/base

(require rackunit
         "../main.rkt")

(define ts
  (test-suite
   "syntax"
   (test-case "<Function>: define/curry"
     (define/curry (my/add x y z)
       (+ x y z))

     (check-equal? 3 (my/add 1 1 1))
     (check-equal? 3 ((my/add 1 1) 1)))

   (test-case "<function>: define/curry"
     (define/curry/contract (my/sub x y)
       (->i ([x positive?]
             [y (x) (<=/c x)])
            [result positive?])
       (- x y))

     (check-equal? 1 (my/sub 2 1))
     (check-equal? 1 ((my/sub 2) 1))
     (check-exn exn:fail:contract?
                (λ ()
                  (my/sub 1 2))
                ))

   (test-case "<function>: curry/n"
     (define test/add2 (curry/n 2 +))
     (check-equal? 4 (test/add2 1 3))
     (check-equal? 4 ((test/add2 1) 3)))))

(module+ test
  (require rackunit/text-ui)
  (void (run-tests ts 'verbose)))
