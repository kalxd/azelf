#lang s-exp "./internal/reader.rkt"

(require "../main.rkt")

(define curry-ts
  (test-suite
   "syntax : curry"
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

(define match-ts
  (test-suite
   "syntax : match"
   (test-case "<match>: define/match1"
    (define/match1 inc
      [1 0]
      [_ (add1 it)])
    (check-equal? 0 (inc 1))
    (check-equal? 3 (inc 2)))

   (test-case "<match>: define/match1/contract"
    (define/match1/contract inc
      (-> positive? positive?)
      [1 10]
      [10 1]
      [2 "2"]
      [_ (add1 it)])
    (check-equal? 10 (inc 1))
    (check-equal? 1 (inc 10))
    (check-exn exn:fail:contract?
               (λ () (inc 2))))

   (test-case "<match>: define/contract/match"
    (define/match/contract (list/sum xs)
      (-> (listof positive?) (or/c zero? positive?))
      [((list)) 0]
      [((list a as ...)) (+ a (list/sum as))])

    (check-equal? 10 (list/sum (list 1 2 3 4)))
    (check-exn exn:fail:contract?
               (λ ()
                 (list/sum (list 1 -2 3 -4)))))))

(define pipeline-ts
  (test-suite
   "syntax : pipeline"

   (test-case "<pipeline>: ->>"
     ; 常规操作，最低要求。
     (define a (->> 10
                    (λ (x) (+ 1 x))
                    (+ it it)
                    (- it 1)))
     (check-equal? 21 a)

     ; 副作用测试。
     (check-equal? 10
                   (->> 1
                        (! (add1 it))
                        (+ it 9)))

     ; 中断测试。
     (check-equal? 10
                   (->> 1
                        (when (= it 1)
                          (break 10))
                        add1)))

   (test-case "<pipeline>: <-<"
    ; 最低要求。
    (define f (<-< number->string
                   (+ 10 it)))

    (check-equal? "20" (f 10))

    ; 副作用测试。
    (define g (<-< add1
                   (! (+ 10 it)
                      (+ it it))
                   add1))
    (check-equal? 3 (g 1)))))


(define task-list
  (list curry-ts match-ts pipeline-ts))
