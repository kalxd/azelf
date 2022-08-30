#lang s-exp "./internal/reader.rkt"

(require "../main.rkt")

(define maybe-ts
  (test-suite
   "data : maybe"

   (test-case "<maybe>: maybe-map"
    (check-equal? (Just 2) (maybe-map add1 (Just 1)))
    (check-equal? nothing ((maybe-map add1) nothing)))

   (test-case "<maybe>: maybe-unwrap"
    (check-equal? 1 (maybe-> 2 (Just 1)))
    (check-equal? 2 (maybe-> 2 nothing)))

   (test-case "<maybe>: maybe-then"
    (define (f x)
      (Just (add1 x)))
    (check-equal? (Just 2) (maybe-then f (Just 1)))
    (check-equal? nothing (maybe-then f nothing)))

   (test-case "<maybe>: maybe-filter"
     (check-equal? (Just 2) (maybe-filter even? (Just 2)))
     (check-equal? nothing (maybe-filter odd? (Just 2)))
     (check-equal? nothing (maybe-filter even? nothing)))

   (test-case "<maybe>: maybe-replace"
    (check-equal? (Just 1) (maybe-replace 1 (Just "1")))
    (check-equal? (Just 1) (maybe-replace 1 (Just #f)))
    (check-equal? nothing (maybe-replace 1 nothing)))

   (test-case "<maybe>: maybe-and"
    (check-pred Nothing? (maybe-and (Just 1) nothing))
    (check-pred Nothing? (maybe-and nothing (Just "hello")))
    (check-equal? (Just 1) (maybe-and (Just 2) (Just 1))))

   (test-case "<maybe>: maybe-or"
    (check-equal? nothing (maybe-or (Just 1) nothing))
    (check-equal? nothing (maybe-or nothing (Just 2)))
    (check-equal? (Just 2) (maybe-or (Just 2) (Just 1))))

   (test-case "<maybe>: maybe-alt"
    (check-equal? (Just 1) (maybe-alt nothing (Just 1)))
    (check-equal? (Just 2) (maybe-alt (Just 2) nothing))
    (check-equal? nothing (maybe-alt nothing nothing))
    (check-equal? (Just 1) (maybe-alt (Just 1) (Just 2))))

   (test-case "<maybe>: maybe-else"
    (check-equal? 2 (maybe-else (void) add1 (Just 1)))
    (check-equal? 11 (maybe-else (void) string->number (Just "11")))
    (check-pred void? (maybe-else (void) add1 nothing)))

   (test-case "<maybe>: ->maybe"
    (check-equal? (Just 1) (->maybe 1))
    (check-equal? nothing (->maybe #f)))

   (test-case "<maybe>: maybe-unwrap"
    (check-equal? 1 (maybe-unwrap (Just 1)))
    (check-equal? "ab" (maybe-unwrap (Just "ab")))
    (check-exn exn:fail?
               (λ ()
                 (maybe-unwrap nothing))))

   (test-case "<maybe>: maybe-catch"
    (check-equal? (Just 1) (maybe-catch 1))
    (check-equal? (Just 10) (maybe-catch (* 1 10)))
    (check-equal? nothing (maybe-catch (/ 1 0))))))

(define maybe-do-ts
  (test-suite
   "data : maybe-do"

   (test-case "<maybe>: maybe/do"
    (check-equal? nothing
                  (maybe/do
                   (a <- nothing)
                   (b <- (Just 20))
                   (Just (+ a b))))
    (check-equal? (Just 10)
                  (maybe/do
                   (a <- (Just 1))
                   (b <- (Just 9))
                   (Just (+ b a))))
    (check-equal? (Just 3)
                  (maybe/do
                   (a <- 1)
                   (b <- 2)
                   (+ a b)))
    (check-equal? nothing
                  (maybe/do
                   (a <- 1)
                   (b <- #f)
                   (+ a b)))

    (check-equal? (Just 20)
                  (maybe/do
                   (a <- (Just 5))
                   (let b = a)
                   (let c = (Just 10))
                   (c <- c)
                   (Just (+ a b c))))

    (check-equal? (Just 10)
                  (maybe/do
                   (a <- (Just 5))
                   (! nothing)
                   (Just (+ a a))))

    (check-equal? (Just 20)
                  (maybe/do
                   (a <- (Just 10))
                   (when (= a 10)
                     (break (Just 20)))
                   a)))))

(define task-list
  (list maybe-ts
        maybe-do-ts))
