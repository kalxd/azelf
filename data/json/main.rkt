#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         "../maybe/maybe.rkt"
         json
         racket/contract)

(export-from "./json.rkt")
(provide json->string
         json->output)

(define/contract json->string
  (-> ToJSON? string?)
  (compose jsexpr->string
           ->json))

(module+ test
  (require rackunit)

  (test-case "<ToJSON>:json->string"
    (check-equal? "1"
                  (json->string 1))
    (check-equal? "null"
                  (json->string nothing))
    (check-equal? "null"
                  (json->string (Just nothing)))
    (check-equal? "2"
                  (json->string (Just (Just 2))))))

(define/contract (json->output port o)
  (-> output-port? ToJSON? void?)
  (let ([js (->json o)])
    (write-json js port)))

(module+ test
  (require racket/port)
  (test-case "<ToJSON>:json->output"
    (check-equal? "null"
                  (call-with-output-string
                   (λ (port) (json->output port nothing))))
    (check-equal? "2"
                  (call-with-output-string
                   (λ (port) (json->output port (Just (Just 2))))))))
