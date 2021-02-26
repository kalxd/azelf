#lang racket/base

(require (only-in "../../internal/macro.rkt"
                  export-from)
         json
         racket/contract)

(export-from "./json.rkt")
(provide json->string)

(define/contract json->string
  (-> ToJSON? string?)
  (compose jsexpr->string
           ->json))

(module+ test
  (require rackunit
           "../maybe/maybe.rkt")
  (test-case "<ToJSON>:json->string"
    (check-equal? "1"
                  (json->string 1))
    (check-equal? "null"
                  (json->string nothing))
    (check-equal? "2"
                  (json->string (Just (Just 2))))))
