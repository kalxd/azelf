#lang racket/base

(require "../internal/keyword.rkt"
         "../internal/pipeline.rkt")

(require racket/contract
         racket/port)

(provide file/save-by)

(define/contract (file/save-by f filepath)
  (-> (-> input-port?) path-string? void?)
  (void (call-with-output-file filepath
          (λ (port)
            (->> (f)
                 port->bytes
                 (write-bytes it port)))
          #:exists 'replace)))
