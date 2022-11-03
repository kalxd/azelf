#| 一些配置文件处理。 |#
#lang racket/base

(require "../internal/pipeline.rkt"
         "../internal/keyword.rkt"
         "../internal/curry.rkt"
         "../data/maybe.rkt")

(require racket/contract
         racket/file
         racket/match
         racket/generic)

(provide config-ref
         ConfigRef?
         config-get
         config-put)

(struct ConfigRef [ref]
  #:constructor-name config-ref

  #:methods gen:custom-write
  [(define/generic self/write-proc write-proc)
   (define (write-proc c port mode)
     (match-define (ConfigRef ref) c)
     (self/write-proc ref port mode))]

  #:guard
  (λ (ref name)
    (unless (path-string? ref)
      (raise-argument-error name
                            (format "~a不是有效路径" ref)
                            ref))
    ref))

(define/curry/contract (config-get key config)
  (-> symbol? ConfigRef? (Maybe/c any/c))
  (match-define (ConfigRef ref) config)
  (maybe-catch
    (get-preference key
                    (λ () (->> (format "~a不存在" key)
                               raise-user-error))
                    'timestamp
                    ref)))

(define/curry/contract (config-put key value config)
  (-> symbol? any/c ConfigRef? void?)
  (define key-list (list key))
  (define value-list (list value))
  (match-define (ConfigRef ref) config)
  (put-preferences key-list
                   value-list
                   #f
                   ref))
