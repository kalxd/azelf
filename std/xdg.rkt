#| 一些配置文件处理。 |#
#lang racket/base

(require "../syntax/pipeline.rkt"
         "../internal/keyword.rkt")

(require racket/contract
         racket/file)

(define/contract (make-xdg-dir name)
  (-> string? path-for-some-system?)
  (->> (find-system-path 'home-dir)
       (build-path it ".config" name)
       (! (make-directory* it))))
