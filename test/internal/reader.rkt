#lang racket/base

(require rackunit
         (for-syntax racket/base))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out rackunit))

(define-for-syntax (expand-body stx body-list)
  (with-syntax ([(pm body ...)
                (local-expand (quasisyntax/loc stx
                                (#%module-begin #,@body-list))
                              'module-begin
                              '())])
    (quasisyntax/loc stx
      (pm body ...))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (let ([expand-body (expand-body stx #'(body ...))]
           [task-list (datum->syntax stx 'task-list)])
       (quasisyntax/loc stx
         (#,@expand-body

          (module+ test
            (require rackunit/text-ui)

            (define (run-task task)
              (void (run-tests task 'verbose)))

            (define (run-all-task task-list)
              (for ([task task-list])
                (run-task task)))

            (run-all-task #,task-list)))))]))
