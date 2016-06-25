#lang racket

(provide (all-defined-out))

(define (list->mlist list)
  (if (null? list)
      '()
      (if (pair? list)
          (mcons (car list) (list->mlist (cdr list)))
          (println "Not a list"))))