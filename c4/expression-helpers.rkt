#lang racket

(provide (all-defined-out))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq) (cons 'begin seq))
