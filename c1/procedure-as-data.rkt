#lang racket

(define (cons x y)
(define (dispatch m)
(cond ((= m 0) x)
((= m 1) y)
(else (error "Argument not 0 or 1: CONS" m))))
dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

((cons 10.2 2) 0)

; dont understand this church numerals stuff
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
(lambda (f) (lambda (x) (f ((n f) x)))))

(define thezero ((zero "whogivesashit") 0))
thezero
