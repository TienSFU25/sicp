#lang racket

(provide (all-defined-out))

;(cons 1 '(2 3 4))
;(mcons 1 (mcons 2 (mcons 3 (mcons 4 '()))))

;(define m (mcons 1 (mcons 2 3)))
;(mcar m)
;(mcdr (mcdr m))

(define 2primitive-procedures
  (list (mcons 'car car)
        (mcons 'cdr cdr)
        (mcons 'cons cons)
        (mcons 'null? null?)))
(define (2primitive-procedure-names)
  (mcons 'car (mcons 'cdr (mcons 'cons (mcons 'null? '())))))
(define (2primitive-procedure-objects)
  (mcons (lambda () (mcons 'primitive car))
         (mcons (lambda () (mcons 'primitive cdr))
                (mcons (lambda () (mcons 'primitive cons))
                       (mcons (lambda () (mcons 'primitive null?)) '())))))

; (mcdr (mcdr (mcdr (mcdr (primitive-procedure-names)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cadr cadr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '* *)
        (list 'positive? positive?)
        (list 'zero? zero?)
        (list 'assoc assoc)
        (list 'println println)
        (list 'null? null?)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(primitive-procedure-names)
(primitive-procedure-objects)