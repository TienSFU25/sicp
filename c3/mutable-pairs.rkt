#lang racket

(provide lookup)
(provide insert!)
(provide make-table)

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value)
                          (mcdr table)))))
  'ok)

(define (make-table)
  (mcons 'randomtablename null))

;(define mytable (make-table))
;(insert! 'a 'b mytable)
;(insert! 'c 'd mytable)
;(insert! 'e 'f mytable)
;(lookup 'c mytable)

;(print mytable)
