#lang racket

; dispatcher method

(provide get)
(provide put)
(provide make-table)

(define (make-table)
  (let ((local-table (mcons "randomasstable" null)))
    (define (printtable) (println local-table))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (mcdr subtable))))
              (if record (mcdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key-1 (mcons (mcons key-2 value) (mcdr local-table)))
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            ((eq? m 'printtable) printtable)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define mytable (make-table))
((mytable 'insert) "k1" "k2" "randomassvalue")

((mytable 'printtable))
((mytable 'insert) "k1" "k3" "another")

((mytable 'printtable))

((mytable 'insert) "k1" "k4" "vlk4")
((mytable 'printtable))

((mytable 'insert) "k2" "k12" "yetanother")

((mytable 'printtable))
((mytable 'lookup) "k1" "k2")
((mytable 'lookup) "k1" "k3")

((mytable 'lookup) "k2" "k12")

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert))
