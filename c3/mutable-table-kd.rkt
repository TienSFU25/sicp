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
    (define (lookup listofkeys)
      (define (lookuprec remaining-keys prevtable)
        (if (null? remaining-keys)
            (mcdr prevtable)
            (let ((subtable (assoc (car remaining-keys) (mcdr prevtable))))
              (if subtable
                  (lookuprec (cdr remaining-keys) subtable)
                  false))))
      (lookuprec listofkeys local-table)
     )
    (define (make-empty-subtable table newkey)
      (let ((subtable (mcons newkey null)))
        (begin (set-mcdr! table
                          (mcons subtable (mcdr table)))
        subtable
        )
      ))
    (define (insert! listofkeys value)
      (define (insertrec completed-keys remaining-keys previous-table)
        (if (null? remaining-keys)
            (set-mcdr! previous-table value)
            (let ((thiskey (car remaining-keys)))
              (let ((next-table (assoc thiskey (mcdr previous-table)))
                    (next-completed (cons completed-keys (car remaining-keys)))
                    (next-remaining (cdr remaining-keys)))
                (if next-table
                    (insertrec next-completed next-remaining (mcdr next-table))
                    (let ((next-table (make-empty-subtable previous-table thiskey)))
                      (insertrec next-completed next-remaining next-table)))))))
      (insertrec '() listofkeys local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            ((eq? m 'printtable) printtable)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define mytable (make-table))
((mytable 'insert) '("k1" "k2") "randomassvalue")

((mytable 'printtable))
((mytable 'insert) '("k1" "k3") "another")

((mytable 'printtable))

((mytable 'insert) '("k1" "k4") "vlk4")
((mytable 'printtable))
((mytable 'insert) '("k2" "k12") "yetanother")

((mytable 'printtable))
((mytable 'lookup) '("k1" "k2"))
((mytable 'lookup) '("k1" "k3"))
((mytable 'lookup) '("k2" "k12"))

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert))

((mytable 'lookup) '("k2" "k44"))

((mytable 'insert) '("k1" "k5" "k3" "k7" "k10") "vl5ks")
((mytable 'printtable))
(println "ditmelookingup complicated shit")
((mytable 'lookup) '("k1" "k5" "k3" "k7"))
((mytable 'lookup) '("k1" "k5" "k3" "k7" "k10"))
