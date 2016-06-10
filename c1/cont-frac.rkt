#lang racket
(define (cont-frac n d k)
  (define (cont-frac-iter n d k counter)
    (if (= k counter)
        (/ (n counter) (d counter))
        (/ (n counter) (+ (d counter) (cont-frac-iter n d k (+ 1 counter))))))
  (cont-frac-iter n d k 1))

(cont-frac (lambda (i) 1.0)
(lambda (i) 1.0)
100000)

(/ 1 1.61)