#lang racket

;(require "circ-test-ab.rkt")
(require "circ-test-a.rkt")

(provide b)

(define (b y)
  (if (= y 2)
      2
      (a (+ y 1))))

(b -1)