#lang racket

(require "circ-test-ab.rkt")
(provide a)

(define (a x)
  (if (= x 2)
      1
      (b (+ x 1))))