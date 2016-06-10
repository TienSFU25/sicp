#lang racket

(require "../c1/gcd.rkt")
(require "random.rkt")

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else(iter (- trials-remaining 1)
                     trials-passed))))
  (iter trials 0))

(estimate-pi 1000)

;exercise

(define (estimate-integral trials x1 x2 y1 y2 predicate)
  (define (rect-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (predicate x y)))
  (let ((area (* (- x2 x1) (- y2 y1)))
    (percent-pass (monte-carlo trials rect-test)))
    (* area percent-pass)))

(define (pi-predicate x y) (< (+ (- (* x x) 1) (- (* y y)) 1) 1))

(+ (estimate-integral 10000 0 2 0 2 pi-predicate) 0.001)