#lang racket

(require "constraints-connector.rkt")
(require "constraints-arithmetic.rkt")

(define A (make-connector))
(define B (make-connector))
(define S (make-connector))
(define two (make-connector))
(constant 2 two)
(define avg (make-connector))

(define box-1 (adder A B S))
(define box-2 (multiplier avg two S))

(probe 'A A)
(probe 'B B)
(probe 'avg avg)

(define global-informant 'bomay)

(set-value! A 1 global-informant)
(set-value! B 2 global-informant)
(forget-value! A global-informant)
(set-value! A 6 global-informant)


(define (squarer a b)
  (multiplier a a b))

(define Q (make-connector))
(define W (make-connector))

(squarer Q W)
(probe 'Q Q)
(probe 'W W)
(set-value! Q 12 global-informant)
(forget-value! Q global-informant)
(set-value! W 100 global-informant)