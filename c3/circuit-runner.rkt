#lang racket

(require "agenda.rkt")
(require "wires.rkt")
(require "boolean-circuits.rkt")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(probe 'first-and a)
(probe 'second-and b)
(probe 'and-result c)

(and-gate a b c)
(set-signal! a 0)
(set-signal! b 1)

(propagate)
(set-signal! a 1)
(set-signal! b 0)

(propagate)
