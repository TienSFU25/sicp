#lang racket

(require "streams-as-list.rkt")

(define (add-one-two) (+ 1 2))
(add-one-two)

(define (add-one-two-promise) (delay add-one-two))
(add-one-two-promise)

((force (add-one-two-promise)))