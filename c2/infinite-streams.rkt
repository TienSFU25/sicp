#lang racket

(require "streams-as-list.rkt")

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))