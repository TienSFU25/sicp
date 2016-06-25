#lang racket

(require "streams-as-list.rkt")

(provide (all-defined-out))

(define (add-streams s1 s2) (stream-map2 + s1 s2))
(define (sub-streams s1 s2) (stream-map2 - s1 s2))
(define (mul-streams s1 s2) (stream-map2 * s1 s2))
(define (div-streams s1 s2) (stream-map2 / s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

; lol magic
(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define s (cons-stream 1 (add-streams s s)))

;(stream-ref s 0)

(define factorial-stream
  (cons-stream
   1
   (mul-streams (integers-starting-from 2) factorial-stream)))

(stream-ref factorial-stream 5)

(define partial-sum-stream
  (cons-stream
   1
   (add-streams (integers-starting-from 2) partial-sum-stream)))

(define (partial-sums s)
  (define news (cons-stream
                0
                (add-streams s news)
                ))
  (stream-cdr news))

(display-nth (scale-stream partial-sum-stream 2) 5)

(define mult2 (scale-stream integers 2))
(define mult3 (scale-stream integers 3))
(define mult5 (scale-stream integers 5))

(define hamming (merge (merge mult2 mult3) (merge mult3 mult5)))

(display-nth hamming 20)

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-nth (expand 1 7 10) 5)

(display-nth (partial-sums integers) 5)