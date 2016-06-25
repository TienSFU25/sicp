#lang racket

(require "streams-as-list.rkt")
(require "stream-add.rkt")
;(require "infinite-streams.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define s1 (integral integers 0 1))

(display-nth s1 10)
(define ones (cons-stream 1 ones))
(display-nth ones 5)

(define (RC R C dt)
  (lambda (s v0)
    (let ((v0s (scale-stream ones v0)))
      (add-streams v0s
                   (add-streams (scale-stream (integral s 0 dt) (/ 1 C))
                                (scale-stream s R))))  ;this one not really correct
    ))

(define RC1 (RC 5 1 0.5))
(define rc1 (RC1 integers 1))

(display-nth rc1 5)

(define (sign-change-detector a1 a2)
  (if (< (* a1 a2) 0)
      1
      0))

(define (make-zero-crossings sense-data)
  (define zero-crossings
    (stream-map2 sign-change-detector
                sense-data
                (stream-cdr sense-data)))
  zero-crossings
)

(define testsignal (stream-from-list '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-cross-test (make-zero-crossings testsignal))
(display-nth zero-cross-test 10)

(define (smooth-stream stream)
  (define (smooth-stream-rec s1 s2)
    (stream-map (lambda (x) (/ x 2)) (add-streams s1 s2)))
  (smooth-stream-rec stream (cons-stream 0 stream)))

(display-nth (smooth-stream testsignal) 10)
