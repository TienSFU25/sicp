#lang racket

(require "streams-as-list.rkt")
(require "infinite-streams.rkt")

(provide (all-defined-out))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (xpairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

(define int-pairs (pairs integers integers))

(stream-filter
 (lambda (pair) (even? (+ (car pair) (cadr pair))))
 int-pairs)

(display-nth int-pairs 20)

(define (qpairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(define qint-pairs (qpairs integers integers))
(display-nth qint-pairs 20)

