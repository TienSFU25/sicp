#lang racket

(require "streams-as-list.rkt")
(require "infinite-streams.rkt")
(require "infinite-pairs.rkt")

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (apair) (list (stream-car s) (car apair) (cadr apair)))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(define pythas (stream-filter
 (lambda (triple) (let ((i (car triple))
                        (j (cadr triple))
                        (k (caddr triple)))
                    (= (* k k) (+ (* i i) (* j j)))))
   int-triples))

(display-nth int-triples 50)
(println 'pythas)
(display-nth pythas 3)