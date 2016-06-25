#lang racket

(require "streams-as-list.rkt")
(require "infinite-streams.rkt")
(require "infinite-pairs.rkt")

(define (weighted-merge s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1weight (w s1car))
                 (s2weight (w s2car)))
           (cond ((< s1weight s2weight)
                  (cons-stream
                   s1car
                   (weighted-merge (stream-cdr s1) s2 w)))
                 ((> s1weight s2weight)
                  (cons-stream
                   s2car
                   (weighted-merge s1 (stream-cdr s2) w)))
                 (else
                  (cons-stream
                   s1car
                   (weighted-merge (stream-cdr s1)
                          (stream-cdr s2) w)))))))))

(define (weighted-pairs s t w)
  (weighted-merge
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))
   w))

(define (wsum pair)
  (+ (car pair) (cadr pair)))

(define normal-int-pairs (weighted-pairs integers integers wsum))
(display-nth normal-int-pairs 20)

(define (wweird pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define (not235 x)
  (and (not (divisible? x 2)) (not (divisible? x 3)) (not (divisible? x 5))))

(define s1 (stream-filter
            not235
            integers))

(define s2 (stream-filter
            not235
            integers))

(define s3 (weighted-pairs s1 s2 wweird))
(display-nth s3 20)

(define (wramanujan pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i) (* j j j))))

(define ramanujan (weighted-pairs integers integers wramanujan))
;(define first-ramanujan (stream-while-prev (lambda (curr prev) (println (wramanujan curr))(println (wramanujan prev)) (println '----) (= (wramanujan curr) (wramanujan prev)))
; ramanujan))

; first-ramanujan
