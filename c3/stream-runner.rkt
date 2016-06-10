#lang racket

(require "streams-as-list.rkt")

(define range (stream-enumerate-interval
               10001 10010));
(println 'defining-k-----)
(define k (stream-filter even? range))

(println 'printing-k-----)
k
;(display-stream k)

(stream-cdr k)
(stream-cdr k)

(map (lambda (x) (+ 1 x)) '(1 2 3 4))

(define (something a . end)
  (println a)
  (println end))

(something 1)
(something 1 2)
(something 1 2 3 4)

(stream-map1 + '(1 2) '(3 4))

(define s1 (stream-enumerate-interval 1 4))
(define s2 (stream-enumerate-interval 5 8))

(define s3 (stream-map2 + s1 s2 s2))
s3
(display-stream s3)

(define (show x)
  (display-line x)
  x)

; start of dumb prints
(println "starting dumb printing")
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(println '-----)
(stream-ref x 5)
(stream-ref x 7)

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map2 accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

;(display-stream seq)
(stream-ref y 7)
;(display-stream z)
;(println sum)
