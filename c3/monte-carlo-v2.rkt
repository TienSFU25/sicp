#lang racket

(require "random.rkt")
(require "streams-as-list.rkt")

(define random-init 1)

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

(display-nth pi 10)

(define (random-stream input-stream)
  (define (random-stream-rec stream lastvalue rinit)
    (if (= (stream-car stream) 0)
        (random-stream-rec (cons-stream 1 (stream-cdr stream)) (+ rinit 1) (+ rinit 1))
        (let ((nextval (rand-update lastvalue)))
          (cons-stream nextval
                       (random-stream-rec (stream-cdr stream) nextval rinit)))))
  (random-stream-rec input-stream 0 0))

(display-nth (random-stream (stream-from-list '(1 1 1 1 1 1 1 1 1 1 1 1))) 10)
(display-nth (random-stream (stream-from-list '(1 1 1 1 1 0 1 1 1 1 1 1))) 10)

(define (random-range-stream x1 x2)
    (cons-stream (random-in-range x1 x2) (random-range-stream x1 x2)))

(define (estimate-integral-modular x1 x2 y1 y2 predicate)
  (let ((xs (random-range-stream x1 x2))
        (ys (random-range-stream y1 y2))
        (area (* (- x2 x1) (- y2 y1))))
    (let ((results (stream-map2 (lambda (x y) (predicate x y)) xs ys)))
      (scale-stream (monte-carlo results 0 0) area))
    ))

(define (pi-predicate x y) (< (+ (- (* x x) 1) (- (* y y)) 1) 1))

(display-nth (random-range-stream 5 10) 10)
(stream-ref (estimate-integral-modular 0 2 0 2 pi-predicate) 10000)