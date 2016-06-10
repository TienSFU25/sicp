#lang racket
(define (compose f g) (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (sq x) (* x x))

((compose sq inc) 6)

(define (repeat f n)
  (define (repeat-iter f n counter)
    (if (= counter n)
        f
        (let ((nextf (compose f f)))
               (repeat-iter nextf n (+ counter 1)))))
  (repeat-iter f n 1))

((repeat sq 2) 5)

;smoothing exercise

(define dx 0.0001)
(define (average a b c) (/ (+ a b c) 3))

(define (nsmooth f n)
  (define (smooth f)
    (lambda (x) (average (f (+ x dx)) (f (- x dx)) (f x))))
  (repeat smooth n)
)

(define (iter-improve goodenough? improve-guess)
  (define (oneguess lastguess)
                    (let ((nextguess (improve-guess lastguess)))
                      (if (goodenough? nextguess lastguess)
                          nextguess
                          (oneguess nextguess))))
  (lambda (guess) (oneguess guess)))

(define tolerance 0.0001)
(define (fixed-point f)
  (iter-improve
   (lambda (a b) (< (abs (- a b)) tolerance))
   f))

(define (average2 a b) (/ (+ a b) 2))

(define (average-damp f)
(lambda (x) (average2 x (f x))))

(define (sqrt x)
  ((fixed-point (average-damp (lambda (y) (/ x y))
               )) 1))

(sqrt 2)