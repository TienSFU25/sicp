#lang racket

(define tolerance 0.0001)

(define (closeenough a b)
  (< (abs (- a b)) tolerance)
  )

(define (fixed-point f)
  (define (fixed-point-rec lastguess)
    (let ((nextguess (f lastguess)))
      (display lastguess)
      (newline)
      (if (closeenough nextguess lastguess)
          nextguess
          (fixed-point-rec nextguess))
      ))
  (fixed-point-rec 1.0)
)

(define dx 0.00001)

(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g)
(fixed-point (newton-transform g)))

(newtons-method (lambda(x) (+ (* x x x) (* 4 x x) 3)))

(define (square x) (* x x))

(define (sqrt x)
(newtons-method
(lambda (y) (- (square y) x))))