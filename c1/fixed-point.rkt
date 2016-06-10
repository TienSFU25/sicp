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

;(fixed-point cos)
;(fixed-point (lambda (y) (+ (sin y) (cos y))))
;(fixed-point (lambda(x) (+ (* x x) 1)))

(define (average a b)
  (/ (+ a b) 2)
  )

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))
                             )))

;(cos 0.73)
(sqrt 3)

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x)))))
golden-ratio

(define (square x) (* x x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               ))

(cube-root 5.0)