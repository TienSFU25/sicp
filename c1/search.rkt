#lang racket

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (sum-integers a b)
(if (> a b)
0
(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
(if (> a b)
0
(+ (cube a)
(sum-cubes (+ a 1) b))))

(define (pi-sum a b)
(if (> a b)
0
(+ (/ 1.0 (* a (+ a 2)))
(pi-sum (+ a 4) b))))

(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a)
         (sum (next a) b term next))))

(define (inc n) (+ n 1))

(define (sumcubes a b) (sum a b cube inc))
(cube 7)
(sumcubes 1 10)

(define (pisum a b)
  (sum a b
       (lambda (x) (/ 1.0 (* x (+ x 2))))
       (lambda (x) (+ x 4))))

(pisum 1 10000)

(define (integral f a b dx)
(* (sum (+ a (/ dx 2.0))
        b
        f
        (lambda (x) (+ x dx)))
dx))

(integral cube 0 1 0.01)

(define (add a b)
  ((lambda (a b)
    (+ a b))
  a
  b))

(add 12 32)

(define (f x y)
(let ((a (+ 1 (* x y)))
(b (- 1 y)))
(+ (* x (square a))
(* y b)
(* a b))))

(+ (let ((x 3))
(+ x (* x 10)))
5)

(define (average a b) (/ (+ a b) 2))
(define (positive a) (> a 0))
(define (negative a) (< a 0))

(define (closeenough a b)
	(< (abs (- a b)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
  (if (closeenough neg-point pos-point)
      midpoint
      (let ((testvalue (f midpoint)))
      (cond ((positive testvalue) (search f neg-point midpoint))
            ((negative testvalue) (search f midpoint pos-point))
            (else midpoint))))))

(define (half-interval-method f a b)
(let ((a-value (f a))
(b-value (f b)))
(cond ((and (negative a-value) (positive b-value))
(search f a b))
((and (negative b-value) (positive a-value))
(search f b a))
(else
(error "Values are not of opposite sign" a b)))))

(define (myfunc a)
  (+ (* 3 a) 2))

(search myfunc -3 6)

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda(x)(+ (* x x x) (* x x) 1))
                      -5
                      6)

(+ 1 1)