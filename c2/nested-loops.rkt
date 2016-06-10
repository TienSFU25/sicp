#lang racket

(define nil '())
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (nested n) (accumulate
                    append nil (map (lambda (i)
                                      (map (lambda (j) (list i j))
                                           (enumerate-interval 1 (- i 1))))
                                    (enumerate-interval 1 n))))

(nested 5)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; lol
(define (prime? x) (even? x))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))

; exercises start below

(define (unique-pairs x)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 x)))

(unique-pairs 5)

(define (tris x)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 x)))
                  (enumerate-interval 1 x)))
           (enumerate-interval 1 x)))

(tris 4)

(define (tri-equal-s n s)
  (define (pair-eq-s? pair)
    (= s (+ (car pair) (cadr pair) (cadr (cdr pair)))))
  (filter pair-eq-s? (tris n)))

(tri-equal-s 3 6)