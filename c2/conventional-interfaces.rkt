#lang racket

;shitty versions

(define (square x) (* x x))
(define nil '())

(filter (lambda (x) (= 1 x)) (list 1 2))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;(accumulate + 12 (list 1 2 3))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (fib k) (k))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map2 square (list 1 2 3))

(define (appendy seq1 seq2)
  (accumulate cons seq2 seq1))

(appendy (list 1 2) (list 3 4 5))

(define (lengthy sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(lengthy (list 1 2 33 3 4 1 2 3))

; exercises down here

(define (horny-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (print this-coeff)(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horny-eval 2 (list 1 2 3))

(define (count-leaves2 t)
  (accumulate +
              0
              (map
               (lambda (x)
                 (cond ((null? x) 0)
                       ((pair? x) (+ (count-leaves2 (list (car x))) (count-leaves2 (cdr x))))
                       (else 1)))
               t))
  )

(count-leaves2 (list 1 (list 1 2 (list 3 4) 6) 2 3))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;matrix stuff

(define v1 (list 2 3 4))
(define v2 (list 1 3 5))
(define m1 (list (list 1 2 3) (list 2 4 5)))
(define m2 (list (list 3 4) (list 1 3) (list 3 6)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v1 v2)
(map * v1 v2)

(define (matrix-*-vector m v) (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector m1 v1)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m1)

(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (x) (matrix-*-vector cols x)) m1)))

(matrix-*-matrix m1 m2)