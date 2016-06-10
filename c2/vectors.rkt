#lang racket
(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (op-vec op)
  (lambda (v1 v2) 
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define (add-vect v1 v2) ((op-vec +) v1 v2))
(define (sub-vect v1 v2) ((op-vec -) v1 v2))

(define (scale-vect v s) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v ))))

(add-vect (make-vect 1 2) (make-vect 3 4))
(scale-vect (make-vect 1 2) 3)

(define (make-frame origin edge1 edge2)
(list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cadr cdr(f)))