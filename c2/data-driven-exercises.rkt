#lang racket

;old definition of derivatives

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                   (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        ; ⟨more rules can be added here⟩
        (else (error "unknown expression type:
DERIV" exp))))

;new way, using a "dispatcher"

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define install-add-deriv
  (define (add-deriv exp var)
    ((make-sum (deriv (addend exp) var)
               (deriv (augend exp) var))))
  (put 'deriv '+ (lambda (e v) (add-deriv e v)))
  'done)

(define install-mul-deriv
  (define (mul-deriv exp var)
    (make-product
     (multiplier exp)
     (deriv (multiplicand exp) var))
    (make-product
     (deriv (multiplier exp) var)
     (multiplicand exp)))
    (put 'deriv '+ (lambda (e v) (mul-deriv e v)))
    'done)