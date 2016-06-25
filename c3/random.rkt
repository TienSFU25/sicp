#lang racket

(provide rand)
(provide random-in-range)
(provide rand-update)

(define random-init 0)
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (random-in-range low high)
(let ((range (- high low)))
(+ low (random range))))