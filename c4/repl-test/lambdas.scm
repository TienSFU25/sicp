; #lang scheme

(define (add-one fn)
  (lambda (x) (+ 1 (fn x))))

(define (times-five x) (* x 5))

(define (add-three x) (+ x 3))

(define add-four (add-one add-three))
(define *5+1 (add-one times-five))

(add-three 5)
(add-four 5)
(*5+1 3)