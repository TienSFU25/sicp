#lang racket

(require "streams-as-list.rkt")
(require "stream-add.rkt")

(define (average a b) (/ (+ a b) 2))
(define (square a) (* a a))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt-stream2 x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream2 x))))

(display-nth (sqrt-stream 3) 5)
(display-nth (sqrt-stream2 3) 5)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display-nth pi-stream 8)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-nth (euler-transform pi-stream) 8)

; super tranforms :|
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-nth (accelerated-sequence euler-transform pi-stream) 5)

; exercise
(define (sqrt-tol x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (define (tol-rec diff sn)
    (if (< (abs (stream-car diff)) tolerance)
        (stream-car sn)
        (tol-rec (stream-cdr diff) (stream-cdr sn))))
  (let ((sn-1 (cons-stream -1000 stream)))
    (let ((diff-stream (sub-streams stream sn-1)))
      (display-nth diff-stream 10)
      (newline)
      (display-nth stream 10)
      (newline)
      (tol-rec diff-stream stream))))

(sqrt-tol 9 0.05)
