#lang racket

; selectors
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? s) (null? s))
(define the-empty-stream null)

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (force e)
  (e))

; these are defined as macros
; to prevent "eager" execution
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

; filter and map on streams
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream s f)
  (stream-map (lambda (x) (* f x)) s))

; print methods for visualization
(define (display-nth s n)
  (define (iter counter stream)
    (if (= counter n)
        (begin (display '...)(newline))
        (begin (display (stream-car stream))
               (display #\space)
               (iter (+ 1 counter) (stream-cdr stream)))))
  (iter 0 s))

(define (delimiter) (displayln "------------"))

; start of real code
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(println integers)

; fibs example
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(displayln "First 10 fibs")
(display-nth fibs 10)
(delimiter)

; fibs the second
(define (add-streams s1 s2) (stream-map + s1 s2))
(define fibs-second
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(displayln "First 10 fibs, with implicit definition")
(display-nth fibs-second 10)
(delimiter)

; primes example
(define (divisible? x y)
  (= 0 (remainder x y)))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(displayln "First 10 primes")
(display-nth primes 10)
(delimiter)

; integral example
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(displayln "Integral example")
(define summations (integral integers 0 1))
(display-nth summations 10)
(delimiter)