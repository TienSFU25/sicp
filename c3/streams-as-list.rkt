#lang racket

(provide (all-defined-out))

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
  ;(println 'FORCED)
  (e))

; THIS SHIT NOT GONNA WORK
; (delay b) already evaled when consing
; (define (cons-stream a b) (cons a (delay b)))
; (define (delay e) (memo-proc (lambda () e)))

; special form
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-enumerate-interval low high)
  ;(println 'enumming)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             ;(println 'before-streaming-cdr)
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (println x))

(define (stream-map1 proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons
       (apply proc (map car argstreams))
       (apply stream-map1
              (cons proc (map cdr argstreams))))))

(define (stream-map2 proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (foo x)
  (display "foo: ") (write x) (newline)
  x)

(define cs (cons-stream 1 (foo 2)))
(println 'should-not-see-anything-before-this)
(stream-cdr cs)