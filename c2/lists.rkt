#lang racket
(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)

(cdr one-through-four)

(car (cdr one-through-four))
(cadr one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (sameparity . list)
  (define (sameparityrec currlist firstitem)
    ;(print currlist)
    (if (= 0 (length currlist))
        '()
        (if (= (remainder (car currlist) 2) (remainder firstitem 2))
            (cons (car currlist) (sameparityrec (cdr currlist) firstitem))
            (sameparityrec (cdr currlist) firstitem))))
  (sameparityrec list (car list))
  )

(sameparity 1 2 3 4 5)
(sameparity 2 3 4 5 6 7)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)

(define x (cons (list 1 2) (list 3 4)))
(length x)
(list x x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 1 2 (list 3 4) 6) 2 3))