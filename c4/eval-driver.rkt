#lang racket

(require "eval-data-directed.rkt")

;(define test-files (directory-list "."))

;(open-input-file "./hello-world.tby")
;(read in)
;(close-input-port in)

#| wtf lol

test-files
(car test-files)

(define (read-dir files)
  (define (read-rec rest)
    (if (null? files)
        'done
        (let ((in 1))
         (begin
           (call-with-input-file (path->string (car rest)) (lambda (in) (read in)))
                 (read-rec (cdr rest))))
    ))
  (read-rec files))

(read-dir test-files)

|#

(define (next-read in)
  (let ((input (read in)))
    (if (eof-object? input)
        'done-reading
        (let ((result (eval input the-global-environment)))
          (begin (displayln "NONCRAP")
                 (displayln result)
                 (next-read in))))
    ))

#|
(call-with-input-file "repl-test/hello-world.scm"
  (lambda (in) (next-read in)))

(call-with-input-file "repl-test/defines.scm"
  (lambda (in) (next-read in)))

(call-with-input-file "repl-test/lambdas.scm"
  (lambda (in) (next-read in)))
|#

(call-with-input-file "repl-test/conds.scm"
  (lambda (in) (next-read in)))
