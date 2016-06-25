#lang racket

(require "primitives.rkt")
(require "utils.rkt")

(provide setup-environment)
(provide set-variable-value!)
(provide define-variable!)
(provide lookup-variable-value)
(provide extend-environment)
(provide sub-environment)

(define (cons a b) (mcons a b))
(define (car a) (mcar a))
(define (cdr a) (mcdr a))
(define (set-car! a b) (set-mcar! a b))
(define (set-cdr! a b) (set-mcdr! a b))

(define (mlength s)
  (if (null? s)
      0
      (+ 1 (mlength (mcdr s)))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (let ((mvars (list->mlist vars))
        (mvals (list->mlist vals)))
  (if (= (mlength mvars) (mlength mvals))
      (cons (make-frame mvars mvals) base-env)
      (if (< (mlength mvars) (mlength mvals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals)))))

(define (sub-environment base-env) (extend-environment null null base-env))

; if env empty
; if var is found
; if vars is empty

(define (traverse-env if-env-empty if-var-found if-vars-empty var env)
  (define (env-loop env)
    (define (scan vars vals frame)
      (cond ((null? vars) (if-vars-empty frame))
            ((eq? var (car vars)) (if-var-found vals))
            (else (scan (cdr vars) (cdr vals) frame))))
    (if (eq? env the-empty-environment)
        (if-env-empty)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)
                frame))))
  (env-loop env))

(define (define-variable! var val env)
  (traverse-env (lambda() 'wtf)
                (lambda (vals) (set-car! vals val))
                (lambda (frame) (add-binding-to-frame! var val frame))
                var env))

(define (lookup-variable-value var env)
  (traverse-env (lambda() (error "Unbound variable" var))
                (lambda (vals) (car vals))
                (lambda (frame) (lookup-variable-value var (enclosing-environment env)))
                var env))

(define (set-variable-value! var val env)
  (traverse-env (lambda() (error "Unbound variable: SET!" var))
                (lambda (vals) (set-car! vals val))
                (lambda (frame) (set-variable-value! var val (enclosing-environment env)))
                var env))

(define (old-define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (old-lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (old-set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (old-define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; test shit
#|
(define parent-env (extend-environment (mcons 'car (mcons 'lon '())) (mcons 'caidit (mcons 'vcl '())) the-global-environment))
parent-env
(lookup-variable-value 'car parent-env)
(lookup-variable-value 'car the-global-environment)
(set-variable-value! 'car 'caidis parent-env)
(lookup-variable-value 'car parent-env)
(lookup-variable-value 'car the-global-environment)
|#