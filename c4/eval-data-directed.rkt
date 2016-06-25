#lang racket

(require "../c3/mutable-table-2d.rkt")
(require "../c2/tags.rkt")

(require "expression-helpers.rkt")
(require "simple-expressions.rkt")
(require "expressions.rkt")
(require "environments.rkt")

(provide the-global-environment)
(provide eval)

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (apply-generic op env exp)
  (let ((type-tags (list (type-tag exp))))
    ; (println 'args-are)
    ; (println exp)
    ; (println (contents exp))
    ; (println type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (proc exp env)
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (method-exists? exp)
  (let ((type-tags (list (type-tag exp))))
    (let ((proc (get 'eval type-tags)))
      (if proc
          proc
          false))
    ))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        #|
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))

        ((cond? exp) (eval (cond->if exp) env))
|#
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((method-exists? exp) (apply-generic 'eval env exp))
        ((application? exp)
         (myapply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else (println "WTF WAS THAT")
              )))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; installing stuff for data-directed dispatch
(define (install-lambda)
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (put 'eval '(lambda) eval-lambda)
  'lambda)
(install-lambda)

; conditionals
(define (install-if)
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
  (put 'eval '(if) eval-if)
  'if)
(install-if)

(define (install-cond)
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause)
    (if (cond-arrow-clause? clause)
        (cddr clause)
        (cdr clause)))
  (define (cond-arrow-clause? clause)
    (equal? (cadr clause) '=>))
  (define (cond->if exp env) (expand-clauses (cond-clauses exp) env))
  (define (expand-clauses clauses env)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (let ((seq-exp (sequence->exp (cond-actions first))))
                         (if (cond-arrow-clause? first)
                             ; (make-begin '(println 1))
                             ; seq-exp
                             (begin (println 'yay) (make-begin (make-procedure (eval (cond-predicate first) env) seq-exp env)))
                             seq-exp)
                         )
                       (expand-clauses rest env))))))
  (define (eval-cond exp env)
    (eval (cond->if exp env) env))
  (put 'eval '(cond) eval-cond)
  'cond)
(install-cond)

; sequences
(define (install-begin)
  (define (eval-sequence exps env)
    (cond ((last-exp? exps)
           (eval (first-exp exps) env))
          (else
           (eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env)))
    )
  (put 'eval '(begin) eval-sequence)
  'begin)
(install-begin)

(define (eval-sequence exps env)
  ((get 'eval '(begin)) exps env))

; procedures (functions)
(define (install-function)
  (define (eval-fn exp env)
    (myapply (eval (operator exp) env)
             (list-of-values (operands exp) env)))
  (put 'eval '(call) eval-fn)
  'functions)
(install-function)

; Assignments and definitions
(define (install-eval-assignment)
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (put 'eval '(set!) eval-assignment)
  'assignment)
(install-eval-assignment)

(define (install-eval-definition)
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)
  (put 'eval '(define) eval-definition)
  'definition)
(install-eval-definition)

; ands and ors
(define (install-eval-and)
  (define (eval-and exps env)
    (define (eval-and-rec rest-exprs)
      (let ((result (eval (car rest-exprs))))
        (if (true? result)
            (if (last-exp? rest-exprs)
                result
                (eval-and-rec (cdr rest-exprs)))
            false))
      )
    (eval-and-rec (and-exprs exps))
    )
  (put 'eval '(and) eval-and)
  'and)
(install-eval-and)

(define (install-eval-or)
  (define (eval-or exps env)
    (define (eval-or-rec rest-exprs)
      (let ((result (eval (car rest-exprs))))
        (if (true? result)
            result
            (if (last-exp? rest-exprs)
                false
                (eval-or-rec (cdr rest-exprs)))))
      )
    (eval-or-rec (or-exprs exps))
    )
  (put 'eval '(or) eval-or)
  'and)
(install-eval-or)

; lets
(define (install-let)
  (define (let-exprs exps) (cadr exps))
  (define (let-body exps) (caddr exps))
  (define (eval-let expr env)
    (let ((pairs (let-exprs expr)))
      (let ((vars (map car pairs))
            (exps (map cadr pairs))
            (body (let-body expr)))
        (let ((temp-fn (make-lambda vars body))
              (evaled-exps (map eval exp env)))
          (myapply temp-fn evaled-exps))))
    )
  (put 'eval '(let) eval-let)
  'let)
(install-let)

(define apply-in-underlying-scheme apply)

; apply
(define (myapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        
        ((compound-procedure? procedure)
         ; (println 'procedure)
         ; (println procedure)
         ; (println (procedure-body procedure))
         ; (println (procedure-parameters procedure))
         ; (println arguments)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; driver
(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (println input)
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; (driver-loop)

(eval '(define (add-three x) (+ x 3)) the-global-environment)
(eval '(begin (begin (println 1)) (println 2) (println 3) (println 4)) the-global-environment)
(eval '(add-three 5) the-global-environment)
(eval '(define (add-one fn) (lambda(x) (+ 1 (fn x)))) the-global-environment)
(eval '((add-one add-three) 10) the-global-environment)
