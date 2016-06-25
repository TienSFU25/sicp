;#lang scheme

(if (positive? -5)
    (println "sai con me may roi")
    (if (zero? -5)
        (println "ngu vai cut")
        (if (positive? 5)
            'correct
            (println "deo con gi de noi"))))

(cond
  ((positive? -5) (error "doesn't get here"))
  ((zero? -5) (error "doesn't get here, either"))
  ((positive? 5) (println "vaicut") 'correct-again))

;(cond
;  ((positive? 5) => lambda(x) (println x)))

(cond ((positive? 5) => (lambda (x) (println x)))
      (else false))