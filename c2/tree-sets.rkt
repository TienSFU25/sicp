#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define nil '())
(define tree1 (make-tree 5 (list 4 (list -3 nil nil) nil) (list 8 nil nil)))

(define tree2 (make-tree 3 (list 2 (list 1 nil nil) nil) (list 7 nil nil)))

(adjoin-set 6 tree2)
(tree->list-1 tree2)
(tree->list-2 tree2)

(define (list->tree elements)
  (car (partial-tree elements (- (length elements) 0))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(list->tree (list 3 4 2 1 5))

(define (merge-union list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (let ((f1 (car list1))
                    (f2 (car list2)))
                (cond
                  ((= f1 f2) (cons f1 (merge-union (cdr list1) (cdr list2))))
                  ((< f1 f2) (cons f1 (merge-union (cdr list1) list2)))
                  ((> f1 f2) (cons f2 (merge-union list1 (cdr list2))))                  
                      )))))

(define (union-set tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
        (list2 (tree->list-1 tree2)))
    (let ((merged-list (merge-union list1 list2)))
      (list->tree merged-list))))

(print 'wtf)
(newline)
(union-set tree1 tree2)