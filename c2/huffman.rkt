#lang racket
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define deadend 'd)
(define (deadend? object) (eq? object deadend))

(define (encode-symbol symbol tree)
  (define (encode-rec symbol tree ans)
    (if (leaf? tree)
        (if (eq? (symbol-leaf tree) symbol)
            ans
            deadend)
        (let ((leftlist (append ans '(0)))
              (rightlist (append ans '(1))))
          (let ((leftans (encode-rec symbol (left-branch tree) leftlist)))
            (if (deadend? leftans)
                (encode-rec symbol (right-branch tree) rightlist)
                leftans)))))
  (encode-rec symbol tree '())
  )

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)
(encode '(A D A B B C A) sample-tree)

(define (successive-merge leaves)
  (define (insert thelist element)
    (if (null? thelist)
        (list element)
        (if (< (weight (car thelist)) (weight element))
            (cons (car thelist) (insert (cdr thelist) element))
            (cons element thelist))))
  (cond ((null? leaves) '())
        ((> (length leaves) 1)
         (let ((apair (make-code-tree (car leaves) (cadr leaves))))
                          (let ((nextleaves (adjoin-set apair (cddr leaves))))
                            (successive-merge nextleaves))))
        (else (car leaves))))

'(((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define samplepairs (list '(A 4) '(B 2) '(C 1) '(D 1)))

;(make-leaf-set samplepairs)
(define mytree (generate-huffman-tree samplepairs))
(print sample-tree)
(newline)
(print sample-message)
(newline)
(decode sample-message mytree)
(decode sample-message sample-tree)

(print "Rock song stuff")
(newline)

(define rockpairs (list '(A 2) '(GET 2) '(SHA 3) '(WAH 1) '(BOOM 1) '(JOB 2) '(NA 16) '(YIP 9)))
(define your-dads-tree (generate-huffman-tree rockpairs))
(print your-dads-tree)
(newline)
(define rockmessage '(GET A JOB SHA NA NA NA NA NA GET A JOB WAH YIP YIP YIP YIP SHA BOOM))
(define encodedrock (encode rockmessage your-dads-tree))
(decode encodedrock your-dads-tree)