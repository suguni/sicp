#lang sicp

;; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 7 2)))


;; 1.3
(define (square x) (* x x))

(define (square-sum x y)
  (+ (square x) (square y)))

(define (min x y z)
  (if (> x y)
      (if (> y z) z y)
      (if (> x z) z x)))

(define (larger-square-sum x y z)
  (let ((m (min x y z)))
    (cond ((= m x) (square-sum y z))
          ((= m y) (square-sum x z))
          ((= m z) (square-sum x y)))))

(larger-square-sum 1 2 3)
(larger-square-sum 3 1 2)
(larger-square-sum 2 3 1)
(larger-square-sum 3 2 1)
(larger-square-sum 1 3 2)
(larger-square-sum 2 1 3)

;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 -1)
(a-plus-abs-b 1 1)