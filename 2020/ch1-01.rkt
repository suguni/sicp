#lang sicp

;; ex 1.1
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

;; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 7 2)))


;; ex 1.3
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

;; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 -1)
(a-plus-abs-b 1 1)

;; ch 1.1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


;; ex 1.7
(sqrt 9)
(sqrt 2)
(sqrt (square 0.001))
(sqrt (square 9999999999))
;; (sqrt (square 1000000000000000.999999999))

(define (sqrt2 x)
  (sqrt-iter2 1.0 (improve 1.0 x) x))

(define (sqrt-iter2 old new x)
  (if (good-enough2? old new)
      new
      (sqrt-iter2 new (improve new x) x)))

(define (good-enough2? old new)
  (< (abs (- old new)) 0.0001))


(sqrt2 9)
(sqrt2 2)
(sqrt2 (square 0.001))
(sqrt2 (square 9999999999))

;; ex1.8
(define (cube-root x)
  (define (iter old new)
    (if (good-enough2? old new)
        new
        (iter new (improve new))))
  
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (iter 1.0 (improve 1.0)))


(cube-root (* 3 3 3))
(cube-root 0.000001)