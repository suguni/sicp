#lang racket

;; (define (sum term a next b)
;;  (if (> a b)
;;      0
;;      (+ (term a) (sum term (next a) next b))))

;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

;; (sum identity 0 inc 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next k) (+ k 2))
  (define (term k) (+ (* 4 (y k)) (* 2 (y (+ k 1)))))
  (* (/ h 3) (+ (y 0) (sum term 1 next n))))

(simpson square 0 1.0 100)
(simpson square 0 1.0 1000)
(integral square 0 1.0 0.01)
(integral square 0 1.0 0.001)

(simpson cube 0 1.0 100)
(simpson cube 0 1.0 1000)
(integral cube 0 1.0 0.01)
(integral cube 0 1.0 0.001)

;; 1.30
(define (product1 term a next b)
  (if (> a b)
      1
      (* (term a) (product1 term (next a) next b))))

;; 1.30
(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define pi
  (product1 (lambda (x) (/ (* x (+ x 2)) (square (+ x 1) )))
            2
            (lambda (n) (+ n 2))
            100))
