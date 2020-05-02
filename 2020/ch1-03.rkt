#lang sicp

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

; (simpson square 0 1.0 100)
; (simpson square 0 1.0 1000)
; (integral square 0 1.0 0.01)
; (integral square 0 1.0 0.001)

; (simpson cube 0 1.0 100)
; (simpson cube 0 1.0 1000)
; (integral cube 0 1.0 0.01)
; (integral cube 0 1.0 0.001)

;; 1.31
(define (product1 term a next b)
  (if (> a b)
      1
      (* (term a) (product1 term (next a) next b))))

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define pi
  (* 4 (product2 (lambda (x) (/ (* x (+ x 2.0)) (square (+ x 1) )))
                 2
                 (lambda (n) (+ n 2))
                 1000)))

;; 1.32
;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum3 term a next b)
  (accumulate + 0 term a next b))

(define (product3 term a next b)
  (accumulate * 1 term a next b))

(* 4 (product3 (lambda (x) (/ (* x (+ x 2.0)) (square (+ x 1) )))
                 2
                 (lambda (n) (+ n 2))
                 1000))

;; 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (pred a)
                  (combiner (term a) result)
                  result))))
  (iter a null-value))

(define (prime? n) #t)

(define (prime-sq-sum a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (search f neg pos)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((mid (/ (+ neg pos) 2.0)))
    (if (close-enough? neg pos)
        mid
        (let ((test-value (f mid)))
          (cond ((positive? test-value) (search f neg mid))
                ((negative? test-value) (search f mid pos))
                (else mid))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))

  (define (next-and-display guess)
    (let ((next (f guess)))
      (display "next: ")
      (display next)
      (newline)
      next))
  
  (define (try guess)
    (let ((next (next-and-display guess)))
      (if (close-enough? next guess)
          next
          (try next))))

  (try first-guess))


; (define (sqrt x) (fixed-point (lambda (y) (/ (+ y (/ x y)) 2.0)) 1.0))

;; 1.35
; (define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0))
; 1.61803398875

;; 1.36
; (define x-power-x (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

;; 1.37
(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (cont-frac2 n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0.0))

; (/ 1.0 (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 14))

;; 1.38
(+ 2 (cont-frac2 (lambda (i) 1.0)
                 (lambda (i) (let ((q (quotient i 3))
                                   (r (remainder i 3)))
                               (if (= r 2) (* 2 (+ q 1)) 1)))
                 100))

(define (tan-x x k)
  (- (cont-frac2 (lambda (i) (- (expt x i)))
                 (lambda (i) (- (* 2 i) 1))
                 k)))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

