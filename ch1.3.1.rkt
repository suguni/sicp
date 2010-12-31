#lang racket

; 1.3.1 프로시저를 인자로 받는 프로시저

;; a ~ b 까지 정수 합.
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;; 정해진 넓이 속 정수를 모두 세제곱하여 더하기
(define (cube x)
  (* x x x))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;; 1 / (1 * 3) + 1 / (5 * 7) + 1 / (9 * 11) + ...
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; SUM
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; sum-cubes의 sum 이용 버전
(define (inc x) (+ x 1))
(define (sum-cubes-2 a b)
  (sum cube a inc b))

;; sum-integers의 sum 이용 버전
(define (identify x) x)
(define (sum-integers-2 a b)
  (sum identify a inc b))

;; pi-sum의 sum 이용 버전
(define (pi-sum-2 a b)
  (define (term x)
    (/ 1.0 (* x (+ x 2))))
  (define (next x)
    (+ x 4))
  (sum term a next b))

;; integral
(define (integral f a b dx)
  (define (next x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) next b) dx))

;; ex 1.29
;; Simpson's rule, http://en.wikipedia.org/wiki/Simpson%27s_rule
;;
;; Simpson's rule은 f(x)를 a~b로 정적분하는 규칙으로,
;; a, (a+b)/2, b의 3점을 지나는 Lagrange polynomial interpolation(2차 방정식) 하고, 이를 적분한 식으로부터 면적을 계산한다.
;; f(x)의 a~b 구간은 2차 방정식으로 interpolation한 함수를 P(x)라고 하고, a와 b의 중간점((a+b)/2)을 m이라 하면,
;; P(x)의 a~b 구간에서 적분값은 ((b - a) / 6) * (f(a) + 4f(m) + f(b)) 이다.
;;
;; f(x)의 A~B 구간을 짝수인 n 구간(h=(B-A)/n)으로 나누고(나눈 위치를 x0(=A), x1, x2, ... xn(=B) 이라고 하고), 2 구간씩을 Simpson's rule에 따라 계산하면
;; 정적분 값은 h/3 * (f(x0) + 4*f(x1) + 2*f(x2) + 4*f(x3) + 2*f(x4) + ... + 4*f(xn-1) + f(xn)) 이 되고 이를 SUM으로 표현하면,
;; h/3 * (f(x0) + 4*SUM(f(x2i-1)) + 2*SUM(f(x2)) + f(n)) 이 된다.
;; 

;; 아래 2가지 방법의 결과가 틀리다.

;; 이건 a부터 시작해서 h만큼 늘어가는 방법
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (term x) ; x = a + kh
    (* (cond ((or (= x a) (= x b)) 1)
             ((odd? (/ (- x a) h)) 4)
             (else 2))
       (f x)))
  (define (next x)
    (+ x h))
  (* (/ h 3.0) (sum term a next b)))
;; (integral-simpson cube 0 10 100) => 2500.0

;; 이건 n만큼 돌린다고 생각하고 짠것. 
(define (integral-simpson-2 f a b n)
  (define h (/ (- b a) n))
  (define (term x)
    (* (cond ((or (= x 0) (= x n)) 1)
             ((odd? x) 4)
             (else 2))
       (f (+ a (* x h)))))
  (* (/ h 3.0) (sum term 0 inc n)))
;; (integral-simpson-2 cube 0 10 100) => 2500.0

;; ex 1.30

;; recursive process SUM
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a) (sum term (next a) next b))))

;; iterative process SUM 짜기
(define (sum-i term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ result (term x)))))
  (iter a 0))

;; iterative process sum 테스트
(define (sum-cubes-i a b)
  (sum-i cube a inc b))

;; ex 1.31
;; product를 차수높은 프로시저로 짜기
;; a - recursive process
(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a) (product-r term (next a) next b))))
;; (product-r identify 1 inc 10) => 3628800

;; b - iterative process
(define (product-i term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* result (term x)))))
  (iter a 1))
;; (product-i identify 1 inc 10) => 3628800

;; c - pi/4 계산하기
;; pi/4 = (2/3)*(4/3)*(4/5)*(6/5)*(6/7)*...
;; 분자는 2 4 4 6 6 8 8 ...
;; i가 홀수면 i+1, 짝수면 i+2
;; 분모는 3 3 5 5 7 7 9 9 ...
;; i가 홀수면 i+2, 짝수면 i+1
(define (pi-over-4 n)
  (define (term a)
    (/ (if (odd? a) (+ a 1.0) (+ a 2))
       (if (odd? a) (+ a 2.0) (+ a 1))))
  (product-i term 1 inc n))
;; (pi-over-4 1000000) => 0.7853985560957135
;; (/ pi 4) => 0.7853981633974483

;; ex 1.32
;; accumulate 짜기
(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-r combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (term x)))))
  (iter a null-value))

(define (product-a-r term a next b)
  (accumulate-r * 1 term a next b))
;; (product-a-r identify 1 inc 10) => 3628800

(define (product-a-i term a next b)
  (accumulate-i * 1 term a next b))
;; (product-a-i identify 1 inc 10) => 3628800

(define (sum-a-r term a next b)
  (accumulate-r + 0 term a next b))
;; (sum-a-r identify 0 inc 10) => 55

(define (sum-a-i term a next b)
  (accumulate-i + 0 term a next b))
;; (sum-a-i identify 0 inc 10) => 55

;; ex 1.33
;; filtered-accumulate 짜고 책에 있는 a, b 짜기
(define (filtered-accumulate-r combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate-r combiner null-value filter term (next a) next b))))

(define (filtered-accumulate-i combiner null-value filter term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (if (filter x) (term x) null-value)))))
  (iter a null-value))
 
;(filtered-accumulate-r + 0 even? identify 0 inc 10) ; => 30
;(filtered-accumulate-r + 0 odd? identify 0 inc 10)  ; => 25
;(filtered-accumulate-i + 0 even? identify 0 inc 10) ; => 30
;(filtered-accumulate-i + 0 odd? identify 0 inc 10)  ; => 25

(define (square x)
  (* x x))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder a b) 0))
  (define (square x)
    (* x x))
  (define (find-divisor value test-div)
    (cond ((divides? value test-div) test-div)
          ((> (square test-div) value) value)
          (else (find-divisor value (+ test-div 1)))))
  (find-divisor n 2))

(define (sum-prime-square a b)
  (filtered-accumulate-i + 0 prime? square a inc b))

;; (sum-prime-square 2 10) => 2*2 + 3*3 + 5*5 + 7*7 = 87

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

(define (product-seed n)
  (define (filter x)
    (= (GCD x n) 1))
  (filtered-accumulate-i * 1 filter identify 1 inc (- n 1)))
;; (product-seed 10) ;; => (* 1 3 7 9) = 189

