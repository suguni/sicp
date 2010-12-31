#lang racket

;; chapter 1.3.4 프로시저를 만드는 프로시저
(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try g)
    (let ((next (f g)))
      (if (close-enough? g next)
          next
          (try next))))
  (try guess))


;; (fixed-point (lambda (x) (cos x)) 1.0)
;; (fixed-point (lambda (x) (average x (cos x))) 1.0)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
;; (sqrt 2)
(define (sqrt-2 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
;; (sqrt-2 2)

;; newton-method
;; f(x)의 접선식이 0이 되는 점을 두번째 x값으로 하여 풀이하는 방법
;; http://en.wikipedia.org/wiki/Newton%27s_method
(define dx 0.0001)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;(define (newton-method f s)
;  (fixed-point (lambda (x) (- x (/ (f x) ((derive f) x)))) s))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((derive f) x)))))

(define (newton-method f s)
  (fixed-point (newton-transform f) s))

(define (sqrt-nm x)
  (newton-method (lambda (y) (- (* y y) x)) 10.0))

;; (sqrt-nm 2)
;; (newton-method (lambda (x) (- (* x x) 1)) -2.0)

;; sqrt를 계산하는데
;; y => x / y 의 고정점 찾기 문제로 푸는 방법과
;; y^2 - x = 0 를 newton method로 푸는
;; 2가지 방법을 봤음
;; 하지만 두번째 newton method는 결국 y => y - (y^2 - x) / (2 * dy) 의 고정점 찾기 문제임
;; 이를 일반화 하여 fixed-point-of-transform 프로시저로 짜면 아래와 같다.
(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

;; sqrt x / y 고정점 찾기
(define (sqrt-fpt-1 x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

;; sqrt newton method
(define (sqrt-fpt-2 x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x)) newton-transform 1.0))

;; ex 1.40
;; newton-method로 x^3 + a*x^2 + b*x + c = 0 의 해 구하기
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; (x-1)^3
;; (newton-method (cubic -3 3 -1) 2)

;; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

;; 맞바꿈 계산법으로 풀기
;
;(((double (double double)) inc) 5)
;
;(((double (lambda (x) (double (double x)))) inc) 5)
;
;(((lambda (y) ((lambda (x) (double (double x))) ((lambda (z) (double (double z))) y))) inc) 5)
;
;(((lambda (x) (double (double x))) ((lambda (z) (double (double z))) inc)) 5)
;
;(((lambda (x) (double (double x))) (double (double inc))) 5)
;
;((double (double (double (double inc)))) 5)
;
;((double (double (double (lambda (x) (inc (inc x)))))) 5)
;
;((double (double (lambda (a)
;                   ((lambda (x) (inc (inc x)))
;                    ((lambda (x) (inc (inc x))) a))))) 5)
;
;((double (lambda (b)
;           ((lambda (a)
;              ((lambda (x) (inc (inc x)))
;               ((lambda (x) (inc (inc x))) a)))
;            ((lambda (a)
;               ((lambda (x) (inc (inc x)))
;                ((lambda (x) (inc (inc x))) a))) b)))) 5)
;((lambda (c)
;   ((lambda (b)
;      ((lambda (a)
;         ((lambda (x) (inc (inc x)))
;          ((lambda (x) (inc (inc x))) a)))
;       ((lambda (a)
;          ((lambda (x) (inc (inc x)))
;           ((lambda (x) (inc (inc x))) a))) b)))
;    ((lambda (b)
;      ((lambda (a)
;         ((lambda (x) (inc (inc x)))
;          ((lambda (x) (inc (inc x))) a)))
;       ((lambda (a)
;          ((lambda (x) (inc (inc x)))
;           ((lambda (x) (inc (inc x))) a))) b))) c))) 5)
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (b)
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a)))
;     ((lambda (a)
;        ((lambda (x) (inc (inc x)))
;         ((lambda (x) (inc (inc x))) a))) b))) 5))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (a)
;    ((lambda (x) (inc (inc x)))
;     ((lambda (x) (inc (inc x))) a)))
;  ((lambda (a)
;     ((lambda (x) (inc (inc x)))
;      ((lambda (x) (inc (inc x))) a))) 5)))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (a)
;    ((lambda (x) (inc (inc x)))
;     ((lambda (x) (inc (inc x))) a)))
;  ((lambda (x) (inc (inc x)))
;   ((lambda (x) (inc (inc x))) 5))))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (a)
;    ((lambda (x) (inc (inc x)))
;     ((lambda (x) (inc (inc x))) a)))
;  ((lambda (x) (inc (inc x)))
;   (inc (inc 5)))))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (a)
;     ((lambda (x) (inc (inc x)))
;      ((lambda (x) (inc (inc x))) a)))
;   (inc (inc (inc (inc 5))))))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (x) (inc (inc x)))
;      ((lambda (x) (inc (inc x))) (inc (inc (inc (inc 5)))))))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; ((lambda (x) (inc (inc x)))
;      (inc (inc (inc (inc (inc (inc 5))))))))
;
;((lambda (b)
;   ((lambda (a)
;      ((lambda (x) (inc (inc x)))
;       ((lambda (x) (inc (inc x))) a)))
;    ((lambda (a)
;       ((lambda (x) (inc (inc x)))
;        ((lambda (x) (inc (inc x))) a))) b)))
; (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))
;
;((lambda (a)
;   ((lambda (x) (inc (inc x)))
;    ((lambda (x) (inc (inc x))) a)))
; ((lambda (a)
;    ((lambda (x) (inc (inc x)))
;     ((lambda (x) (inc (inc x))) a))) (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))
;
;((lambda (a)
;   ((lambda (x) (inc (inc x)))
;    ((lambda (x) (inc (inc x))) a)))
; ((lambda (x) (inc (inc x)))
;   ((lambda (x) (inc (inc x))) (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))) 
;
;((lambda (a)
;   ((lambda (x) (inc (inc x)))
;    ((lambda (x) (inc (inc x))) a)))
; ((lambda (x) (inc (inc x)))
;   (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))) 
;
;((lambda (a)
;   ((lambda (x) (inc (inc x)))
;    ((lambda (x) (inc (inc x))) a)))
; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))
; 
;((lambda (x) (inc (inc x)))
; ((lambda (x) (inc (inc x))) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))
;
;(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;
;


  
;; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; ((compose square inc) 6) ;; => 49

;; ex 1.43
(define (repeated f i)
  (define (iter g n)
    (if (= n 1)
      g
      (iter (compose g f) (- n 1))))
  (iter f i))
;; ((repeated inc 10) 0) ;; => 10
;; ((repeated square 2) 5) ;; => (square (square 5)) => 625


;; ex 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))


;; ex 1.45
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
;; (cube-root 27)

(define (4th-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y y)))) 1.0))
;; (4th-root 16) => infinite loop

;; y -> x/y^(n-1) 은 몇번 average-damp 해야 하는지 실험하기
(define (nth-root x n)
  (fixed-point ((repeated average-damp n) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 실험 결과.. 패턴이 안보인다
;; n = 4 > 2
;; n = 5 > 2
;; n = 13 > 3
;; n = 22 > 2


;; ex 1.46 - iterative-improve
(define (iterative-improve good? improve)
  (lambda (guess)
    (define (iter v)
      (if (good? v)
          v
          (iter (improve v))))
    (iter guess)))

;; 참고 솔루션과 틀린점은
;; guess 프로시저가 하나의 인자만을 받는다는 점이다.
;; 대신 improve를 정의하는 scope에서 자유변수를 사용하게 된다
;; 이렇게 하지 않고, 2개의 인자를 받는 경우(참고 솔루션) sqrt의 경우 결국 문제를 fixed-point로 변환해야만 한다.
;; 뭐가 맞는건지 ????

;; sqrt
(define (sqrt-ii x)
  (define (good? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve good? improve) 1.0))

(display "iterative-improve sqrt")
(newline)
(sqrt-ii 9)

;; fixed-point
(define (fixed-point-ii f g)
  (define (good? guess)
    (< (abs (- guess (f guess)) 0.00001)))
  ((iterative-improve good? f) g))

(display "iterative-improve fixed-point cos")
(newline)
(fixed-point cos 1.0)
