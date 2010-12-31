#lang racket

(define (search f neg-point pos-point)
  (define (good-enough? point)
    (< (abs (f point)) 0.01))
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (average x y)
    (/ (+ x y) 2))
  (let ((mid-point (average pos-point neg-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((mid-value (f mid-point)))
          (cond ((negative? mid-value)
                (search f mid-point pos-point))
               ((positive? mid-value)
                (search f neg-point mid-point))
               (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else (error "Value are not of opposite sign" a b)))))
  
;; (search (lambda (x) (+ (* x x) 1)) -1.0 3.0) 
;; (half-interval-method (lambda (x) (- (* x x) 1)) -0.5 2.0)

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (let ((next (f guess)))
    (if (close-enough? guess next)
        guess
        (fixed-point f next))))

;; (fixed-point (lambda (x) (* x x)) 2)

(define (fixed-point-2 f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try g)
    (let ((next (f g)))
      (if (close-enough? g next)
          next
          (try next))))
  (try guess))

;; (fixed-point-2 (lambda (x) (* x x)) 1.5) -> 발산
;; y=x^2의 경우 1보다 큰값이 첫번째 guess로 들어가면 발산한다.


;; 반복한다.
;; (define (sqrt-by-fp n)
;;  (fixed-point-2 (lambda (x) (/ n x)) 1.4))

(define (average a b)
  (/ (+ a b) 2))

;; 반복하지 않도록 n/x가 아닌 x와 n/x의 평균을 계산
(define (sqrt-by-fp n)
  (fixed-point-2 (lambda (x) (average x (/ n x))) 1.0))


;; ex 1.36
(define (fixed-point-3 f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (define (try g)
    (display g)
    (newline)
    (let ((next (f g)))
      (if (close-enough? g next)
          next
          (try next))))
  (try guess))

(define (xsx-1)
  (fixed-point-3 (lambda (x) (/ (log 1000) (log x))) 2.0))
;; 진행-24회
;; 2.0
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.7598507024015393
;; 5.2158437849258945
;; 4.182207192401398
;; 4.82776509834459
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851

(define (xsx-2)
  (fixed-point-3 (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
;; 진행-8회
;; 2.0
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136243
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624

;; ex1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (<= i k)
        (/ (n i) (+ (d i) (iter (+ i 1))))
        0))
  (iter 1))

(define phi (/ (+ 1 (sqrt 5)) 2))

(display "phi/i                       : ")
(/ 1 phi)

;; (a) k가 11에서 1/phi의 소수점 4자리에 맞는다.
(display "recursive process cont-frac : ")
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

;; (b) 반복하는 프로세스로 짜기
(define (cont-frac-2 n d k)
  (define (iter i result)
    (if (>= i 1)
        (iter (- i 1) (/ (n i) (+ (d i) result)))
        result))
  (iter k 0))

(display "iterative process cont-frac : ")
(cont-frac-2 (lambda (i) 1.0)
             (lambda (i) 1.0)
             11)

;; ex 1.38
;; e-2를 연속분수(ex 1.37)로 계산하기
;; N = 1, D = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
(define (e-cf k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (+ (quotient i 3) 1))
        1))
  (cont-frac-2 n d k))

(begin
  (display "e          : ")
  (exp 1))

(begin
  (display "e-cf (@20) : ")
  (+ 2 (e-cf 20)))

;; ex 1.39
;; tan x를 연속분수로 계산하기
;; N = x, -x^2, -x^2, -x^2,...
;; D = 1, 3, 5, 7, 9, 11, ...

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (+ i (- i 1)))
  (cont-frac-2 n d k))

(display "tan 45deg         : ")
(tan (/ pi 4))

(display "tan-cf 45deg (@9) : ")
(tan-cf (/ pi 4) 10)

;; 1.3.3 review
;; 차수 높은 프로시저의 활요에 대해 알아본 section
