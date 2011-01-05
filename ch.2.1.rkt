#lang racket

;; ch 2.1.1

; (define (make-rat n d)
;  (cons n d))

;; n/d가 기약분수가 아닌 경우 처리
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (= g 1)
        (cons n d)
        (cons (/ n g) (/ d g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 기약분수 처리 test
(print-rat (make-rat 2 4)) ;; 1/2
(print-rat (make-rat 2 5)) ;; 2/5

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; ex 2.1
(define (make-rat2 n d)
  (let ((g (gcd n d))
        (s (/ (* n d) (abs (* n d)))))
    (cons (* s (abs (/ n g)))
          (abs (/ d g)))))

;; n의 부호에 관계없이
;; d가 - 이면 d * (-1) , n * (-1)
;; d가 + 이면 d 유지 , n 유지
;; 위에처럼 안해도 된다. --;
;; 나중에 다시 풀어보기!!!

;; make-rat2 test
(make-rat2 2 4)   ;  1 . 2
(make-rat2 2 -4)  ; -1 . 2
(make-rat2 -2 4)  ; -1 . 2

;; ch 2.1.1 요약
;; 유리수를 연산하는 프로시저들을 만들어 봤다.
;; 유리수가 내부적으로 어떻게 구성되는지 알지 못하더라도
;; make-rat, numer, denom을 가지고 유리수의 +, -, *, / 연산을 만들 수 있었다.
;; 또한 내분 구현을 변경(항상 기약분수가 되게 하는 구현 추가)하더라도 유리수를 만들고 쓰는데는 아무런 변화가 없었다.
;;
;; 유리수를 만들기 위해 쌍(pair) 데이터 구조를 사용하였다.
;; cons, car, cdr 프로시저로 쌍 데이터를 조작할 수 있었다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ch 2.1.2

;; ex 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point
     (/ (+ (x-point p1) (x-point p2)) 2)
     (/ (+ (y-point p1) (y-point p2)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point
 (midpoint-segment
  (make-segment
   (make-point 0 0)
   (make-point 10 0)))) ;; (5,0)

(print-point
 (midpoint-segment
  (make-segment
   (make-point 0 0)
   (make-point 10 10)))) ;; (5,5)

;; ex 2.3

;; segment length 계산
(define (length-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (sqrt (+ (sqr (- (x-point p1) (x-point p2)))
             (sqr (- (y-point p1) (y-point p2)))))))

;; 두 개의 segment를 인자로 받음. - 가로변, 세로변 (한 점이 동일하고 서로 수직)
(define (make-rect base height) (cons base height))
(define (base-rect r) (length-segment (car r)))
(define (height-rect r) (length-segment (cdr r)))

(define (length-rect r)
  (let ((w (base-rect r))
        (h (height-rect r)))
  (+ (* w 2) (* h 2))))
(define (area-rect r)
  (let ((w (base-rect r))
        (h (height-rect r)))
    (* w h)))

;; test
(let ((r (make-rect 
          (make-segment (make-point 0 0) (make-point 10 0))
          (make-segment (make-point 0 0) (make-point 0 5)))))
  (newline)
  (display (length-rect r)) ;; 30
  (newline)
  (display (area-rect r)))  ;; 50

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 두 개의 point를 인자로 받음 - left-bottom point, top-right point
(define (make-rect2 p1 p2) (cons p1 p2))
(define (lb-rect r) (car r)) ;; left-bottom point selector
(define (tr-rect r) (cdr r)) ;; top-right point selector
(define (width-rect2 r)
  (- (x-point (tr-rect r))
     (x-point (lb-rect r))))
(define (height-rect2 r)
  (- (y-point (tr-rect r))
     (y-point (lb-rect r))))

(define (length-rect2 r)
  (let ((w (width-rect2 r))
        (h (height-rect2 r)))
    (+ (* w 2) (* h 2))))

(define (area-rect2 r)
  (let ((w (width-rect2 r))
        (h (height-rect2 r)))
    (* w h)))

;; test-2
(let ((r (make-rect2 (make-point 0 0) (make-point 10 10))))
  (newline)
  (display (length-rect2 r)) ;; 40
  (newline)
  (display (area-rect2 r)))  ;; 100

;; 결론: 사각형의 밑변 길이와 높이를 가져오는 selector만 있으면
;; 면적과 둘레길이를 계산하는 프로시저는 그대로 사용할 수 있다.

;; ch 2.1.3

;; ex 2.4

(define (cons-1 x y)
  (lambda (m) (m x y)))

(define (car-1 z)
  (z (lambda (p q)  p)))

;; a - (car (cons x y)) => x 확인하기.
; > (car (lambda (m) (m x y)))
; > ((lambda (m) (m x y)) (lambda (p q) p))
; > ((lambda (p q) p) x y)
; > x

;; b - cdr 정의하기
(define (cdr-1 z)
  (z (lambda (p q) q)))

(car-1 (cons-1 'x 'y)) ;; 'x
(cdr-1 (cons-1 'x 'y)) ;; 'y

;; ex 2.5
;; 수와 산술 연산만으로 정수 쌍 표현하기
(define (cons-2 a b) (* (expt 2 a) (expt 3 b)))
;; (define (car-2 z) (some-process z 2))
;; (define (cdr-2 z) (some-process z 3))
;; some-process는 z를 2 또는 3으로 나눠서 나머지가 0가 아닌때까지의 나누는 회수를 반환하는 프로시저다.
;; 근데... 이렇게 하는게 산술 연산만으로 정수 쌍 표현한다고 할 수 있나?
;; 아래 divide-count 프로시저가 구현한 것.

;(define (divide-count v i)
;  (if (zero? (remainder v i))
;      (+ 1 (divide-count (/ v i) i))
;      0))

(define (divide-count value divisor)
  (define (iter v n)
    (if (zero? (remainder v divisor))
        (iter (/ v divisor) (+ n 1))
        n))
  (iter value 0))

(define (car-2 z) (divide-count z 2))
(define (cdr-2 z) (divide-count z 3))

;; test
(car-2 (cons-2 20 3)) ;; 10
(cdr-2 (cons-2 2 30)) ;; 9

;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;; one, two, + 프로시저 정의하기

;; 먼저 (add-1 zero)를 맞바꿈 계산범으로 풀어보자.
(add-1 zero)


;; ch 2.1.4

(newline)
(print "ch 2.1.4 interval")
(newline)

;; ex 2.7
(define (make-interval a b) (cons a b))

;; (define (lower-bound x) (car x))
;; (define (upper-bound x) (cdr x))

;; 위와같이 짤 수도 있는데,
;; 위의 make-interval 프로시저에서 a와 b값을 체크하는 프로세스가 없으므로 아래처럼 짜는게 좋을 듯.
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; test interval
(define (print-interval x)
  (newline)
  (display (lower-bound x))
  (display "~")
  (display (upper-bound x)))

(define (print-interval2 x)
  (display (lower-bound x))
  (display "~")
  (display (upper-bound x)))

(print "ex 2.7 interval data test")
(print-interval (make-interval 2 6)) ;; 2~6
(print-interval (make-interval 6 2)) ;; 2~6
(print-interval (add-interval (make-interval 2 6)
                              (make-interval 1 4))) ;; 3~10
(print-interval (mul-interval (make-interval 2 6)
                              (make-interval 1 4))) ;; 2~24
(print-interval (div-interval (make-interval 2 6)
                              (make-interval 1 4))) ;; 1/2~6

;; ex2.8
;;; 구간의 뺄샘: 두 구간의 하한끼리, 상한끼리 뺀 값을 새로운 하한과 상한으로 하는 구간 삑X!!!
;(define (sub-interval x y)
;  (make-interval (- (lower-bound x) (lower-bound y))
;                 (- (upper-bound x) (upper-bound y))))
;
;;; sub-interval test
;(print-interval (sub-interval (make-interval 10 20)
;                              (make-interval 5 10))) ;; 5~10
;(print-interval (sub-interval (make-interval 0 10)
;                              (make-interval -10 20))) ;; -10~10 맞는건가???

;; 위 아닌듯... --;

;; solution(http://community.schemewiki.org/?sicp-ex-2.8)에 있는거처럼
;; x-y 를 x + (-y)로 보고 짜 보면
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))
;; sub-interval test
(newline)
(print "================================================================================")
(newline)
(print "ex 2.8 sub-interval test")
(print-interval (sub-interval (make-interval 10 20)
                              (make-interval 5 10))) ;; 0~15
(print-interval (sub-interval (make-interval 0 10)
                              (make-interval -10 20))) ;; -20~20


;; ex 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;; 아래와 같은 구간 x1, x2가 있을 때
;; 구간 x1: l1, u1, w1 = (u1-l1)/2
;; 구간 x2: l2, u2, w2 = (u2-l2)/2

;; 구간의 덧셈 (x1 + x2)
;; lower bound : l1 + l2
;; upper boudn : u1 + u2
;; width       : w3 = ((u1+u2) - (l1+l2)) / 2 = ((u1-l1) + (u2-l2)) / 2
;;                  = (u1-l1) / 2 + (u2-l2) / 2 = w1 + w2                        OK!

;; 구간의 뺄셈 (x1 - x2)
;; lower bound : l1 + (-u2)
;; upper bound : u1 + (-l2)
;; width       : w3 = ((u1-l2) - (l1-u2)) / 2 = ((u1-l1) - (u2-l2)) / 2
;;                  = (u1-l1) / 2 - (u2-l2) / 2 = w1 - w2                      OK!

;; 구간의 곳셈이 그렇지 못한 보기
(let ((int1 (make-interval 0 10))
      (int2 (make-interval 5 10)))
  (let ((mul-int (mul-interval int1 int2))
        (div-int (div-interval int1 int2)))
    (newline)
    (print "================================================================================")
    (newline)
    (print "ex 2.9) interval */ op. & width test")
    (newline)
    (print "int1:      ")
    (print-interval2 int1)
    (print " - width: ")
    (print (width-interval int1))
    (newline)
    (print "int2:      ")
    (print-interval2 int2)
    (print " - width: ")
    (print (width-interval int2))
    (newline)
    (print "int1*int2: ")
    (print-interval2 mul-int)
    (print " - width: ")
    (print (width-interval mul-int))
    (newline)
    (print "int1/int2: ")
    (print-interval2 div-int)
    (print " - width: ")
    (print (width-interval div-int))))

;; ex 2.10
;; 문제에 모호한점 있음.
;;
;; 문제에서 "0이 들어 있는 구간 값으로 나눠야 할 때" 라는 말은 구간이 0에 걸쳐 있다는 말로 해석됨
;; 즉, -1 ~ 1 구간처럼.
;; 원서에도 "it is not clear what it means to divide by an interval that spans zero" 라고 되어 있는데,
;; 원서와 번역본이 동일한 느낌.
;;
;; div-interval 프로시저에서 문제가 되는 케이스는
;; 나누는 구간(div-interval 프로시저에서 y 인자)의 lower나 upper가 0인 경우이다.
;; 따라서, 0과 관련된 부분은 구간의 upper나 lower에 0이 존재한다는 것으로 해석하고 풀이함.
(define (div-interval2 x y)
  (if (= (* (upper-bound y) (lower-bound y)) 0)
      (error "divide by 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; error! case
;; (div-interval2 (make-interval 5 10) (make-interval 0 5))

;; ex 2.11
;; mul-interval 다시 짜기.
;; 입력 구간의 부호에 따른 결과 구간의 L, U table
;; | Lx | Ux | Ly | Uy | L     | U
;; | +  | +  | +  | +  | Lx*Ly | Ux*Uy
;; | +  | +  | -  | +  | Ux*Ly | Ux*Uy
;; | +  | +  | -  | -  | Ux*Ly | Lx*Uy
;; | -  | +  | +  | +  | Lx*Uy | Ux*Uy
;; | -  | +  | -  | +  | Lx*Uy | Ux*Uy  2가지 경우 나올 수 있음.
;;                       Ux*Ly | Lx*Ly
;; | -  | +  | -  | -  | Ux*Ly | Lx*Ly
;; | -  | -  | +  | +  | Lx*Uy | Ux*Ly
;; | -  | -  | -  | +  | Lx*Uy | Lx*Ly
;; | -  | -  | -  | -  | Ux*Uy | Lx*Ly

(define (mul-interval2 x y)
  (let ((Lx (lower-bound x))
        (Ux (upper-bound x))
        (Ly (lower-bound y))
        (Uy (upper-bound y)))
    (cond ((and (>= Lx 0) (>= Ux 0) (>= Ly 0) (>= Uy 0)) (make-interval (* Lx Ly) (* Ux Uy)))
          ((and (>= Lx 0) (>= Ux 0) (<  Ly 0) (>= Uy 0)) (make-interval (* Ux Ly) (* Ux Uy)))
          ((and (>= Lx 0) (>= Ux 0) (<  Ly 0) (<  Uy 0)) (make-interval (* Ux Ly) (* Lx Uy)))
          ((and (<  Lx 0) (>= Ux 0) (>= Ly 0) (>= Uy 0)) (make-interval (* Lx Uy) (* Ux Uy)))
          ((and (<  Lx 0) (>= Ux 0) (<  Ly 0) (>= Uy 0)) (make-interval (min (* Lx Uy) (* Ux Ly))
                                                                        (max (* Ux Uy) (* Lx Ly))))
          ((and (<  Lx 0) (>= Ux 0) (<  Ly 0) (<  Uy 0)) (make-interval (* Ux Ly) (* Lx Ly)))
          ((and (<  Lx 0) (<  Ux 0) (>= Ly 0) (>= Uy 0)) (make-interval (* Lx Uy) (* Ux Ly)))
          ((and (<  Lx 0) (<  Ux 0) (<  Ly 0) (>= Uy 0)) (make-interval (* Lx Uy) (* Lx Ly)))
          ((and (<  Lx 0) (<  Ux 0) (<  Ly 0) (<  Uy 0)) (make-interval (* Ux Uy) (* Lx Ly))))))
          
(let ((x-P (make-interval 5 10))
      (x-S (make-interval -5 5))
      (x-M (make-interval -10 -5))
      (y-P (make-interval 2 4))
      (y-S (make-interval -2 2))
      (y-M (make-interval -4 -2)))
  (newline)
  (print "================================================================================")
  (newline)
  (print "ex 2.11 case test")
  (print-interval (mul-interval2 x-P y-P))  ;; 10~40
  (print-interval (mul-interval2 x-P y-S))  ;; -20~20
  (print-interval (mul-interval2 x-P y-M))  ;; -40~-10
  (print-interval (mul-interval2 x-S y-P))  ;; -20~20
  (print-interval (mul-interval2 x-S y-S))  ;; -10~10
  (print-interval (mul-interval2 x-S y-M))  ;; -20~20
  (print-interval (mul-interval2 x-M y-P))  ;; -40~-10
  (print-interval (mul-interval2 x-M y-S))  ;; -20~20
  (print-interval (mul-interval2 x-M y-M))) ;; 10~40

;; 위의 경우 구간별 U와 L의 부호를 확인하는 부분들이 반복되고 있음.
;; cond가 아닌 if 문으로 하면 확인 프로세스를 줄일 수 있으나 코드 보기가 어려워져서 안 좋음. ^^;

;; ex 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

;; ex 2.13
;; 두 구간의 곱셈 결과 구간의 tolerance를 계산하는 간단한 식.
;; 단, t1, t2는 모두 양수이고 작은값이라고 가정한다.
;; int1(c1, t1) * int2(c2, t2) = int3(c3, t3) 이면
;; t3 = t1 + t2 이다.
;; 왜? 경험적으로 해 보면 그렇게 나온다.
(percent (mul-interval (make-center-percent 10 5)
                       (make-center-percent 20 2))) ;; => 약 7


;; 병렬 저항 계산식
;(define (par1 r1 r2)
;  (div-interval (mul-interval r1 r2)
;                (add-interval r1 r2)))
;
;(define (par2 r1 r2)
;  (let ((one (make-interval 1 1)))
;    (div-interval one
;                  (add-interval (div-interval one r1)
;                                (div-interval one r2)))))

;; ex 2.14
;; A / A, A / B
(define I-A (make-center-percent 10 0.1))
(define I-B (make-center-percent 5 0.2))
(newline)
(div-interval I-A I-A) ;; => '(0.998 . 1.002), 1이어야 하는데, 그렇지 안음.
                       ;; 그런데 구간 연산에서 1의 의미는 무엇인가? - 구간  * 연산에 대한 항등원
(div-interval I-A I-B) ;; ???

;; <문제점 해석>
;; A * B = C <=> A = C / A 성립하지 않음.
;; (mul-interval (make-interval 1 2) (make-interval 2 4)) => '(2 . 8)
;; (div-interval (make-interval 2 8) (make-interval 2 4)) => '(0.5 . 4.0) => '(1 . 2)가 나와야 하는것 아닌가?
;; A / A = I 성립하지 않음.(I는 항등원으로 (1 . 1)로 본다.)
;; (div-interval (make-interval 2 4) (make-interval 2 4)) => '(0.5 . 2.0)

;; 만일 ex 2.13의 가정을 바탕으로 다음과 같이 짠다면?
(define (mul-interval-new i1 i2)
  (make-center-percent (* (center i1) (center i2))
                       (+ (percent i1) (percent i2))))
(define (div-interval-new i1 i2)
  (make-center-percent (/ (center i1) (center i2))
                       (- (percent i1) (percent i2))))

(define icp1 (make-center-percent 10 1))
(define icp2 (make-center-percent 5 1))
(define icp3 (mul-interval-new icp1 icp2))
(define icp4 (div-interval-new icp3 icp1)) ;; == icp2 ???

(newline)
(center icp3)
(percent icp3)
(center icp4) ;; 5
(percent icp4) ;; 1

;; mul-interval-new, div-interval-new 는 위에서 언급한 문제점이 해소됨.
;; 저항 문제를 다시 계산해 보면
(define (par1 r1 r2)
  (div-interval-new (mul-interval-new r1 r2)
                    (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval-new one
                      (add-interval (div-interval-new one r1)
                                    (div-interval-new one r2)))))

(newline)
(let ((p1 (par1 icp1 icp2))
      (p2 (par2 icp1 icp2)))
  (display (center p1))
  (newline)
  (display (percent p1))
  (newline)
  (display (center p2))
  (newline)
  (display (percent p2)))

;; 저항 계산 문제 해결됨. !!

;; ex 2.15
;; 동일한 대수식이 다른 결과를 가진다면 그 연산의 정의가 잘못된 것임
;; 문제에서는 값이 아닌 구간의 연산이므로 연산 과정이 많으면 많을수록 오류가 점점 커진다고 볼 수 있다.

;; ex 2.16
;; ex 2.14에서 만든 mul-interval-new, div-interval-new 사용.
