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
