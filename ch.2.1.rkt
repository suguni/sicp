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
