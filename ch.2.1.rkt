#lang racket

;; ch 2.1.1

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (/ (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

(define (sub-rat x y)
  (/ (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

(define (mul-rat x y)
  (/ (* (numer x) (numer y))
     (* (denom x) (denom y))))

(define (div-rat x y)
  (/ (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

