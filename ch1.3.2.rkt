#lang racket

;; chapter 1.3.2

(define (square x)
  (* x x))

;; ex 1.34
(define (f g)
  (g 2))

;; (f square)
;; (square 2)
;; 4

;; (f (lambda (z) (* z (+ z 1))))
;; ((lambda (z) (* z (+ z 1))) 2)
;; (* 2 (+ 2 1))
;; 6

;; (f f)
;; (f 2)
;; (2 2) ==> error, f 프로시저의 인자는 프로시저야 함
