#lang racket

;; ch 2.3.2

;; 미분 규칙
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; 대수식 표현하기
(define (variable? x) (symbol? x)) ;변수인가?

(define (same-variable? v1 v2) ;같은 변수인가?
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))
;(define (make-product m1 m2) (list '* m1 m2))
;; 좀 더 깔끔한 표현을 위해 make-sum, make-product 수정
(define (make-sum a1 a2)  ;덧셈식 생성
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)  ;숫자 num과 같은가?
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)  ;곱셈식 생성
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) ;첫째가 + 기호?
  (and (pair? x) (eq? (car x) '+)))

(define (augend s) (cadr s)) ;더하임수 - 두번째 원소

(define (addend s) (caddr s)) ;덧수 - 세번째 원소

(define (product? x) ;첫째가 * 기호?
  (and (pair? x) (eq? (car x) '*)))

(define (multiplicand p) (cadr p)) ;곱하임수 - 두번째 원소

(define (multiplier p) (caddr p)) ;곱수 - 세번째 원소

;> (deriv '(+ x 3) 'x)
;> (deriv '(* x y) 'x)
;> (deriv '(* (* x y) (+ x 3)) 'x)

;; ex 2.58
;; .a
(define (deriv-i exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-i exp)
         (make-sum-i (deriv-i (addend-i exp) var)
                   (deriv-i (augend-i exp) var)))
        ((product?-i exp)
         (make-sum-i
           (make-product-i (multiplier-i exp)
                         (deriv-i (multiplicand-i exp) var))
           (make-product-i (deriv-i (multiplier-i exp) var)
                         (multiplicand-i exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-sum-i a1 a2) (list a1 '+ a2))
(define (make-product-i m1 m2) (list m1 '* m2))
(define (sum?-i x) (and (pair? x) (eq? (cadr x) '+)))
(define (product?-i x) (and (pair? x) (eq? (cadr x) '*)))
(define (augend-i s) (car s))
(define (addend-i s) (caddr s))
(define (multiplicand-i p) (car p))
(define (multiplier-i p) (caddr p))

;> (deriv-i '(x + 3) 'x)
;> (deriv-i '(x * y) 'x)
;> (deriv-i '((x * y) * (x + 3)) 'x)
