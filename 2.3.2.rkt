#lang racket

(define (=number? exp num)
  (and (number? exp) (= exp num)))
      
(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))
(define (augend e) (cadr e))               ;; a + b 에서 a
(define (addend e) (caddr e))              ;; a + b 에서 b
(define (sum? e)                           ;; 덧셈식인가?
  (and (pair? e) (eq? (car e) '+)))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))
(define (multiplicand e) (cadr e))         ;; a * b 에서 a
(define (multiplier e) (caddr e))          ;; a * b 에서 b
(define (product? e)                       ;; 곱셈식인가?
  (and (pair? e) (eq? (car e) '*)))

;; 미분식
(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (derive (augend exp) var)
                   (derive (addend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplicand exp) (derive (multiplier exp) var))
                   (make-product (multiplier exp) (derive (multiplicand exp) var))))
        (else 
         (error "unknown expression"))))

(derive '(+ x 3) 'x)
(derive '(+ (* x x) (* 3 x)) 'x)
(derive '(* (* x y) (+ x 3)) 'x)