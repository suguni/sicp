#lang racket

(define (=number? exp num)
  (and (number? exp) (= exp num)))
      
(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a b)
;  (cond ((=number? a 0) b)
;        ((=number? b 0) a)
;        ((and (number? a) (number? b)) (+ a b))
;        (else (list '+ a b))))
;(define (augend e) (cadr e))               ;; a + b 에서 a
;(define (addend e) (caddr e))              ;; a + b 에서 b
;(define (sum? e)                           ;; 덧셈식인가?
;  (and (pair? e) (eq? (car e) '+)))
;
;(define (make-product a b)
;  (cond ((or (=number? a 0) (=number? b 0)) 0)
;        ((=number? a 1) b)
;        ((=number? b 1) a)
;        ((and (number? a) (number? b)) (* a b))
;        (else (list '* a b))))
;(define (multiplicand e) (cadr e))         ;; a * b 에서 a
;(define (multiplier e) (caddr e))          ;; a * b 에서 b
;(define (product? e)                       ;; 곱셈식인가?
;  (and (pair? e) (eq? (car e) '*)))

;; 미분식
;(define (derive exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (derive (augend exp) var)
;                   (derive (addend exp) var)))
;        ((product? exp)
;         (make-sum (make-product (multiplicand exp) (derive (multiplier exp) var))
;                   (make-product (multiplier exp) (derive (multiplicand exp) var))))
;        (else 
;         (error "unknown expression"))))
;
;(derive '(+ x 3) 'x)
;(derive '(+ (* x x) (* 3 x)) 'x)
;(derive '(* (* x y) (+ x 3)) 'x)


;; ex 2.56
;; a**0 => 1, 0**b => 0, a**1 => a, 1**b => 1
(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? a 0) 0) ;; ??
        ((=number? b 1) a)
        ((=number? a 1) 1) ;; ??
        ((and (number? a) (number? b)) (expt a b))
        (else (list '** a b))))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

;; exponentiation이 추가된 derive
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
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product (make-product n (make-exponentiation u (make-sum n -1)))
                         (derive u var))))
        (else
         (error "unknown expression"))))

;; ex 2.57
;; solution 설명 듣고 복기. 그다지 좋아 보이지 않음.
(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (make-sum-list (list a b)))))
(define (make-sum-list seq)
  (let ((l (length seq)))
    (cond ((= l 1) (car seq))
          ((= l 2) (cons '+ seq))
          (else (list '+ (car seq) (make-sum-list (cdr seq)))))))
(define (augend e) (cadr e))
(define (addend e)
  (if (> (length (cddr e)) 1)
      (make-sum-list (cddr e))
      (caddr e)))
(define (sum? e)                           ;; 덧셈식인가?
  (and (pair? e) (eq? (car e) '+)))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (make-product-list (list a b)))))
(define (make-product-list seq)
  (let ((l (length seq)))
    (cond ((= l 1) (car seq))
          ((= l 2) (cons '* seq))
          (else (list '* (car seq) (make-product-list (cdr seq)))))))
(define (multiplicand e) (cadr e))         ;; a * b 에서 a
(define (multiplier e)
    (if (> (length (cddr e)) 1)
      (make-product-list (cddr e))
      (caddr e)))
(define (product? e)                       ;; 곱셈식인가?
  (and (pair? e) (eq? (car e) '*)))

;; ex 2.58-a prefix 폼을 infix 폼으로 바꾸기
(define (ifx-make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))
(define (ifx-augend e) (car e))               ;; a + b 에서 a
(define (ifx-addend e) (caddr e))              ;; a + b 에서 b
(define (ifx-sum? e)                           ;; 덧셈식인가?
  (and (pair? e) (eq? (cadr e) '+)))

(define (ifx-make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))
(define (ifx-multiplicand e) (car e))         ;; a * b 에서 a
(define (ifx-multiplier e) (caddr e))          ;; a * b 에서 b
(define (ifx-product? e)                       ;; 곱셈식인가?
  (and (pair? e) (eq? (cadr e) '*)))

(define (ifx-derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((ifx-sum? exp)
         (ifx-make-sum (ifx-derive (ifx-augend exp) var)
                       (ifx-derive (ifx-addend exp) var)))
        ((ifx-product? exp)
         (ifx-make-sum (ifx-make-product (ifx-multiplicand exp) (ifx-derive (ifx-multiplier exp) var))
                       (ifx-make-product (ifx-multiplier exp) (ifx-derive (ifx-multiplicand exp) var))))
        (else 
         (error "unknown expression"))))

;(ifx-derive '(x + 3) 'x)
;(ifx-derive '((x * x) + (3 * x)) 'x)
;(ifx-derive '((x * y) * (x + 3)) 'x)

;; ex 2.58-b
;; ???
