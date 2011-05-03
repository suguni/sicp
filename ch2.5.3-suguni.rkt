(load "support/common.rkt")
(load "support/data-directed-prog.rkt")

;; generic arithmetic operators
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  ;; equ?
  (define (equ? x y) (eq? x y))
  (put 'equ? '(scheme-number scheme-number) equ?)
  
  ;; =zero?
  (put '=zero? '(scheme-number) zero?) ;; ??? (zero? x)
  
  ;; ex 2.81
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  
  'done)


(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational package
(define (install-rational-package)
  
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (if (not (zero? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (cons n d)))
  
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
  
  (define (tag x)
    (attach-tag 'rational x))
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  ;; equ?
  (define (equ? x y)
    (and (eq? (numer x) (numer y))
         (eq? (denom x) (denom y))))
  (put 'equ? '(rational rational) equ?)
  
  ;; =zero?
  (put '=zero? '(rational)
       (lambda (x) (and (zero? (numer x)) (not (zero? (denom x))))))
  
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; 직각좌표 package
(define (install-rectangular-package)
  
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  
  ;; utility
  (define (tag x) (attach-tag 'rectangular x))
  
  ;; interface
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  ;; equ?
  (define (equ? x y)
    (and (eq? (real-part x) (real-part y))
         (eq? (imag-part x) (imag-part y))))
  (put 'equ? '(rectangular rectangular) equ?)
  
  ;; =zero?
  (put '=zero? '(rectangular)
       (lambda (z) (and (zero? (real-part z))
                        (zero? (imag-part z)))))
  
  'done)

;; 극좌표 package
(define (install-polar-package)
  
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  
  ;; utility
  (define (tag x) (attach-tag 'polar x))
  
  ;; interface
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; equ?
  (define (equ? x y)
    (and (eq? (magnitude x) (magnitude y))
         (eq? (angle x) (angle y))))
  (put 'equ? '(polar polar) equ?)
  
  ;; =zero?
  (put '=zero? '(polar)
       (lambda (z) (zero? (magnitude z))))

  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; 복소수 pakcage
(define (install-complex-package)
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (* (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (/ (angle z1) (angle z2))))
  
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; complex에 대해서 real-part, imag-part, magnitude, angle 가능하게 함.
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ;; equ?
  (put 'equ? '(complex complex)
       (lambda (x y) (apply-generic 'equ? x y)))

  ;; =zero?
  (put '=zero? '(complex)
       (lambda (z) (apply-generic '=zero? z)))
  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; polynomial package
(define (install-polynomial-package)
  
  ;; polynomial 구조
  ;; (variable ((order coeff) (order coeff) ... (order coeff)))
  
  (define (make-poly variable term-list)
    (cons variable term-list))
  
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? e) (symbol? e))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

;  (define (adjoin-term term term-list)
;    (if (=zero? (coeff term))
;        term-list
;        (cons term term-list)))
;  
;  (define (the-empty-termlist) '())
;  (define (empty-termlist? term) (null? term))
;  (define (first-term term-list) (car term-list))
;  (define (rest-terms term-list) (cdr term-list))

  ;; ex 2.89
  ;; 다른 코드(adjoin-term을 이용하는 프로시저들)는 그대로 두고 adjoin-term 프로시저만 변경
  ;; 아래 테스트 코드에서는 sparse term-list로 테스트하고 있어 수정함.
  (define (adjoin-term term term-list)
    (define (insert-n v n l) ;; l=(1 2 3 4) v=2, n=3 => (2 2 2 1 2 3 4)
      (if (= n 0)
          l
          (insert-n v (- n 1) (cons v l))))
    (define (replace-n v n l)
      (define (iter input idx output)
        (if (null? input)
            output
            (iter (cdr input) (- idx 1)
                  (append output (list (if (= idx n) v (car input)))))))
      (iter l (- (length l) 1) '()))
    (let ((max-order (- (length term-list) 1))
          (o (order term))
          (c (coeff term)))
      (if (> o max-order)
          (cons c (insert-n 0 (- o max-order 1) term-list))
          (replace-n c o term-list))))
  
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list))) ;; !!!
  (define (rest-terms term-list)
    (cdr term-list))

  
  ;; (put 'terms 'polynomial (lambda(x) (term-list x)))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1))
                 (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                   (else (adjoin-term
                          (make-term (order t1)
                                     (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms l1)
                                     (rest-terms l2)))))))))
  
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms l))))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Poly not in same var -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Poly not in same var -- MUL-POLY" (list p1 p2))))
  
  
  
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  ;; ex 2.87
  (define (=zero-poly? p)
    (define (=zero-terms? ts)
      (if (null? ts)
          #t
          (if (=zero? (coeff (first-term ts)))
              (=zero-terms? (rest-terms ts))
              #f)))
    (let ((ts (term-list p)))
      (if (empty-termlist? ts)
          #t
          (=zero-terms? ts))))
  (put '=zero? '(polynomial) =zero-poly?)
  
  ;; ex 2.88
  (define (sub-terms l1 l2)
    (let ((minus-l2 (mul-terms (adjoin-term (make-term 0 -1)
                                            (the-empty-termlist)) l2))) ;; ((0 -1))
      (add-terms l1 minus-l2)))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Poly not in same var -- SUB-POLY" (list p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

;; polynomial package test code

;; sparse term list
;; (define p1 (make-polynomial 'x '((2 1) (1 2) (0 1))))  ;; x^2 + 2x + 1
;; (define p2 (make-polynomial 'x '((2 1) (1 -2) (0 1)))) ;; x^2 - 2x + 1

;; dense term list 인 경우
(define p1 (make-polynomial 'x '(1 2 1)))  ;; x^2 + 2x + 1
(define p2 (make-polynomial 'x '(1 -2 1))) ;; x^2 - 2x + 1
(add p1 p2) ;; > 2x^2 + 2

;; ex 2.87 test code
(define zero-poly1 (make-polynomial 'x '()))
;(define zero-poly2 (make-polynomial 'x '((2 0) (0 0))))
(define zero-poly2 (make-polynomial 'x '(0 0 0))) ;; 0x^2 + 0x + 0
(=zero? zero-poly1)
(=zero? zero-poly2)
(=zero? p1)

;; ex 2.88 test code
;(mul (make-polynomial 'x '((0 -1))) p1) ;; -x^2 + -2x + -1
;(mul (make-polynomial 'x '((0 -1))) p2) ;; -x^2 + 2x + 1
;(add p1 p2) ;; 2x^2 + 2
;(=zero? (add p1 (mul (make-polynomial 'x '((0 -1))) p1))) ;; 0
;(sub p1 p2) ;; 4x
;(sub p2 p1) ;; -4x

;; ex 2.89
;; dense term package
;(define (install-dense-term-package)
;
;  (define (adjoin-term term term-list)
;    (define (insert-n v n l)
;      (if (= n 0)
;          l
;          (insert-n v (- n 1) (cons v l))))
;    (define (replace-n v n l)
;      (define (iter input idx output)
;        (if (null? input)
;            output
;            (iter (cdr input) (- idx 1)
;                  (append output (list (if (= idx n) v (car input)))))))
;      (iter l (- (length l) 1) '()))
;    (let ((max-order (- (length term-list) 1))
;          (o (order term))
;          (c (coeff term)))
;      (if (> o max-order)
;          (cons c (insert-n 0 (- o max-order 1) term-list))
;          (replace-n c o term-list))))
;  
;  (define (the-empty-termlist) '())
;  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list))) ;; !!!
;  (define (rest-terms term-list) (cdr term-list))
;  (define (empty-termlist? term-list) (null? term-list))
;  
;  ;; 이건 그대로 이용
;  (define (make-term order coeff) (list order coeff))
;  (define (order term) (car term))
;  (define (coeff term) (cadr term))
;  
;  
;  'done)

;;(define py (make-polynomial 'y '((2 1) (0 1))))
(define py (make-polynomial 'y '(1 0 1)))