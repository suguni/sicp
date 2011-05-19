(load "ch2.5.1.rkt")
;; 정수 package 재정의
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
  ;; for ex 2.81
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)
(install-scheme-number-package)

;; ch 2.5.2
(define (put-coercion a b c) (put a b c))
(define (get-coercion a b) (get a b))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

;; 타입 바꾸기 체계를 가진다.
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (let ((t1->t2 (get-coercion type1 type2))
;                      (t2->t1 (get-coercion type2 type1)))
;                  (cond ((equal? t1->t2 t2->t1) (write "같은 타입 바꾸기는 실행되지 않습니다.")) ;; for ex 2.81-c
;                        (t1->t2
;                         (apply-generic op (t1->t2 a1) a2))
;                        (t2->t1
;                         (apply-generic op a1 (t2->t1 a2)))
;                        (else
;                         (write "No method for these types"
;                                (list op type-tags))))))
;              (write "No method for these types"
;                     (list op type-tags)))))))

;; ex 2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;; a.
;; [예시] (exp (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 4 5))
;; [결과] 무한 반복에 빠진다.
;; [원인] apply-generic에서 t1->t2 와 t2->t1 이 동일한 타입 바꾸기를 가진다. 때문에 재귀에 재귀를 반복하여 호출한다.
;; b.
;; apply-generic은 정상적으로 동작하고 있다.
;; c.
;; 위의 코드에 기록 (임시방편)

;; ex 2.82 - 미완성

;scheme-number
;reational
;complex

(make-complex-from-real-imag (make-rational (make-scheme-number 10) (make-scheme-number 2)) (make-scheme-number 10))


;; ex 2.83
;; install-package에 각각 아래의 코드를 추가
(put 'raise '(scheme-number)
     (lambda (x)
       (make-rational x 1)))

;       (if (integer? x)
;           (make-rational x 1)
;           (make-complex-from-real-imag x 0))))

(put 'raise '(rational)
     (lambda (x)
       (make-complex-from-real-imag (/ (car x) (cdr x)) 0)))
;;       (make-scheme-number (/ (car x) (cdr x)))))  ; number,denom 대신 임시로 car,cdr 사용

(define (raise x) (apply-generic 'raise x))

;; ex 2.84
;; 탑구조에 대한 우위비교를 위해서 숫자화 시킨다. (예. 정수=1, 유리수=2, ... )
;; 잘 섞이는(compatible) 방식이란??
(define (tower-level x)
  (let ((typex (type-tag x)))
    (cond ((eq? typex 'rational) 1)
          ((eq? typex 'complex) 3)
          (else
           (let ((y (contents x)))
             (if (integer? y)
                 0
                 2))))))

(define (raise-to level x)
  (if (= level (tower-level x))
      x
      (raise-to level (raise x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                     (a2 (cadr args))
                     (level1 (tower-level a1))
                     (level2 (tower-level a2)))
                (cond ((< level1 level2)
                       (apply-generic op (raise-to level2 a1) a2))
                      ((< level2 level1)
                       (apply-generic op a1 (raise-to level1 a2)))
                      (else
                       (error "No method for these types"
                              (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; ex 2.85
;; 끌어내리는 연산인 project 연산 구현
;; 이를 일반적으로 사용할 수 있는 drop 프로시저도 구현
;; (apply-generic 수정 미완결)
(put 'project '(complex)  ; 복소수 -> 실수 (단, install-complax-package 내부에 추가)
     (lambda (x) (make-rational (cadr x) 1)))

(put 'project '(rational)  ; 복소수 -> 실수 (단, install-complax-package 내부에 추가)
     (lambda (x) (make-scheme-number (car x))))

(define (project x) (apply-generic 'project x))

(define (droppable? x)
  (if (eq? (type-tag x) 'scheme-number)
      #f
      (equ? x (raise (project x))))) ;; 삑!!! 수정요망

(define (drop x)
  (if (droppable? x)
      (drop (project x))
      x))

;; (drop (make-complex-from-real-imag (make-rational 10 3) 0))
;; 이게 안됨

;(define (apply-generic op . args)
;
;  (define (convert type ags)
;    (let ((type-tags (map type-tag ags)))
;      (if (null? ags)
;          '()
;          (cons ((get-coercion (car type-tags) t) (car ags))
;                (convert type (cdr ags))))))
;    
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (let ((type (common-coercion-type args)))
;            (if type ;; 변환이 불가능한 경우 또는 모두 동일한 type인 경우 #f
;                (apply apply-generic (cons op (convert type args)))
;                (write "cannot convert type")))))))
            

; 타입 바꾸기 체계를 가진다.
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (cond ((= (length args) 2)
;                 (let ((type1 (car type-tags))
;                       (type2 (cadr type-tags))
;                       (a1 (car args))
;                       (a2 (cadr args)))
;                   (let ((t1->t2 (get-coercion type1 type2))
;                         (t2->t1 (get-coercion type2 type1)))
;                     (cond (t1->t2
;                            (apply-generic op (t1->t2 a1) a2))
;                           (t2->t1
;                            (apply-generic op a1 (t2->t1 a2)))
;                           (else
;                            (write "No method for these types"))))))
;;;                                   (list op type-tags)))))))
;                ((> (length args) 2)
;                 (apply apply-generic (cons op
;                                            (cons (apply-generic op
;                                                                 (car args) 
;                                                                 (cadr args))
;                                                  (cddr args))))))))))
;
;
;(put-coercion 'scheme-number 'rational 
;              (lambda (n) (make-rational n 1)))
;(put-coercion 'scheme-number 'complex 
;              (lambda (n) (make-complex-from-real-imag n 0)))
;(put-coercion 'rational 'complex
;              (lambda (r) (make-complex-from-real-imag r 0)))
;
;(apply-generic 'add
;               (make-scheme-number 2)
;               (make-rational 1 2)
;               (make-complex-from-real-imag 2 2)) ;; 4.5 + 2i
;
;;; 삑!!!  errpr
;(apply-generic 'mul
;               (make-scheme-number 2)
;               (make-rational 1 2)
;               (make-complex-from-real-imag 2 2))

