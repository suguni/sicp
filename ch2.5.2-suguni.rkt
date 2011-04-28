(define (square x) (* x x))

;; SICP support code
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (write "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; generic operator
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (write "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'scheme-number)
           (number? contents))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (if (number? datum)
          'scheme-number
          (write "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (if (number? datum)
          datum
          (write "Bad tagged datum -- CONTENTS" datum))))

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

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; get/put coercion
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;; coercion operators
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

;; generic operator
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (write "No method for these types"))))) ;; (list op type-tags)
              (write "No method for these types")))))) ;;  (list op type-tags)


(define PI 3.1415926)
(define n1 (make-scheme-number 10))
(define r1 (make-rational 1 5))
(define cr1 (make-complex-from-real-imag 3 4))
(define cm1 (make-complex-from-mag-ang 1 (/ PI 4)))

;; 아래 두 식은 동일
((get 'add '(complex complex)) ((get-coercion 'scheme-number 'complex) n1) cr1)
(apply-generic 'add n1 cr1)

;; ex 2.81
;; scheme-number->complex와 함께 아래도 정의 했다.
;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex n) n)
;; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;; (put-coercion 'complex 'complex complex->complex)

;; a. 아래와 같이 exp를 정의했는데 scheme-number package에만 exp 프로시저가 있고, complex package에는 없다.
(define (exp x y) (apply-generic 'exp x y))
;; exp를 complex 에 대해 수행하면??
;; ex) (exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 2 0))
;; [정답!] apply-generic이 무한 루프에 빠진다.
;;        apply-generic 처음에 (get op type-tags)로 operator를 가져오는데,
;;        complex에는 정의가 안되어 있으므로 type coercion 시도.
;;        문제는 complex->complex coercion 때문에 
;;        (apply-generic op (t1->t2 a1) a2)) 식이 결국은 원래 호출된것과 동일한 식이 되어 무한 루프에 빠짐.

;; b. 동일한 type에 대해 type coercion을 정의하는 것이 옳은가? 아닌가? -- 문제가 이게 아닌듯.
;;    같은 타입의 인자에 대해 무엇을 하는게 옳은가? apply-generic을 그대로 두는게 옳은가?
;; [정답!?] 연산이 가능한 package에 연산이 정의되지 않은 문제인거 아닌가? 
;;         동일한 타입에 대해서는 변환이 발생하지 않도록 하고, 연산 가능한 package에는 모두 연산을 정의하도록 해야 한다.

;; c. 동일한 타입인 경우 바꾸기가 발생하지 않게 apply-generic 변경
;;    위에서 동일한 타입에 대해 coercion을 정의하지 않았다면 원래 apply-generic도 잘 동작함.
;;    위의 type-coercion이 정의되었다고 하면 아래와 같이 수정 필요.
;; generic operator
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags) (cadr type-tags)))) ;; type check
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (write "No method for these types"))))) ;; (list op type-tags)
              (write "No method for these types")))))) ;; (list op type-tags)

;; ex 2.82
