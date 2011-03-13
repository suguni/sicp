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
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;; 데이터 중심 프로그래밍 : 2.4.2에서 cond로 tag를 확인해서 처리했는데 이 과정을 일반화된 연산을 이용하는 방식

(define (square x) (* x x))

;; contents에 tag 붙이기
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; 복소수를 직각 좌표로 표현하는 경우
(define (install-rectangular-package)
  ;; function
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
  ;; ...
  
  'done)

;; 복소수를 극 좌표로 표현하는 경우
(define (install-polar-package)
  ;; function
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
  ;; ...
  
  'done)

;; generic operator
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

;; 이런식으로 사용.
(define (real-part z) (apply-generic 'real-part z))

;; 2.4.2에서는 새로운 type이 추가될때마다 real-part, imag-part 등의 selector를 변경해야 했다.
;; 또 다른 타입에 대한 프로그램이 추가될때마다 이름이 겹치지 않는지 확인해서 수정이 필요했다.
;; 위와 같이 짜면 새로운 타입에 대한 프로그램을 짜는 사람만 고민하면 된다.

;; 근데 put와 get이 어떻게 동작하는지 잘 모르겠음. put의 type 인자가 리스트인데... 왜???


;; ex 2.73
(define (=number? exp num)
  (and (number? exp) (= exp num)))
      
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))
;;        (else ((get 'deriv (operator exp)) (operands exp) var))))

;; (+ x 2)
;; operator > +
;; operands > (x 2)

;; a. 위에서 한일은? 
;; number인 경우와 variable인 경우는 동일하게 처리하고, 나머지(식)인 경우는 식의 연산자에 대한 deriv 프로시저를 가져와 미분을 실행한다.
;; number와 variable의 경우는 내부 방식에 상관없이 동일하게 처리되므로 데이터 중심 방식으로 다룰 필요가 없다.

;; b. +, * 식을 미분하는 프로시저 짜기
(define (install-deriv-package)
  (define (make-sum a b)
    (cond ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))
  (define (augend e) (car e))               ;; a + b 에서 a
  (define (addend e) (cadr e))              ;; a + b 에서 b

  (define (deriv-sum exp var)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var)))
  
  (define (make-product a b)
    (cond ((or (=number? a 0) (=number? b 0)) 0)
          ((=number? a 1) b)
          ((=number? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b))))
  (define (multiplicand e) (car e))         ;; a * b 에서 a
  (define (multiplier e) (cadr e))          ;; a * b 에서 b

  (define (deriv-product exp var)
    (make-sum (make-product (multiplicand exp) (deriv (multiplier exp) var))
              (make-product (multiplier exp) (deriv (multiplicand exp) var))))

  ;; c. exponentiation 식 추가
  (define (make-exponentiation a b)
    (cond ((=number? b 0) 1)
          ((=number? a 0) 0)
          ((=number? b 1) a)
          ((=number? a 1) 1)
          ((and (number? a) (number? b)) (expt a b))
          (else (list '^ a b))))
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  
  (define (deriv-exponentiation exp var)
    (let ((u (base exp))
          (n (exponent exp)))
      (make-product (make-product n (make-exponentiation u (make-sum n -1)))
                    (deriv u var))))

  ;; interface
;  (put 'deriv '+ deriv-sum)
;  (put 'deriv '* deriv-product)
;  (put 'deriv '^ deriv-exponentiation)
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '^ 'deriv deriv-exponentiation)
  'done)

;; d. op / type 이 아닌 type / op 로 인덱싱 되어 있다면?
;; deriv 프로시저의 마지막 절이 ((get (operator exp) 'deriv) (operands exp) var) 가 된다.
;; put 프로시저에서 op와 type을 반대로 입력하면 된다.


;; ex 2.74

;; A 부서
(define (install-dep-A-package)
  
  (define (get-record employee file) '())
  (define (get-salary record) '())
  
  (define (make-file records) '())
  (define (add-record record) '())
  
  (define (tag x) (attach-tag 'depA x))

  ;; interface
  (put 'get-record 'depA get-record)
  (put 'get-salary 'depA get-salary)

  ;; A 부서 file 만들기
  (put 'make-file 'depA (lambda (records) (tag (make-file records))))
  
  ;; file에 record 추가
  (put 'add-record 'depA (lambda (record) (tag (add-rcord record))))

  'DONE)

;; B 부서
(define (install-dep-B-package)
  
  (define (get-record employee file) '())
  (define (get-salary record) '())
  
  (define (make-file records) '())
  (define (add-record record) '())
  
  (define (tag x) (attach-tag 'depB x))

  ;; interface
  (put 'get-record 'depB get-record)
  (put 'get-salary 'depB get-salary)

  ;; B 부서 file 만들기
  (put 'make-file 'depB (lambda (records) (tag (make-file records))))
  
  ;; file에 record 추가
  (put 'add-record 'depB (lambda (record) (tag (add-rcord record))))

  'DONE)

;; a. get-record 프로시저 짜기
;; 파일이 어떤 부서에 속하는지 알 수 있어야 하고, 이 정보는 파일의 태그로 저장된다.
;; 파일의 자료 구조 : '(부서명 레코드)
(define (get-record employee file)
  ((get 'get-record (type-tag file)) employee (contents file)))

;; b. get-salary 프로시저 짜기
;; 주어진 레코드가 어떤 부서에 속하는 것인지 알 수 있어야 한다. 파일과 마찬가지로 태그로 저장하면 된다.
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

;; c. find-employee-record 프로시저 짜기
(define (find-employee-reocrd employee file-list)
  (if (null? file-list)
      '()
      (let ((record (get-record employee (car file-list))))
        (if (null? record)
            (find-employee-record employee (cdr file-list))
            record))))

;; d. 새로운 부서를 처리할 패키지를 만들어야 한다.

;; 메시지 패싱 기법
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define (apply-generic op arg)
  (arg op))

;; ex 2.75
(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part)
           (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define c1 (make-from-mag-ang 10 (/ 3.141592 2)))

;; ex 2.76
;; * 방법
;;    1. dispatch on type
;;    2. data directed
;;    3. message passing
;; * 새로운 데이터 타입이 늘어나는 경우 
;;    1 방식에서는 모든 연산 프로시저들에서 새로운 데이터 타입을 처리할 수 있는 조건절을 추가해야 한다. 기존 코드를 많이 수정해야 한다.
;;    상대적으로 2의 경우 기존 코드에 변경 없이 새로운 프로시저를 정의해서 사용하면 되므로 프로그램이 잘못될 가능성과 범위가 줄어든다.
;; * 새로운 연산이 늘어나는 경우
;;    3 방식에서는 새로운 데이터 타입이 늘어날때 1 방식의 문제와 동일한 문제가 발생한다.
;;    즉, 각 연산을 기존 데이터 물체에 해당하는 프로시저에 추가해야 한다.
;;    상대적으로 1 방식은 기존 코드에 영향을 주는 부분이 많지 않다.
;; * data directed 방식은?
;;    새로운 데이터 타입이 늘어나거나 새로운 연산이 늘어나게 되면 수정이 필요하지만, 대신 기존 인터페이스와 구현은 수정할 부분이 없으므로
;;    3가지 방법 중에서 가장 코드의 변경으로 문제가 생길 가능성이 적다고 볼 수 있다.

