;; 3.3.5 관계 알리기

;; p372 관계 시스템 돌려보기

;(define (celsius-fahrenheit-converter c f)
;  (let ((u (make-connector))
;        (v (make-connector))
;        (w (make-connector))
;        (x (make-connector))
;        (y (make-connector)))
;    (multiplier c w u)
;    (multiplier v x u)
;    (adder v y f)
;    (constant 9 w)
;    (constant 5 x)
;    (constant 32 y)
;    'ok))
;
;(define C (make-connector))
;(deifne F (make-connector))
;(celsius-fahrenheit-converter C F)
;
;(probe "Celsius temp" C)
;(probe "Fahrenheit temp" F)
;
;(set-value! C 25 'user) ;; 세번째 인자는 왜 필요할까?
;
;(set-value! F 77 'user) ;; error, C가 25가 되면서 F가 계산되었는데, 그 값과 맞지 않기 때문.
;
;(forget-value! C 'user)
;(set-value! F 77 'user)

;; 관계 중심 시스템: 계산이 한쪽으로만 흐르지 않는다.

;; p374 관계 시스템 만들기

;; 관계 상자 - adder
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; 관계 상자 - multiplier
(define (multiplier a1 a2 product)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! product
                       (* (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? product))
           (set-value! a2
                       (/ (get-value product) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? product))
           (set-value! a1
                       (/ (get-value product) (get-value a2))
                       me))))
  
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  
  (connect a1 me)
  (connect a2 me)
  (connect product me)
  me)


;; constant
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
  
;; probe
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  
  (define (process-new-value)
    (print-probe (get-value connector)))
  
  (define (process-forget-value)
    (print-probe "?"))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

;; 연결선 연산
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; p372 예제
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F) ;; 'ok

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user) ;; 세번째 인자는 왜 필요할까?
(set-value! F 77 'user) ;; ignored, C가 25가 되면서 F가 계산되었기 때문
(forget-value! C 'user) ;; ?, done
(set-value! F 77 'user)

;; ex 3.33
(define (average-box a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2.0 y)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(average-box A B C)
(probe "Input A" A)
(probe "Input B" B)
(probe "Input C" C)

;; test
(set-value! A 10 'user) ;; Probe: Input A = 10
(set-value! B 5 'user)  ;; Probe: Input B = 5
                        ;; Probe: Input C = 7.5
(forget-value! B 'user) ;; Probe: Input B = ?
                        ;; Probe: Input C = ?
(set-value! C 8 'user)  ;; Probe: Input C = 8
                        ;; Probe: Input B = 6.0

;; ex 3.34
;; ???

;; ex 3.35, squarer를 기본 관계로 정의하기
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                       (sqrt (get-value b))
                       me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a) (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  
  (connect a me)
  (connect b me)
  me)
  
;; test
(define X1 (make-connector))
(define X2 (make-connector))
(squarer X1 X2)
(probe "X1" X1)
(probe "X2" X2)
(set-value! X1 4 'user)  ;; Probe: Input X1 = 4
                         ;; Probe: Input X2 = 16
(forget-value! X1 'user) ;; Probe: Input X1 = ?
                         ;; Probe: Input X2 = ?
(set-value! X2 25 'user) ;; Probe: Input X2 = 25
                         ;; Probe: Input X1 = 5

;; ex 3.36
;; ???

;; ex 3.37
(define (celsius-fahrenheit-converter-exp x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;; test
(define C-exp (make-connector))
(define F-exp (celsius-fahrenheit-converter-exp C-exp))
(probe "C-exp" C-exp)
(probe "F-exp" F-exp)
(set-value! C-exp 25 'user) ;; Probe: C-exp = 25
                            ;; Probe: F-exp = 77
