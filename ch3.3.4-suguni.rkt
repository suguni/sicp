;; 3.3.4 A simulator for digital circuits

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; [TODO]
(define (get-signal wire)
  '())

;; [TODO]
(define (set-signal! wire value)
  '())

;; [TODO]
(define (add-action! wire proc)
  '())

;;; full-adder 사용은? 아래와 같이 하면 될까?
;(define in1 (make-wire))
;(set-signal! in1 1)
;(define in2 (make-wire))
;(set-signal! in2 1)
;(define sum (make-wire))
;(define c (make-wire))
;(full-adder in1 in2 sum c)
;;; 이후 sum, c 확인
;
;;; 아니면..
;(define in1 (make-wire))
;(define in2 (make-wire))
;(define sum (make-wire))
;(define c (make-wire))
;(full-adder in1 in2 sum c)
;(set-signal! in1 1)
;;; sum, c 확인
;(set-signal! in2 1)
;;; sum, c 확인
;(set-signal! in2 0)
;;; sum, c 확인
;;; 이후 sum, c 확인
;;; 이렇게 하면 이후 in1, in2 를 변경할때마다 full-adder 를 실행하지 않고도 sum, c 를 확인할 수 있다.
;;; 회로(full-adder)는 고정된 것이므로, 즉 full-adder 프로시저가 물체이므로 이런 논리가 더 적당하다.


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;; [TODO]
(define (after-delay delay proc)
  '())

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((not (or (valid-signal? a1) (valid-signal? a2))) (error "Invalid signal" a1 a2))
        ((and (= a1 1) (=a2 1)) 1)
        (else 0)))

(define (valid-signal? s)
  (or (= s 1) (= s 0)))

;; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay and-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((not (or (valid-signal? a1) (valid-signal? a2))) (error "Invalid signal" a1 a2))
        ((or (= a1 1) (=a2 1)) 1)
        (else 0)))

;; ex 3.29
(define (or-gate2 a1 a2 output)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (a1 (make-wire)))
    (inverter a1 i1)
    (inverter a2 i2)
    (and-gate i1 i2 a1)
    (inverter a1 output)))
;; delay = (+ (* 3 inverter-delay) and-gate-delay)

;; ex 3.30
(define (ripple-carry-adder a-wires b-wires s-wires c)
  
  (define (make-wires count)
    (define (iter wires n)
      (if (= 0 n)
          wires
          (iter (cons (make-wire) wires) (- n 1))))
    (iter '() count))

  (define (iter a b c-in s c-out)
    (if (null? a)
        'ok
        (begin
          (full-adder (car a) (car b) c-in (car s) (car c-out))
          (iter (cdr a) (cdr b) (car c-out) (cdr s) (cdr c-out)))))
  
  (let ((c-out-wires (make-wires (length a-wires))))
    (iter a-wires b-wires c s-wires c-out-wires)))

;; half-adder-delay <= (+ or-delay inverter-delay (* 2 and-delay))
;; full-adder-delay <= (+ (* 2 half-adder-delay) or-delay)
;; ripple-carry-adder-delay <= (* n full-adder-delay)
;;                           = (* n (+ (* 2 (+ or-delay inverter-delay (* 2 and-delay))) or-delay))
